drop trigger on_notification on public.notification;
drop function notify_server_on_notification;

create table public.notification_counter ( 
  institution_id bigserial not null,
  amount int not null,
  constraint notification_counter__institution_id__fk foreign key (institution_id) references auth.institution(id));

create or replace function notify_server_on_notification_counter()
returns trigger as
$$
declare
  result jsonb;
begin
    select
      jsonb_build_object(
        'institutionId', institution_id, 
        'count', amount)
    into result
    from public.notification_counter
    where institution_id = new.institution_id;
  perform
    pg_notify(
      'notification' || '_' || u.id,
      coalesce(result :: text, 'null' :: text))
  from auth.user as u;
  return new;
end;
$$ language 'plpgsql';

create or replace trigger on_notification_counter
after insert on public.notification_counter
for each row execute procedure notify_server_on_notification_counter();

create or replace function notify_server_on_dashboard_transaction()
returns trigger as
$$
declare
  result jsonb;
begin
    select
      jsonb_build_object(
        'ident', new.id,
        'institutionId', i.institution_id,
        'dayOfYear', cast(extract('doy' from new.appearance_on_timeline) as int),
        'hour', cast(extract('hour' from new.appearance_on_timeline) as int),
        'min', cast(extract('min' from new.appearance_on_timeline) as int),
        'textualIdent', new.textual_view,
        'status', new.status,
        'tm', cast(new.appearance_on_timeline as text)) :: jsonb
    into result
    from institution.invoice as i
    where (i.status = 'ForwardedToPaymentProvider' or 
    i.status = 'Confirmed' or 
    i.status = 'Declined') and 
    new.id = i.id;
  perform 
    pg_notify(
      'dashboard_transaction' || '_' || u.id,
      coalesce(result :: text, 'null' :: text))
  from auth.user as u;
  return new;
end;
$$ language 'plpgsql';

create or replace function notify_server_on_dashboard_wallet()
returns trigger as
$$
declare
  result jsonb;
begin
    select
      jsonb_build_object(
         'institutionId', institution_id,
         'ident', id,
         'amount', amount)
        :: jsonb
    into result
    from institution.wallet as w
    where new.id = w.id;
  perform 
    pg_notify(
      'dashboard_wallet' || '_' || u.id,
      coalesce(result :: text, 'null' :: text))
  from auth.user as u;
  return new;
end;
$$ language 'plpgsql';

create or replace function notify_server_on_balanced_book_new_transaction()
returns trigger as
$$
declare
  result jsonb;
begin
    select
      jsonb_build_object(
        'institutionId', iu.institution_id,
        'from', extract(hour from new.appearance_on_timeline),
        'to', extract(hour from new.appearance_on_timeline + interval '1 hour'),
        'amount', i.amount,
        'currency', i.currency,
        'dow', extract(dow from new.appearance_on_timeline),
        'institutionTitle', ai.title
      ) :: jsonb
    into result
    from institution.invoice as i
    inner join auth.institution as ai
    on ai.id = i.institution_id
    where i.id = new.id 
    and new.status = 'ForwardedToPaymentProvider';
    perform
    pg_notify(
      'balanced_book_transaction_add' || '_' || u.id,
      coalesce(result :: text, 'null' :: text))
  from auth.user as u;
  return new;
end;
$$ language 'plpgsql';