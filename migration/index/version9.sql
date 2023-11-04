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
        'institution_id', institution_id, 
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