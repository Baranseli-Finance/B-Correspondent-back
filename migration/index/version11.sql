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
        'dow', extract(isodow from new.appearance_on_timeline),
        'institutionTitle', ai.title
      ) :: jsonb
    into result
    from institution.invoice as i
    inner join institution.user as iu
    on i.institution_id = iu.institution_id
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