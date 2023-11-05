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
        'tm', cast(new.appearance_on_timeline as text),
        'currency', new.currency,
        'amount', new.amount) :: jsonb
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