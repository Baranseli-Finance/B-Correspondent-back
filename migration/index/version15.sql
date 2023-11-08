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
    where id = new.id;
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