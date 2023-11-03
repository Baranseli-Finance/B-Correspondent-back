create or replace function notify_server_on_notification()
returns trigger as
$$
declare
  result jsonb;
begin
    select 
      jsonb_build_object('users', tmp.xs)
    into result  
    from(
      select array_agg(to_jsonb(user_id)) as xs
      from public.notification
      where id = new.id) as tmp;
  perform 
    pg_notify(
      'notification' || '_' || u.id,
      coalesce(result :: text, 'null' :: text))
  from auth.user as u;
  return new;
end;
$$ language 'plpgsql';

create or replace trigger on_notification
after insert on public.notification
for each row execute procedure notify_server_on_notification();