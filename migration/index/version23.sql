create unlogged table cache (
    id serial primary key,
    key text unique not null,
    value jsonb,
    inserted_at timestamp);

create index idx_cache_key on cache (key);

create or replace procedure expire_rows (retention_period integer)
as $$
begin
    delete from cache
    where inserted_at < now() - retention_period;
    commit;
end;
$$ language plpgsql;

select cron.schedule('0 * * * *', $$call expire_rows('1 hour');$$);