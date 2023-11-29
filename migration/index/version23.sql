create unlogged table cache (
    id serial primary key,
    key text unique not null,
    value jsonb not null,
    inserted_at timestamp not null default now());

create index idx_cache_key on cache (key);

create or replace procedure sweep_cache (retention_period interval)
as $$
begin
    delete from cache
    where inserted_at < now() - retention_period;

commit;
end;
$$ language plpgsql;

select cron.schedule('*/5 * * * *', $$call sweep_cache('1 hour');$$);