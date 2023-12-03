alter table cache add column is_permanent bool not null default false;

create or replace procedure sweep_cache (retention_period interval)
as $$
begin
    delete from cache
    where inserted_at < now() - retention_period 
    and not is_permanent;

commit;
end;
$$ language plpgsql;

select cron.unschedule(1);

select cron.schedule('cache_sweeper', '*/5 * * * *', $$call sweep_cache('1 hour');$$);