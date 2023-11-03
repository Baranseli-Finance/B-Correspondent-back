alter table auth.code alter column expire_at drop default;
alter table auth.code alter column expire_at set default now() + interval '30 min';