create table delivery (
  table_ref text not null,
  table_ident bigint not null,
  is_delivered bool not null default false,
  attempts int,
  last_attempt_sent_at timestamp,
  error text,
  is_stuck bool not null default false,
  constraint delivery__unique unique (table_ref, table_ident));

create unique index delivery_idx on delivery (table_ref, table_ident);

alter table institution.invoice add column external_id uuid not null default uuid_generate_v4();

insert into delivery (table_ref, table_ident, is_delivered)
select 'invoice', id, true from institution.invoice;