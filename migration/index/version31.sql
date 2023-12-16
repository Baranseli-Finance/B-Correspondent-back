create table delivery (
  table_ref text not null,
  table_ident bigint not null,
  is_delivered bool not null default false,
  attempts int,
  last_attempt_sent_at timestamp,
  error text,
  constraint delivery__unique unique (table_ref, table_ident));

create unique index delivery_idx on delivery (table_ref, table_ident);