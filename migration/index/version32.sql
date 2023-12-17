alter table auth.institution add column abbreviation text;
drop table institution.transaction cascade;

create table institution.transaction (
    id uuid primary key default uuid_generate_v4(),
    invoice_id bigserial not null,
    sender text not null,
    city text not null,
    country text not null,
    sender_bank text not null,
    sender_transfer_agent text not null,
    sender_transfer_agent_code text not null,
    sender_bank_operation_code text not null,
    receiver_bank text not null,
    receiver_transfer_agent text not null,
    amount decimal(16, 2) not null,
    currency text not null,
    correspondent_bank text null,
    correspondent_bank_transfer_agent text null,
    charges text not null,
    created_at timestamp not null,
    description text not null,
    constraint institution_transaction__invoice_id__fk foreign key (invoice_id) references institution.invoice(id),
    constraint institution_transaction_invoice_id__unique unique (invoice_id));