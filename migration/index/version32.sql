alter table auth.institution add column abbreviation text;
drop table institution.transaction cascade;

create table institution.transaction (
    id uuid primary key default uuid_generate_v4(),
    invoice_id bigserial not null,
    sender text not null,
    city text not null,
    country text not null,
    sender_bank text not null,
    sender_wire_transfer_agent text not null,
    sender_wire_transfer_agent_code text not null,
    sender_bank_operation_code text not null,
    receiver_bank text not null,
    receiver_wire_transfer_agent text not null,
    amount decimal(16, 2) not null,
    currency text not null,
    correspondent_bank text null,
    correspondent_wire_bank_transfer_agent text null,
    charges text not null,
    created_at timestamp not null,
    description text not null,
    constraint institution_transaction__invoice_id__fk foreign key (invoice_id) references institution.invoice(id),
    constraint institution_transaction_invoice_id__unique unique (invoice_id));

create materialized view mv.invoice_and_transaction
as 
  select
    now () as refresh_time,
    i.id as invoice_ident,
    i.created_at as invoice_timestamp,
    i.institution_id,
    i.customer_id,
    i.invoice_id,
    i.invoice_time,
    i.seller,
    i.seller_address,
    i.seller_tax_id,
    i.seller_phone_number,
    i.buyer,
    i.buyer_address,
    i.buyer_tax_id,
    i.buyer_phone_number,
    i.payment_description,
    i.currency as invoice_currency,
    i.amount as invoice_amount,
    i.vat,
    i.status,
    i.fee as invoice_fee,
    i.textual_view,
    i.appearance_on_timeline,
    t.id as transaction_ident,
    t.sender as transaction_sender,
    t.city as transaction_city,
    t.country as transaction_country,
    t.sender_bank,
    t.sender_wire_transfer_agent,
    t.sender_wire_transfer_agent_code,
    t.sender_bank_operation_code,
    t.receiver_bank,
    t.receiver_wire_transfer_agent,
    t.amount as transaction_amount,
    t.currency as transaction_currency,
    t.correspondent_bank,
    t.correspondent_wire_bank_transfer_agent,
    t.charges as transaction_fee,
    t.created_at as transaction_timestamp,
    t.description as transaction_payment_description,
    iu.user_id as user_ident,
    date_trunc('week', now() - interval '7 day') :: date as startpoint,
    (date_trunc('week', now() - interval '1 day')) :: date as endpoint
  from institution.invoice as i
  left join institution.transaction as t
  on i.id = t.invoice_id
  left join institution.user as iu
  on i.institution_id = iu.institution_id
with no data;

create unique index invoice_and_transaction_uq on mv.invoice_and_transaction (invoice_ident, transaction_ident, user_ident);

refresh materialized view mv.invoice_and_transaction;