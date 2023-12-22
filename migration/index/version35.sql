alter table auth.institution add constraint abbreviation_check check (char_length(abbreviation) = 3);
alter table institution.transaction drop column sender_wire_transfer_agent cascade;
alter table institution.transaction drop column created_at;
alter table institution.transaction add column ok_transaction_date date;
alter table institution.transaction add column ok_transaction_time time;
alter table institution.transaction rename column sender to ok_sender;
alter table institution.transaction rename column city to ok_city;
alter table institution.transaction rename column country to ok_country;
alter table institution.transaction rename column sender_bank to ok_sender_bank;
alter table institution.transaction rename column sender_wire_transfer_agent_code to ok_sender_wire_transfer_agent_code;
alter table institution.transaction rename column sender_bank_operation_code to ok_sender_bank_operation_code;
alter table institution.transaction rename column receiver_bank to ok_receiver_bank;
alter table institution.transaction rename column receiver_wire_transfer_agent_code to ok_receiver_wire_transfer_agent_code;
alter table institution.transaction rename column correspondent_bank to ok_correspondent_bank;
alter table institution.transaction rename column correspondent_bank_wire_transfer_agent_code to ok_correspondent_bank_wire_transfer_agent_code;
alter table institution.transaction rename column description to ok_description;
alter table institution.transaction rename column amount to ok_amount;
alter table institution.transaction rename column currency to ok_currency;
alter table institution.transaction rename column charges to ok_fee;

alter table institution.transaction alter column ok_sender drop not null;
alter table institution.transaction alter column ok_city drop not null;
alter table institution.transaction alter column ok_country drop not null;
alter table institution.transaction alter column ok_sender_bank drop not null;
alter table institution.transaction alter column ok_sender_wire_transfer_agent_code drop not null;
alter table institution.transaction alter column ok_sender_bank_operation_code drop not null;
alter table institution.transaction alter column ok_receiver_bank drop not null;
alter table institution.transaction alter column ok_receiver_wire_transfer_agent_code drop not null;
alter table institution.transaction alter column ok_correspondent_bank drop not null;
alter table institution.transaction alter column ok_correspondent_bank_wire_transfer_agent_code drop not null;
alter table institution.transaction alter column ok_description drop not null;
alter table institution.transaction alter column ok_amount drop not null;
alter table institution.transaction alter column ok_currency drop not null;
alter table institution.transaction alter column ok_fee drop not null;

alter table institution.transaction add column failure_reason text;
alter table institution.transaction add column failure_timestamp timestamp;

alter table institution.transaction add column picked_for_delivery bool default false;


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
    i.transaction_textual_ident,
    i.appearance_on_timeline,
    t.id as transaction_ident,
    t.ok_sender as ok_transaction_sender,
    t.ok_city as ok_transaction_city,
    t.ok_country as ok_transaction_country,
    t.ok_sender_bank,
    t.ok_sender_wire_transfer_agent_code,
    t.ok_sender_bank_operation_code,
    t.ok_receiver_bank,
    t.ok_receiver_wire_transfer_agent_code,
    t.ok_correspondent_bank,
    t.ok_correspondent_bank_wire_transfer_agent_code,
    t.ok_description as ok_transaction_payment_description,
    t.ok_transaction_date,
    t.ok_transaction_time,
    t.ok_amount as ok_transaction_amount,
    t.ok_currency as ok_transaction_currency,
    t.ok_fee as ok_transaction_fee,
    t.failure_reason,
    t.failure_timestamp,
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

alter table webhook add column error text;