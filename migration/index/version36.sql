drop materialized view mv.wallet;

create table institution.wallet_history (
    id bigserial primary key,
    institution_id bigserial not null,
    currency text not null,
    amount decimal(24, 2) not null default 0,
    wallet_type text not null,
    startpoint date not null,
    endpoint date not null,
    constraint institution_wallet_history__institution_id__fk foreign key (institution_id) references auth.institution(id),
    constraint institution__wallet_history__unique unique(institution_id, currency, wallet_type, startpoint, endpoint));

insert into institution.wallet_history
(institution_id,
    currency,
    amount,
    wallet_type,
    startpoint,
    endpoint)
select
    institution_id,
    currency,
    amount,
    wallet_type,
    (select (date_trunc('week', now() - interval '1 day'))),
    ((now() - interval '1 day'))
from institution.wallet
on conflict (institution_id, currency, wallet_type, startpoint, endpoint) do nothing;

drop materialized view mv.invoice_and_transaction;
drop schema mv;

create materialized view institution.invoice_and_transaction
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
    iu.user_id as user_ident
  from institution.invoice as i
  left join institution.transaction as t
  on i.id = t.invoice_id
  left join institution.user as iu
  on i.institution_id = iu.institution_id
with no data;

create unique index invoice_and_transaction_uq on institution.invoice_and_transaction (invoice_ident, user_ident);

refresh materialized view institution.invoice_and_transaction;