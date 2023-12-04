drop materialized view mv.wallet;

create materialized view mv.wallet
as
  select
   w.*,
   (date_trunc('week', now() - interval '1 day')) :: date as startpoint,
   (now() - interval '1 day') :: date as endpoint
  from institution.wallet as w
with no data;

create unique index wallet_type_currency_institution_id_from_to_uq on mv.wallet (institution_id, currency, wallet_type, startpoint, endpoint);

refresh materialized view mv.wallet;

drop materialized view mv.invoice_and_transaction;

create materialized view mv.invoice_and_transaction
as 
  select
    now () as refresh_time,
    i.id as invoice_ident,
    i.created_at,
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
    i.fee,
    i.textual_view,
    i.appearance_on_timeline,
    t.id as transaction_ident,
    t.swift_sepa_message_id,
    t.sender_name,
    t.sender_address,
    t.sender_phone_number,
    t.sender_bank,
    t.swift_sepa_code,
    t.sender_bank_account,
    t.amount as transaction_amount,
    t.currency as transaction_currency,
    t.correspondent_bank,
    t.correspondent_bank_swift_sepa_code,
    iu.user_id as user_ident,
    (date_trunc('week', now() - interval '1 day')) :: date as startpoint,
    (now() - interval '1 day') :: date as endpoint
  from institution.invoice as i
  left join institution.transaction as t
  on i.id = t.invoice_id
  left join institution.user as iu
  on i.institution_id = iu.institution_id
with no data;

create unique index invoice_and_transaction_uq on mv.invoice_and_transaction (invoice_ident, transaction_ident);

refresh materialized view mv.invoice_and_transaction;