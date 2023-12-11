drop materialized view mv.wallet;
drop index wallet_type_currency_institution_id_from_to_uq;

create materialized view mv.wallet
as
  select
   w.id,
   w.institution_id,
   w.currency,
   w.amount,
   w.wallet_type,
   date_trunc('week', now() - interval '7 day') :: date as startpoint,
   (date_trunc('week', now() - interval '1 day')) :: date as endpoint
  from institution.wallet as w
with no data;

create unique index mv_wallet_uq on mv.wallet (id, institution_id, currency, amount, wallet_type, startpoint, endpoint);

refresh materialized view mv.wallet;