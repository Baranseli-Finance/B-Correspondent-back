alter table institution.invoice rename textual_view to transaction_textual_ident;
alter materialized view mv.invoice_and_transaction rename textual_view to transaction_textual_ident;