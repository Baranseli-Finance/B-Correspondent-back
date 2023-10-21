create schema if not exists institution;
create table institution.invoice (
    id bigserial primary key,
    created_at timestamptz not null default now(),
    institution_id bigserial not null,
    customer_id text not null,
    invoice_id text not null,
    invoice_time timestamptz not null,
    seller text not null,
    seller_address text not null,
    seller_tax_id text,
    seller_phone_number text,
    buyer text not null,
    buyer_address text not null,
    buyer_tax_id text,
    buyer_phone_number text,
    payment_description text not null,
    currency text not null,
    amount decimal(16, 2) not null,
    vat decimal(16, 2) not null,
    status text not null,
    fee text not null,
    textual_view text not null,
    appearance_on_timeline timestamptz null,
    constraint institution_transaction__institution_id__fk foreign key (institution_id) references auth.institution(id),
    constraint institution_transaction__invoice_id_institution_id_customer_id__unique unique (customer_id, invoice_id, institution_id));

create table institution.invoice_to_institution_delivery (
    invoice_id bigserial not null,
    institution_id bigserial not null,
    external_id uuid not null default uuid_generate_v4(),
    is_delivered bool not null default false,
    attempts int,
    last_attempt_sent_at timestamptz,
    error text,  
    constraint institution__invoice_to_institution_delivery__invoice_id__fk foreign key (invoice_id) references institution.invoice(id),
    constraint institution__invoice_id__fk foreign key (institution_id) references auth.institution(id),
    constraint institution__invoice_institution_id__unique unique (invoice_id, institution_id));

create table institution.transaction (
    id uuid primary key default uuid_generate_v4(),
    invoice_id bigserial not null,
    swift_sepa_message_id bigserial not null,
    sender_name text not null,
    sender_address text not null,
    sender_phone_number text not null,
    sender_bank text not null,
    swift_sepa_code text not null,
    sender_bank_account text not null,
    amount decimal(16, 2) not null,
    currency text not null,
    correspondent_bank text null,
    correspondent_bank_swift_sepa_code text null,
    constraint institution_transaction__invoice_id__fk foreign key (invoice_id) references institution.invoice(id),
    constraint institution_transaction__swift_sepa_message_id__fk foreign key (swift_sepa_message_id) references storage.file(id),
    constraint institution_transaction_invoice_id__unique unique (invoice_id));

create table institution.transaction_to_institution_delivery (
    transaction_id uuid not null,
    institution_id bigserial not null,
    is_delivered bool not null default false,
    attempts int,
    last_attempt_sent_at timestamptz,
    error text,
    constraint transaction__transaction_to_institution_delivery__transaction_id__fk foreign key (transaction_id) references institution.transaction(id),
    constraint transaction__transaction_id__fk foreign key (institution_id) references auth.institution(id),
    constraint institution__transaction_institution_id__unique unique (transaction_id, institution_id));

create table institution.invoice_external_ident (
     invoice_id bigserial not null,
     external_id uuid not null default uuid_generate_v4(),
     constraint institution__invoice_external_ident__invoice_id__fk foreign key (invoice_id) references institution.invoice(id));

create table institution.wallet (
    id bigserial primary key,
    institution_id bigserial not null,
    currency text not null,
    amount decimal(24, 2) not null default 0,
    wallet_type text not null,
    modified_at timestamptz null,
    payment_provider_ident text not null,
    constraint institution_account__institution_id__fk foreign key (institution_id) references auth.institution(id),
    constraint institution__wallet_institution_id_currency_wallet_type__unique unique(institution_id, currency, wallet_type));

create table institution.user (
  institution_id bigserial not null,
  user_id bigserial not null,
  constraint institution_user__institution_id__fk foreign key (institution_id) references auth.institution(id),
  constraint institution_user__user_id__fk foreign key (user_id) references auth.user(id),
  constraint institution_user__institution_id_user_id__unique unique (user_id, institution_id));

create or replace function notify_server_on_transaction()
returns trigger as
$$
declare
  result jsonb;
begin
    select
        json_build_object(
        'ident', tmp.id,
        'user', tmp.user,
        'dayOfYear', cast(extract('doy' from new.appearance_on_timeline) as int),
        'hour', cast(extract('hour' from new.appearance_on_timeline) as int),
        'min', cast(extract('min' from new.appearance_on_timeline) as int),
        'textualIdent', tmp.textual_view,
        'status', new.status,
        'tm', cast(new.appearance_on_timeline as text)) :: jsonb
    into result
    from (
      select
       inv.*,
       u.id as user
      from auth.user as u
      inner join institution.user as iu
      on u.id = iu.user_id
      inner join institution.invoice as inv
      on iu.institution_id = inv.institution_id
      where (new.status = 'ForwardedToPaymentProvider' or 
      new.status = 'Confirmed' or 
      new.status = 'Declined')
      and new.id = inv.id) as tmp;
  perform 
    pg_notify(
      'timeline_transaction' || '_' || u.id,
      coalesce(result :: text, 'null' :: text))
  from auth.user as u;
  return new;
end;
$$ language 'plpgsql';

create or replace trigger on_invoice_update
after update on institution.invoice
for each row execute procedure notify_server_on_transaction();

create or replace function notify_server_on_wallet()
returns trigger as
$$
declare
  result jsonb;
begin
    select
        json_build_object(
         'user', tmp.user,
         'ident', tmp.id,
         'amount', tmp.amount)
        :: jsonb
    into result
    from (
      select
        inw.*,
        u.id as user
      from auth.user as u
      inner join institution.user as iu
      on u.id = iu.user_id
      inner join institution.wallet as inw
      on iu.institution_id = inw.institution_id
      where new.id = inw.id)
      as tmp;
  perform 
    pg_notify(
      'wallet' || '_' || u.id,
      coalesce(result :: text, 'null' :: text))
  from auth.user as u;
  return new;
end;
$$ language 'plpgsql';

create or replace trigger on_wallet_update
after update on institution.wallet
for each row execute procedure notify_server_on_wallet();


create schema if not exists mv;

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
    iu.user_id as user_ident
  from institution.invoice as i
  left join institution.transaction as t
  on i.id = t.invoice_id
  left join institution.user as iu
  on i.institution_id = iu.institution_id
with no data;

create unique index invoice_and_transaction_uq on mv.invoice_and_transaction (invoice_ident, transaction_ident);

refresh materialized view mv.invoice_and_transaction;

create table institution.withdrawal (
  id bigserial primary key,
  user_id bigserial not null,
  wallet_id bigserial not null,
  amount decimal(16, 2) not null,
  status text not null,
  created_at timestamp not null default now(),
  constraint institution_withdrawal__wallet_id__fk foreign key (wallet_id) references institution.wallet(id),
  constraint institution_withdrawal__user_id__fk foreign key (user_id) references auth.user(id));

create or replace function notify_server_on_withdrawal()
returns trigger as
$$
declare
  result jsonb;
begin
    select
      json_build_object(
        'institution', tbl.ident,
        'total', tbl.total,
        'items', tbl.history :: jsonb[]) 
      :: jsonb
    into result  
    from (
      select
        tmp.institution_id as ident,
        tmp2.total,
        array_agg(
          jsonb_build_object(
           'initiator', tmp.initiator,
           'ident', tmp.id,
           'currency', tmp.currency,
           'amount', tmp.amount,
           'withdrawalStatus', tmp.status,
           'created', cast(tmp.created_at as text) || 'Z')) 
           as history   
      from (
        select 
          s.institution_id,
          u.login || '<' || u.email || '>' as initiator,
          f.id,
          s.currency,
          f.amount,
          f.status,
          f.created_at
        from institution.withdrawal as f 
        inner join institution.wallet as s
        on s.id = f.wallet_id
        inner join auth.user as u
        on f.user_id = u.id
        where s.wallet_type = 'debit'
        and new.id = f.id) as tmp
      inner join (
        select
          s.institution_id,
          count(*) as total
        from institution.withdrawal as f 
        inner join institution.wallet as s
        on s.id = f.wallet_id
        group by s.institution_id) as tmp2
      on tmp.institution_id = tmp2.institution_id 
      group by tmp.institution_id, tmp2.total) as tbl;
  perform 
    pg_notify(
      'withdrawal' || '_' || u.id,
      coalesce(result :: text, 'null' :: text))
  from auth.user as u;
  return new;
end;
$$ language 'plpgsql';

create or replace trigger on_withdrawal_update
after update on institution.withdrawal
for each row when (old is distinct from new) 
execute procedure notify_server_on_withdrawal();


create or replace function notify_server_on_withdrawal_new()
returns trigger as
$$
declare
  result jsonb;
begin
    select
      json_build_object(
        'institution', tbl.ident,
        'total', tbl.total,
        'items', tbl.history :: jsonb[]) 
      :: jsonb
    into result  
    from (
      select
        tmp.institution_id as ident,
        tmp2.total,
        array_agg(
          jsonb_build_object(
           'initiator', tmp.initiator,
           'ident', tmp.id,
           'currency', tmp.currency,
           'amount', tmp.amount,
           'withdrawalStatus', tmp.status,
           'created', cast(tmp.created_at as text) || 'Z')) 
           as history   
      from (
        select 
          s.institution_id,
          u.login || '<' || u.email || '>' as initiator,
          new.id,
          s.currency,
          new.amount,
          new.status,
          new.created_at
        from institution.wallet as s
        inner join auth.user as u
        on new.wallet_id = s.id
        where s.wallet_type = 'debit') as tmp
      inner join (
        select
          s.institution_id,
          count(*) as total
        from institution.withdrawal as f 
        inner join institution.wallet as s
        on s.id = f.wallet_id
        group by s.institution_id) as tmp2
      on tmp.institution_id = tmp2.institution_id 
      group by tmp.institution_id, tmp2.total) as tbl;
  perform 
    pg_notify(
      'withdrawal' || '_' || u.id,
      coalesce(result :: text, 'null' :: text))
  from auth.user as u;
  return new;
end;
$$ language 'plpgsql';

create or replace trigger on_withdrawal_new
after insert on institution.withdrawal
for each row 
execute procedure notify_server_on_withdrawal_new();