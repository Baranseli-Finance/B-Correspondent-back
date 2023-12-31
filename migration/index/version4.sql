create schema if not exists institution;
create table institution.invoice (
    id bigserial primary key,
    created_at timestamp not null default now(),
    institution_id bigserial not null,
    customer_id text not null,
    invoice_id text not null,
    invoice_time timestamp not null,
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
    appearance_on_timeline timestamp null,
    constraint institution_transaction__institution_id__fk foreign key (institution_id) references auth.institution(id),
    constraint institution_transaction__invoice_id_institution_id_customer_id__unique unique (customer_id, invoice_id, institution_id));

create table institution.invoice_to_institution_delivery (
    invoice_id bigserial not null,
    institution_id bigserial not null,
    external_id uuid not null default uuid_generate_v4(),
    is_delivered bool not null default false,
    attempts int,
    last_attempt_sent_at timestamp,
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
    last_attempt_sent_at timestamp,
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
    modified_at timestamp null,
    payment_provider_ident text not null,
    constraint institution_account__institution_id__fk foreign key (institution_id) references auth.institution(id),
    constraint institution__wallet_institution_id_currency_wallet_type__unique unique(institution_id, currency, wallet_type));

create table institution.user (
  institution_id bigserial not null,
  user_id bigserial not null,
  constraint institution_user__institution_id__fk foreign key (institution_id) references auth.institution(id),
  constraint institution_user__user_id__fk foreign key (user_id) references auth.user(id),
  constraint institution_user__institution_id_user_id__unique unique (user_id, institution_id));

create or replace function notify_server_on_dashboard_transaction()
returns trigger as
$$
declare
  result jsonb;
begin
    select
        jsonb_build_object(
        'ident', new.id,
        'user', tbl.user,
        'dayOfYear', cast(extract('doy' from new.appearance_on_timeline) as int),
        'hour', cast(extract('hour' from new.appearance_on_timeline) as int),
        'min', cast(extract('min' from new.appearance_on_timeline) as int),
        'textualIdent', new.textual_view,
        'status', new.status,
        'tm', cast(new.appearance_on_timeline as text)) :: jsonb
    into result
    from (
      select
       u.id as user
      from auth.user as u
      inner join institution.user as iu
      on u.id = iu.user_id
      inner join institution.invoice as inv
      on iu.institution_id = inv.institution_id
      where (inv.status = 'ForwardedToPaymentProvider' or 
      inv.status = 'Confirmed' or 
      inv.status = 'Declined') and 
      new.id = inv.id) as tbl;
  perform 
    pg_notify(
      'dashboard_transaction' || '_' || u.id,
      coalesce(result :: text, 'null' :: text))
  from auth.user as u;
  return new;
end;
$$ language 'plpgsql';

create or replace trigger on_dashboard_transaction
after update on institution.invoice
for each row execute procedure notify_server_on_dashboard_transaction();

create or replace function notify_server_on_dashboard_wallet()
returns trigger as
$$
declare
  result jsonb;
begin
    select
        jsonb_build_object(
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
      'dashboard_wallet' || '_' || u.id,
      coalesce(result :: text, 'null' :: text))
  from auth.user as u;
  return new;
end;
$$ language 'plpgsql';

create or replace trigger on_dashboard_wallet_update
after update on institution.wallet
for each row execute procedure notify_server_on_dashboard_wallet();


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
    iu.user_id as user_ident,
    date_trunc('week', now() - interval '1 day') :: date as startpoint,
    (date_trunc('week', now() - interval '1 day') + '6 days'::interval) :: date as endpoint
  from institution.invoice as i
  left join institution.transaction as t
  on i.id = t.invoice_id
  left join institution.user as iu
  on i.institution_id = iu.institution_id
with no data;

create unique index invoice_and_transaction_uq on mv.invoice_and_transaction (invoice_ident, transaction_ident);

refresh materialized view mv.invoice_and_transaction;

create materialized view mv.wallet
as
  select
   w.*,
   date_trunc('week', now() - interval '1 day') :: date as startpoint,
   (date_trunc('week', now() - interval '1 day') + '6 days'::interval) :: date as endpoint
  from institution.wallet as w
with no data;

create unique index wallet_type_currency_institution_id_from_to_uq on mv.wallet (institution_id, currency, wallet_type, startpoint, endpoint);

refresh materialized view mv.wallet;

create table institution.withdrawal (
  id bigserial primary key,
  user_id bigserial not null,
  wallet_id bigserial not null,
  amount decimal(16, 2) not null,
  status text not null,
  created_at timestamp not null default now(),
  external_id uuid default uuid_generate_v4(),
  constraint institution_withdrawal__wallet_id__fk foreign key (wallet_id) references institution.wallet(id),
  constraint institution_withdrawal__user_id__fk foreign key (user_id) references auth.user(id),
  constraint institution__withdrawal__external_id_unique unique (external_id));

create or replace function notify_server_on_withdrawal()
returns trigger as
$$
declare
  result jsonb;
begin
    select
      jsonb_build_object(
        'user', tbl.user_ident,
        'institution', tbl.ident,
        'total', tbl.total,
        'items', tbl.history :: jsonb[]) 
      :: jsonb
    into result  
    from (
      select
        iu.user_id as user_ident,
        tmp.institution_id as ident,
        tmp2.total,
        array_agg(
          distinct
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
        where s.wallet_type = 'credit'
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
      inner join institution.user as iu
      on iu.institution_id = tmp.institution_id
      group by tmp.institution_id, tmp2.total, iu.user_id) as tbl;
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
      jsonb_build_object(
        'user', tbl.user_ident,
        'institution', tbl.ident,
        'total', tbl.total,
        'items', tbl.history :: jsonb[]) 
      :: jsonb
    into result  
    from (
      select
        iu.user_id as user_ident,
        tmp.institution_id as ident,
        tmp2.total,
        array_agg(
          distinct
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
        from institution.withdrawal as w
        inner join auth.user as u
        on w.user_id = u.id
        inner join institution.wallet as s
        on s.id = w.wallet_id
        where s.wallet_type = 'credit' 
        and new.id = w.id) as tmp
      inner join (
        select
          s.institution_id,
          count(*) as total
        from institution.withdrawal as f 
        inner join institution.wallet as s
        on s.id = f.wallet_id
        group by s.institution_id) as tmp2
      on tmp.institution_id = tmp2.institution_id
      inner join institution.user as iu
      on iu.institution_id = tmp.institution_id
      group by tmp.institution_id, tmp2.total, iu.user_id) as tbl;
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

create or replace function notify_server_on_balanced_book_new_transaction()
returns trigger as
$$
declare
  result jsonb;
begin
    select
      jsonb_build_object(
        'user_ident', iu.user_id,
        'institution_ident', iu.institution_id,
        'from', extract(hour from new.appearance_on_timeline),
        'to', extract(hour from new.appearance_on_timeline + interval '1 hour'),
        'amount', i.amount,
        'currency', i.currency,
        'dow', extract(dow from new.appearance_on_timeline),
        'institutionTitle', ai.title
      ) :: jsonb
    into result
    from institution.invoice as i
    inner join institution.user as iu
    on i.institution_id = iu.institution_id
    inner join auth.institution as ai
    on ai.id = i.institution_id
    where i.id = new.id 
    and new.status = 'ForwardedToPaymentProvider';
    perform
    pg_notify(
      'balanced_book_transaction_add' || '_' || u.id,
      coalesce(result :: text, 'null' :: text))
  from auth.user as u;
  return new;
end;
$$ language 'plpgsql';

create or replace trigger on_balanced_book_new_transaction
after update on institution.invoice
for each row 
execute procedure notify_server_on_balanced_book_new_transaction();

create table institution.relation (
  first_id bigserial not null check(first_id != second_id),
  second_id bigserial not null,
  constraint institution_relation__first_id__fk foreign key (first_id) references auth.institution(id),
  constraint institution_relation__second_id__fk foreign key (second_id) references auth.institution(id),
  constraint institution__relation___first_second_unique unique (first_id, second_id));

create or replace function notify_server_on_balanced_book_wallet_update()
returns trigger as
$$
declare
  result jsonb;
begin
  with info as (
    select 
      institution_id as ident,
      currency
    from institution.wallet as w
    inner join institution.withdrawal as wt
    on w.id = wt.wallet_id
    where external_id = new.external_id)
  select 
    jsonb_build_object('wallets', f.wallets) || s.users
  into result
  from(
    select 
      array_agg(jsonb_build_object(
        'ident', tbl.wallet_ident, 
        'institution', tbl.institution_ident,
        'amount', tbl.amount))
      as wallets
    from(
      select
        s.id as wallet_ident,
        s.institution_id as institution_ident,
        s.amount
        from institution.withdrawal as f
        inner join institution.wallet as s
        on f.wallet_id = s.id
        where f.external_id = new.external_id
        union 
        select
            w.id as wallet_ident,
            f.ident as institution_ident,
            w.amount
      from (
        select   
            coalesce(rf.second_id, rs.first_id) as ident
        from auth.institution as i
        left join institution.relation rf
        on i.id = rf.first_id 
        and rf.first_id = (select ident from info)
        left join institution.relation rs
        on i.id = rs.second_id 
        and rs.second_id = (select ident from info)
        where rf.second_id is not null or rs.first_id is not null) as f
        inner join institution.wallet as w
      on w.institution_id = f.ident
      where w.wallet_type = 'debit'
      and w.currency = (select currency from info)) as tbl) as f
    left join (
     select 
       jsonb_build_object('users', tbl.users) as users
     from (
      select
        array_agg(user_id) as users
      from institution.user as f
      inner join (select
            unnest(array[(select ident from info), coalesce(rf.second_id, rs.first_id)])
        from auth.institution as i
        left join institution.relation rf
        on i.id = rf.first_id 
        and rf.first_id = (select ident from info)
        left join institution.relation rs
        on i.id = rs.second_id 
        and rs.second_id = (select ident from info)
        where rf.second_id is not null or rs.first_id is not null) as s(ident)
      on  f.institution_id = s.ident) as tbl) as s on true;
  perform 
    pg_notify(
      'balanced_book_wallet_update' || '_' || u.id,
      coalesce(result :: text, 'null' :: text))
  from auth.user as u;
  return new;
end;
$$ language 'plpgsql';


create or replace trigger on_balanced_book_wallet_update
after update on institution.withdrawal
for each row 
execute procedure notify_server_on_balanced_book_wallet_update();