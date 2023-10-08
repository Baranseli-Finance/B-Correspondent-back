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
    constraint institution_transaction__swift_sepa_message_id__fk foreign key (swift_sepa_message_id) references storage.file(id));

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

create table institution.account (
    institution_id bigserial not null,
    currency text not null,
    amount decimal(24, 2) not null,
    constraint institution_account__institution_id__fk foreign key (institution_id) references auth.institution(id));

create table institution.user (
  institution_id bigserial not null,
  user_id bigserial not null,
  constraint institution_user__institution_id__fk foreign key (institution_id) references auth.institution(id),
  constraint institution_user__user_id__fk foreign key (user_id) references auth.user(id),
  constraint institution_user__institution_id_user_id__unique unique (user_id, institution_id));

 timelineItemDayOfYear :: Int,
       timelineItemHour :: Int,
       timelineItemMin :: Int,
       timelineItemIdent :: Text

create or replace function notify_server_on_invoice()
returns trigger as
$$
declare
  result jsonb;
begin
    select
        json_build_object(
        'day_of_year', coalesce(cast(extract('doy' from tmp.appearance_on_timeline) as int), 0),
        'hour', coalesce(cast(extract('hour' from tmp.appearance_on_timeline) as int), 0),
        'min', coalesce(cast(extract('min' from tmp.appearance_on_timeline) as int), 0),
        'ident', tmp.textual_view,
        'status', tmp.status)
        :: jsonb
    into result
    from (
      select
       inv.*
      from auth.user as u
      inner join institution.user as iu
      on u.id = iu.user_id
      inner join institution.invoice as inv
      on iu.institution_id = inv.institution_id
      where inv.status = 'ForwardedToPaymentProvider' or 
      inv.status = 'Confirmed' or 
      inv.status = 'Declined') as tmp;
  perform 
    pg_notify(
      'timeline_item_update' || '_' || u.id,
      coalesce(result :: text, 'null' :: text))
  from auth.user as u;
  return new;
end;
$$ language 'plpgsql';

create or replace trigger on_invoice_update
after update on institution.invoice
for each row execute procedure notify_server_on_invoice();

