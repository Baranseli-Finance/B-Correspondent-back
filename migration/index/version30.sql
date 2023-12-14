create table institution.transaction_id_structure (
    institution_id bigserial not null references auth.institution(id),
    country text not null,
    currency text not null,
    idx bigint not null,
    constraint institution__transaction_id_structure__unique unique (institution_id, country, currency));