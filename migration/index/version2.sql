create schema if not exists auth;
create table auth.user (
    id bigserial primary key,
    email text not null,
    login text not null,
    pass text not null,
    created_at timestamptz not null default now(),
    modified_at timestamptz,
    constraint auth_user__email__uk unique (email));

create table auth.code (
    code int not null default cast((random()*1000000) as decimal(6)),
    expire_at timestamptz not null default now() + interval '5 min',
    user_id bigserial not null,
    created_at timestamptz not null default now(),
    uuid text not null,
    constraint auth_code__user_id__fk foreign key (user_id) references auth.user(id),
    constraint auth_code__uuid__uk unique (uuid));

create table auth.institution (
    id bigserial primary key,
    title text not null,
    created_at timestamptz not null default now(),
    key text not null,
    constraint auth_institution__title__uk unique (title));

create table auth.jwt (
    id uuid primary key,
    value text not null,
    created_at timestamptz not null default now(),
    is_valid boolean not null default true);

create table auth.user_jwt (
    user_id bigserial not null,
    jwt_id uuid not null,
    constraint auth_user_jwt__user_id__fk foreign key (user_id) references auth.user(id),
    constraint auth_user_jwt__jwt_id__fk foreign key (jwt_id) references auth.jwt(id));

create table auth.institution_jwt (
    inst_id bigserial not null,
    jwt_id uuid not null,
    constraint auth_user_jwt__inst_id__fk foreign key (inst_id) references auth.institution(id),
    constraint auth_user_jwt__jwt_id__fk foreign key (jwt_id) references auth.jwt(id));