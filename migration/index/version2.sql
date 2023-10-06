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
    code int not null default cast(random()*(999999 - 100000) + 100000 as decimal(6)),
    expire_at timestamptz not null default now() + interval '5 min',
    user_id bigserial not null,
    created_at timestamptz not null default now(),
    uuid text not null,
    browser_fp text not null,
    is_expended bool not null default false,
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
    browser_fp text,
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

create table auth.role (
    id bigserial primary key,
    role text not null,
    parent_id bigint,
    constraint auth_role__parent_id__fk foreign key (parent_id) references auth.role(id));

create table auth.user_role (
    user_id bigint not null,
    role_id bigint not null,
    constraint auth_user_role__user_id__fk foreign key (user_id) references auth.user(id),
    constraint auth_user_role__role_id__fk foreign key (role_id) references auth.role(id),
    constraint auth_user_role__user_id_role_id__uk unique (user_id, role_id));

create table auth.inst_role (
    inst_id bigint not null,
    role_id bigint not null,
    constraint auth_inst_role__user_id__fk foreign key (inst_id) references auth.institution(id),
    constraint auth_user_role__role_id__fk foreign key (role_id) references auth.role(id),
    constraint auth_inst_role__inst_id_role_id__uk unique (inst_id, role_id));

insert into auth.role (role) values ('Admin');
insert into auth.role (role, parent_id) values ('Writer', 1);
insert into auth.role (role, parent_id) values ('Reader', 2);
insert into auth.role (role, parent_id) values ('None', 3);
insert into auth.role (role, parent_id) values ('Source', 1);


