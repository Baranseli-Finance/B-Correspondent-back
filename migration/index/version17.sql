create table webhook (
  id uuid primary key default uuid_generate_v4(),
  institution_id bigserial not null,
  created_at timestamp not null default now(),
  message jsonb not null,
  is_delivered bool not null default false,
  constraint webhook__institution_id__fk foreign key (institution_id) references auth.institution(id));

create table webhook_credentials (
  institution_id bigserial not null,
  login text not null,
  password text not null,
  constraint webhook_credentials__institution_id__fk foreign key (institution_id) references auth.institution(id));