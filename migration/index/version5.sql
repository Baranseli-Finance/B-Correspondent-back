create table notification (
  id bigserial primary key,
  user_id bigserial not null,
  created_at timestamp not null default now(),
  is_read boolean not null default false,
  body text not null,
  constraint notification__user_id__fk foreign key (user_id) references auth.user(id));