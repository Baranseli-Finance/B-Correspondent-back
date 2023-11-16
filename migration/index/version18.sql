create table auth.query_credentials (
  institution_id bigserial not null,
  login text not null,
  password text not null,
  constraint auth_query_credentials__institution_id__fk foreign key (institution_id) references auth.institution(id));