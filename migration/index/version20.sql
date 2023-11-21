create table institution.auth_token (
  institution text not null,
  token text not null,
  constraint institution_auth_token__institution__unique unique (institution));