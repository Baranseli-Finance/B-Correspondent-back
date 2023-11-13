create table backup (
  created_at timestamp not null default now(),
  md5_checksum text not null,
  constraint backup__md5_checksum__uk unique (md5_checksum));