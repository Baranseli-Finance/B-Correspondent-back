create table email_journal (
  id uuid primary key default uuid_generate_v4(),
  user_email text not null,
  subject text not null,
  body text not null,
  reject_reason text,
  agent text not null);