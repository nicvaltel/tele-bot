BEGIN TRANSACTION;

create extension IF NOT EXISTS citext;
create extension IF NOT EXISTS  pgcrypto;

CREATE SCHEMA weight_control;

create table weight_control.users (
  user_id bigint primary key not null,
  username text not null,
  created timestamp with time zone default (now() at time zone 'utc')
);

create table weight_control.messages (
  id bigserial primary key not null,
  user_id bigint not null,
  text text not null,
  is_weight boolean not null default 'f',
  sent timestamp with time zone default (now() at time zone 'utc'),
  FOREIGN KEY (user_id) REFERENCES weight_control.users(user_id)
);

create table weight_control.weight (
  id bigserial primary key not null,
  user_id bigint not null,
  weight float4 not null,
  sent timestamp with time zone default (now() at time zone 'utc'),
  FOREIGN KEY (user_id) REFERENCES weight_control.users(user_id)
);

END TRANSACTION;