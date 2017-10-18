create table todos (
  id serial primary key,
  title text not null,
  done boolean not null
);