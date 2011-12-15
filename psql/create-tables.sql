DROP TABLE IF EXISTS replies;
DROP TABLE IF EXISTS tags;
DROP TABLE IF EXISTS pastes;
DROP TABLE IF EXISTS users;


-- USERS & USER SETTINGS -------------------------------------------------------

CREATE TABLE users (
  id                integer,
  name              varchar(100) UNIQUE NOT NULL,
  password          bytea NOT NULL,
  email             varchar(100) UNIQUE,

  default_hidden    boolean DEFAULT FALSE,
  use_global_ids    boolean DEFAULT TRUE,

  PRIMARY KEY (id)
);

INSERT INTO users (id,name,password,email) VALUES (-1, '', '', '');


-- PASTES ----------------------------------------------------------------------

CREATE TABLE pastes (
  id              varchar(40),
  user_id         integer NOT NULL DEFAULT -1,
  date            timestamp with time zone,
  type            varchar(100),
  description     varchar(250),
  md5             bytea NOT NULL,
  content         bytea NOT NULL,
  hidden          boolean DEFAULT FALSE,
  id_is_global    boolean DEFAULT TRUE,

  UNIQUE (user_id, md5),

  PRIMARY KEY (id),
  FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE SET DEFAULT
);

CREATE TABLE replies (
  paste_id      varchar(40),
  reply_id      varchar(40),

  PRIMARY KEY (paste_id, reply_id),
  FOREIGN KEY (paste_id) REFERENCES pastes(id) ON DELETE CASCADE,
  FOREIGN KEY (reply_id) REFERENCES pastes(id) ON DELETE CASCADE
);

CREATE TABLE tags (
  id            varchar(40),
  tag           varchar(100) NOT NULL,

  PRIMARY KEY (id, tag),
  FOREIGN KEY (id) REFERENCES pastes(id) ON DELETE CASCADE
);
