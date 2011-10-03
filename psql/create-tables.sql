DROP VIEW IF EXISTS HS_PasteInfo;
DROP VIEW IF EXISTS HS_User;

DROP TABLE IF EXISTS paste_settings;
DROP TABLE IF EXISTS paste_contents;
DROP TABLE IF EXISTS replies;
DROP TABLE IF EXISTS tags;
DROP TABLE IF EXISTS pastes;
DROP TABLE IF EXISTS users;

-- USERS & USER SETTINGS -------------------------------------------------------

CREATE TABLE users (
  u_id                integer,
  u_name              varchar(100) UNIQUE NOT NULL,
  u_password          bytea NOT NULL,
  u_email             varchar(100) UNIQUE,

  PRIMARY KEY (u_id)
);

INSERT INTO users VALUES (-1, '', '', '');

CREATE VIEW HS_User AS
  SELECT u_id, u_name, u_email
    FROM users;

CREATE TABLE paste_settings (
  ps_user_id          integer,
  ps_default_hidden   boolean DEFAULT FALSE,
  ps_use_global_ids   boolean DEFAULT TRUE,

  PRIMARY KEY (ps_user_id),
  FOREIGN KEY (ps_user_id) REFERENCES users(u_id) ON DELETE CASCADE
);

-- PASTES ----------------------------------------------------------------------

CREATE TABLE pastes (
  p_id              varchar(40),
  p_user_id         integer NOT NULL DEFAULT -1,
  p_date            timestamp with time zone,
  p_type            varchar(100),
  p_description     varchar(250),
  p_md5             bytea NOT NULL,
  p_hidden          boolean DEFAULT FALSE,
  p_id_is_global    boolean DEFAULT TRUE,

  UNIQUE (p_user_id, p_md5),

  PRIMARY KEY (p_id),
  FOREIGN KEY (p_user_id) REFERENCES users(u_id) ON DELETE SET DEFAULT
);

CREATE VIEW HS_PasteInfo AS
  SELECT p_id, p_user_id, p_date, p_type, p_description, p_hidden, p_id_is_global
    FROM pastes;

CREATE TABLE paste_contents (
  pc_paste_id         varchar(40),
  pc_paste_user_id    integer NOT NULL DEFAULT -1,
  pc_content          bytea   NOT NULL,

  PRIMARY KEY (pc_paste_id),
  FOREIGN KEY (pc_paste_id)      REFERENCES pastes(p_id) ON DELETE CASCADE,
  FOREIGN KEY (pc_paste_user_id) REFERENCES users(u_id) ON DELETE SET DEFAULT
);

CREATE TABLE replies (
  r_paste_id             varchar(40),
  r_paste_user_id        integer NOT NULL DEFAULT -1,
  r_reply_paste_id       varchar(40),
  r_reply_paste_user_id  integer NOT NULL DEFAULT -1,

  PRIMARY KEY (r_paste_id, r_reply_paste_id),
  FOREIGN KEY (r_paste_id)            REFERENCES pastes(p_id) ON DELETE CASCADE,
  FOREIGN KEY (r_paste_user_id)       REFERENCES users(u_id) ON DELETE SET DEFAULT,
  FOREIGN KEY (r_reply_paste_id)      REFERENCES pastes(p_id) ON DELETE CASCADE,
  FOREIGN KEY (r_reply_paste_user_id) REFERENCES users(u_id) ON DELETE SET DEFAULT
);

CREATE TABLE tags (
  t_paste_id         varchar(40),
  t_paste_user_id    integer NOT NULL DEFAULT -1,
  t_tag              varchar(100) NOT NULL,

  UNIQUE (t_paste_id, t_tag), -- store each entry only once

  PRIMARY KEY (t_paste_id),
  FOREIGN KEY (t_paste_id)      REFERENCES pastes(p_id) ON DELETE CASCADE,
  FOREIGN KEY (t_paste_user_id) REFERENCES users(u_id) ON DELETE SET DEFAULT
);
