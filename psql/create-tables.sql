DROP VIEW IF EXISTS HS_PasteInfo;
DROP VIEW IF EXISTS HS_User;

DROP TABLE IF EXISTS Paste_settings;
DROP TABLE IF EXISTS Paste_contents;
DROP TABLE IF EXISTS replies;
DROP TABLE IF EXISTS tags;
DROP TABLE IF EXISTS Pastes;
DROP TABLE IF EXISTS users;

-- USERS & USER SETTINGS -------------------------------------------------------

CREATE TABLE users (
  u_id                integer,
  u_name              varchar(100) UNIQUE,
  u_password          bytea NOT NULL,
  u_email             varchar(100),

  PRIMARY KEY (u_id)
);

INSERT INTO users (u_id, u_password) VALUES (-1, '');

CREATE VIEW HS_User AS
  SELECT u_id, u_name, u_email
    FROM users;

CREATE TABLE Paste_settings (
  ps_user_id          integer,
  ps_default_hidden   boolean DEFAULT FALSE,
  ps_default_random   boolean DEFAULT FALSE,
  ps_use_global_ids   boolean DEFAULT TRUE,

  PRIMARY KEY (ps_user_id),
  FOREIGN KEY (ps_user_id) REFERENCES users(u_id) ON DELETE CASCADE
);

-- PasteS -----------------------------------------------------------------------

CREATE TABLE Pastes (
  p_id              varchar(40),
  p_user_id         integer DEFAULT -1,
  p_date            timestamp with time zone,
  p_type            varchar(100),
  p_description     varchar(250),
  p_md5             bytea UNIQUE NOT NULL,
  p_hidden          boolean DEFAULT FALSE,
  p_id_is_global    boolean DEFAULT TRUE,
  p_id_is_custom    boolean DEFAULT FALSE,

  PRIMARY KEY (p_id),
  FOREIGN KEY (p_user_id) REFERENCES users(u_id) ON DELETE SET DEFAULT
);

CREATE VIEW HS_PasteInfo AS
  SELECT p_id, p_user_id, p_date, p_type, p_description, p_hidden, p_id_is_global, p_id_is_custom
    FROM Pastes;

CREATE TABLE Paste_contents (
  pc_Paste_id         varchar(40),
  pc_Paste_user_id    integer DEFAULT -1,
  pc_content          bytea NOT NULL,

  PRIMARY KEY (pc_Paste_id),
  FOREIGN KEY (pc_Paste_id)      REFERENCES Pastes(p_id) ON DELETE CASCADE,
  FOREIGN KEY (pc_Paste_user_id) REFERENCES users(u_id) ON DELETE SET DEFAULT
);

CREATE TABLE replies (
  r_Paste_id             varchar(40),
  r_Paste_user_id        integer DEFAULT -1,
  r_reply_Paste_id       varchar(40),
  r_reply_Paste_user_id  integer DEFAULT -1,

  PRIMARY KEY (r_Paste_id),
  FOREIGN KEY (r_Paste_id)            REFERENCES Pastes(p_id) ON DELETE CASCADE,
  FOREIGN KEY (r_Paste_user_id)       REFERENCES users(u_id) ON DELETE SET DEFAULT,
  FOREIGN KEY (r_reply_Paste_id)      REFERENCES Pastes(p_id) ON DELETE CASCADE,
  FOREIGN KEY (r_reply_Paste_user_id) REFERENCES users(u_id) ON DELETE SET DEFAULT
);

CREATE TABLE tags (
  t_Paste_id         varchar(40),
  t_Paste_user_id    integer DEFAULT -1,
  t_tag             varchar(100),

  PRIMARY KEY (t_Paste_id),
  FOREIGN KEY (t_Paste_id)      REFERENCES Pastes(p_id) ON DELETE CASCADE,
  FOREIGN KEY (t_Paste_user_id) REFERENCES users(u_id) ON DELETE SET DEFAULT
);
