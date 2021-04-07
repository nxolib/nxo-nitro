CREATE TABLE IF NOT EXISTS nxo_settings (
  setting_group   VARCHAR(128) NOT NULL,
  setting_name    VARCHAR(128) NOT NULL,
  setting_display VARCHAR(128) NOT NULL,
  setting_value   TEXT NULL,
  setting_desc    TEXT NULL,
  setting_type    VARCHAR(16) NOT NULL DEFAULT 'string',
  PRIMARY KEY(setting_group, setting_name)
);

CREATE TABLE IF NOT EXISTS nxo_setting_labels (
  setting_group VARCHAR(128) PRIMARY KEY NOT NULL,
  setting_group_label VARCHAR(128) NOT NULL
);

CREATE TABLE IF NOT EXISTS nxo_settings_audit(
  audit_id      UUID DEFAULT gen_random_uuid() PRIMARY KEY,
  setting_group VARCHAR(128) NOT NULL,
  setting_name  VARCHAR(128) NOT NULL,
  user_id       UUID NULL REFERENCES nxo_users(user_id) ON DELETE CASCADE,
  old_value     TEXT NULL,
  new_value     TEXT NULL,
  action_dt     TIMESTAMP NOT NULL DEFAULT now()
);


-- Define the settings.

INSERT INTO nxo_settings(setting_group, setting_name,
                          setting_display, setting_desc,
                          setting_type, setting_value)
VALUES
    ('dev', 'development_mode', 'Development Mode', null, 'onoff', true)
  , ('dev', 'require_login',    'Require Login', null, 'onoff', false)
  , ('dev', 'session_timeout',  'Session Timeout', null, 'integer', 15)

  , ('user', 'password_length', 'Minimum Password Length', null, 'integer', 8)
  , ('user', 'length_words',    'Minimum Length in Words', null, 'string',
      'eight')

  , ('mail', 'smtp_host',  'SMTP Relay',            null, 'string',
       'mailcatcher')
  , ('mail', 'smtp_port',  'SMTP Port',             null, 'integer', 1025)
  , ('mail', 'smtp_from',  'Outgoing Mail Address', null, 'string',
       'outgoing@example.com')
  , ('mail', 'admin_addr', 'Administrator Address', null, 'string',
       'admin@example.com')

  , ('duo', 'duo_ikey', 'Duo Integration Key', '', 'string', null)
  , ('duo', 'duo_skey', 'Duo Secret Key',      '', 'string', null)
  , ('duo', 'duo_akey', 'Duo Application Key', '', 'string', null)
  , ('duo', 'duo_host', 'Duo Hostname',        '', 'string', null)

  , ('ad', 'ad_host',      'AD Hostname',      '', 'string' , null)
  , ('ad', 'ad_port',      'AD Port',          '', 'integer', null)
  , ('ad', 'ad_base',      'AD Base DN',       '', 'string' , null)
  , ('ad', 'ad_bind',      'AD Bind DN',       '', 'string' , null)
  , ('ad', 'ad_pass_file', 'AD Password File', '', 'string' , null)

ON CONFLICT DO nothing;

INSERT INTO nxo_setting_labels(setting_group, setting_group_label)
VALUES
    ('dev',  'Development Settings')
  , ('user', 'User Account Settings')
  , ('duo',  'Duo Security Settings')
  , ('ad',   'Active Directory Settings')
  , ('mail', 'Mail Server Settings')
ON CONFLICT DO NOTHING;
