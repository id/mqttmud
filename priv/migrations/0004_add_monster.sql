CREATE TABLE IF NOT EXISTS monsters(
       id SERIAL PRIMARY KEY,
       name TEXT NOT NULL,
       room_id TEXT REFERENCES rooms(id) ON DELETE CASCADE,
       hp INTEGER NOT NULL,
       current_hp INTEGER NOT NULL,
       ac INTEGER NOT NULL,
       xp INTEGER NOT NULL,
       atk_mod INTEGER NOT NULL,
       dmg TEXT NOT NULL,
       dmg_mod INTEGER NOT NULL,
       loot_min TEXT NOT NULL,
       loot_max TEXT NOT NULL,
       alive BOOLEAN DEFAULT TRUE,
       respawn_interval_seconds INTEGER NOT NULL,
       data JSONB NOT NULL default '{}');
CREATE UNIQUE INDEX IF NOT EXISTS monsters_name ON monsters (name);
CREATE INDEX IF NOT EXISTS monsters_room_id ON monsters (room_id);

INSERT INTO monsters (name, room_id, hp, current_hp, ac, xp, atk_mod, dmg, dmg_mod, loot_min, loot_max, respawn_interval_seconds) VALUES ('Goblin', 'forest', 7, 7, 15, 50, 4, '1d6', 4, '1 sp', '5 sp', 60) ON CONFLICT DO NOTHING;

ALTER TABLE players ADD COLUMN IF NOT EXISTS hp INTEGER NOT NULL DEFAULT 12;
ALTER TABLE players ADD COLUMN IF NOT EXISTS current_hp INTEGER NOT NULL DEFAULT 12;
ALTER TABLE players ADD COLUMN IF NOT EXISTS ac INTEGER NOT NULL DEFAULT 14;
ALTER TABLE players ADD COLUMN IF NOT EXISTS dmg TEXT NOT NULL DEFAULT '1d6';
ALTER TABLE players ADD COLUMN IF NOT EXISTS dmg_mod INTEGER NOT NULL DEFAULT 5;
ALTER TABLE players ADD COLUMN IF NOT EXISTS level INTEGER NOT NULL DEFAULT 1;
ALTER TABLE players ADD COLUMN IF NOT EXISTS alive BOOLEAN NOT NULL DEFAULT TRUE;
ALTER TABLE players ADD COLUMN IF NOT EXISTS respawn_interval_seconds INTEGER NOT NULL DEFAULT 60;

ALTER TABLE sessions ADD COLUMN IF NOT EXISTS status TEXT NOT NULL DEFAULT 'normal';
ALTER TABLE sessions ADD COLUMN IF NOT EXISTS monster_id INTEGER REFERENCES monsters(id) ON DELETE SET NULL;

INSERT INTO game (schema_version) VALUES (4) ON CONFLICT DO NOTHING;
