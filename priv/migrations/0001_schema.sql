CREATE TABLE IF NOT EXISTS players(id SERIAL PRIMARY KEY, name TEXT NOT NULL, data JSONB NOT NULL default '{}');
CREATE UNIQUE INDEX IF NOT EXISTS players_name ON players (name);
CREATE TABLE IF NOT EXISTS sessions(player_id INTEGER PRIMARY KEY, client_id TEXT NOT NULL);
CREATE UNIQUE INDEX IF NOT EXISTS sessions_player_id ON sessions (player_id);
CREATE TABLE IF NOT EXISTS rooms(id TEXT PRIMARY KEY, name TEXT NOT NULL, description TEXT NOT NULL default '');
CREATE TABLE IF NOT EXISTS exits(id TEXT PRIMARY KEY, name TEXT NOT NULL, description TEXT NOT NULL default '', from_id TEXT NOT NULL REFERENCES rooms(id) ON DELETE CASCADE, to_id TEXT NOT NULL REFERENCES rooms(id) ON DELETE CASCADE);
CREATE TABLE IF NOT EXISTS room_players(room_id TEXT REFERENCES rooms(id) ON DELETE CASCADE, player_id INTEGER REFERENCES players(id) ON DELETE CASCADE);