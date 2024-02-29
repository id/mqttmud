CREATE TABLE IF NOT EXISTS players(id SERIAL PRIMARY KEY, name TEXT NOT NULL, data JSONB NOT NULL default '{}');
CREATE UNIQUE INDEX IF NOT EXISTS players_name ON players (name);
CREATE TABLE IF NOT EXISTS sessions(id SERIAL PRIMARY KEY, player_id INTEGER NOT NULL, client_id TEXT NOT NULL);
CREATE UNIQUE INDEX IF NOT EXISTS sessions_player_id_client_id ON sessions (player_id, client_id);
CREATE TABLE IF NOT EXISTS rooms(id TEXT PRIMARY KEY, name TEXT NOT NULL, description TEXT NOT NULL default '');
CREATE TABLE IF NOT EXISTS exits(id TEXT PRIMARY KEY, name TEXT NOT NULL, description TEXT NOT NULL default '', from_id TEXT NOT NULL REFERENCES rooms(id), to_id TEXT NOT NULL REFERENCES rooms(id));
CREATE TABLE IF NOT EXISTS room_players(room_id TEXT REFERENCES rooms(id), player_id INTEGER REFERENCES players(id));
