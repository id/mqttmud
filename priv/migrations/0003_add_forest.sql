CREATE TABLE IF NOT EXISTS game (schema_version INTEGER, updated_at TIMESTAMP WITH TIME ZONE NOT NULL default CURRENT_TIMESTAMP);
CREATE UNIQUE INDEX IF NOT EXISTS game_schema_version ON game (schema_version);
UPDATE rooms SET description = 'A cozy tavern.' WHERE id = 'tavern';
INSERT INTO rooms (id, name, description) VALUES ('village', 'Village', 'A small village.') ON CONFLICT DO NOTHING;
UPDATE room_players SET room_id = 'village' WHERE room_id = 'outside';
INSERT INTO exits (id, name, from_id, to_id, description) VALUES ('village', 'Village', 'tavern', 'village', 'Go out into the village.') ON CONFLICT DO NOTHING;
DELETE FROM exits WHERE id = 'outside';
DELETE FROM exits WHERE id = 'tavern';
INSERT INTO exits (id, name, from_id, to_id, description) VALUES ('tavern', 'Tavern', 'village', 'tavern', 'Enter the tavern.') ON CONFLICT DO NOTHING;
DELETE FROM rooms WHERE id = 'outside';

INSERT INTO rooms (id, name, description) VALUES ('forest', 'Forest', 'A dark and mysterious forest.') ON CONFLICT DO NOTHING;
INSERT INTO exits (id, name, from_id, to_id, description) VALUES ('forest', 'Forest', 'village', 'forest', 'To the forest.') ON CONFLICT DO NOTHING;
INSERT INTO exits (id, name, from_id, to_id, description) VALUES ('village-outside', 'Village', 'forest', 'village', 'Back to the village.') ON CONFLICT DO NOTHING;

INSERT INTO game (schema_version) VALUES (3) ON CONFLICT DO NOTHING;
