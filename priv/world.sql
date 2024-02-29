INSERT INTO rooms (id, name) VALUES ('tavern', 'Tavern') ON CONFLICT DO NOTHING;
INSERT INTO rooms (id, name) VALUES ('outside', 'Outside') ON CONFLICT DO NOTHING;
INSERT INTO exits (id, name, from_id, to_id) VALUES ('outside', 'Outside', 'tavern', 'outside') ON CONFLICT DO NOTHING;
INSERT INTO exits (id, name, from_id, to_id) VALUES ('tavern', 'Tavern', 'outside', 'tavern') ON CONFLICT DO NOTHING;
