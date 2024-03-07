# Multi-User Dungeon MQTT game server in Erlang

## Quick Start

```bash
docker compose up -d
make run
```

Then open `http://localhost:8080` in your browser.

## MQTT and EMQX features used

- Topic rewrite
- Rule Engine rule with MQTT events and republish action
- HTTP API to create users, subscribe and unsubscribe
- ACL File authorization
