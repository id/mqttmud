REBAR ?= $(or $(shell which rebar3 2>/dev/null),$(CURDIR)/rebar3)
REBAR_URL := https://github.com/erlang/rebar3/releases/download/3.22.1/rebar3
APP_NAME := $(shell basename $(CURDIR))
export BUILD_WITHOUT_QUIC=1

all: compile

compile:
	$(REBAR) compile

rel:
	$(REBAR) release

shell:
	$(REBAR) shell

run:
	$(REBAR) release
	_build/default/rel/$(APP_NAME)/bin/$(APP_NAME) console

deps:
	$(REBAR) get-deps

test: eunit ct cover

clean:
	@rm -rf _build erl_crash.dump rebar3.crashdump rebar.lock rebar3

clean-data:
	@rm -rf data/{cluster.uuid,node.uuid,mnesia,trace} data/configs/*.config data/configs/*.args pgdata

fmt:
	@$(REBAR) fmt

_build: deps

$(REBAR):
	curl -fsSL "$(REBAR_URL)" -o $@
	chmod +x $@
