PN=erlymon

# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================
ERL = $(shell which erl)

ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

REBAR=$(shell which rebar3)

ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

.PHONY: all clean deps compile release tar install uninstall

all: clean deps compile release
# =============================================================================
# Rules to build the system
# =============================================================================
deps:
	$(REBAR) upgrade
tar:
	$(REBAR) tar
release:
	$(REBAR) release
compile:
	$(REBAR) compile
clean:
	$(REBAR) clean
	
install:
	cp -r $(CURDIR)/_build/default/rel/${PN} /opt
uninstall:
	rm -rf /opt/${PN}
