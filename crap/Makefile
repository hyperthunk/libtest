.SUFFIXES: 

# env
ERL ?= erl
ERL_TOP ?= /usr/local/lib/erlang
EBIN=./ebin
CODE_PATH := -pa $(EBIN) 
ERL_BOOT := -s ct_run script_start -s erlang halt
ERL_FLAGS=$(CODE_PATH) $(ERL_BOOT) -I include 
BEAMDIRS := `find src -type d | sed s/src/ebin/`
COVER_SOURES := `find src -type f | sed s/src/ebin/`
TESTLOGS=./test/logs

PORT_TEST_DIR := test/port_server_SUITE_data
PORT_TEST_CSRC := `find test/port_server_SUITE_data -type f | grep .c`
PORT_TEST_OBJ=$(patsubst %.c, %.o, $(PORT_TEST_CSRC))

# rules and tasks

.PHONY: clean build test 

all: clean build

#TODO: this is very ugly - we should at least set up rules instead
test: $(TESTLOGS) $(COVER_SOURCES) build
	touch ./libtest.coverdata
	$(ERL) -sname ct $(ERL_FLAGS) -logdir $(TESTLOGS) -dir test -cover coverage.spec

build: $(EBIN)
	$(ERL) -noshell -make

$(EBIN):
	mkdir -p $(BEAMDIRS)

$(TESTLOGS): 
	mkdir -p $@

clean:
	rm -rf $(EBIN) test/*.beam $(TESTLOGS)

