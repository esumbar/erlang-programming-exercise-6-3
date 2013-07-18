# Based on Makefile template from "Programming Erlang" 2/ed
# by Joe Armstrong, p. 163 (PDF version)

.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean 

MODS = my_supervisor add_two

all: compile

compile: ${MODS:%=%.beam}
	
clean:	
	rm -rf *.beam erl_crash.dump
