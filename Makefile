.PHONY: deps test

all: rebar deps compile

rel: rebar deps release

compile:
	./rebar compile

release:
	REL=true ./rebar compile

deps:
	./rebar get-deps

clean:
	./rebar clean
	rm -rf test.*-temp-data

distclean: clean 
	./rebar delete-deps

test: all
	./rebar skip_deps=true eunit

docs:
	./rebar skip_deps=true doc

dialyzer: compile
	@dialyzer -Wno_return -c ./ebin

rebar:
	curl -O http://cloud.github.com/downloads/basho/rebar/rebar
	chmod ugo+x rebar

run:
	erl -name clue@127.0.0.1 -setcookie clue -pa ./deps/*/ebin -pa ./ebin 
