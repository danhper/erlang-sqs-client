REBAR:=rebar

.PHONY: all erl test clean doc 

FOO=/usr/lib/erlang/lib/xmerl-1.3.4/include

all: erl

erl:
	$(REBAR) get-deps compile

test: all
	@mkdir -p .eunit
	$(REBAR) skip_deps=true eunit

clean:
	$(REBAR) clean
	-rm -rvf deps ebin doc .eunit

doc:
	$(REBAR) doc

