.PHONY: build repl test clean clean-all

build:
	@rebar3 compile

repl:
	@rebar3 lfe repl

test: clean
	@rebar3 as test do compile,lfe ltest -tall

xref:
	@rebar3 xref

test-all: test xref examples

clean:
	@rm -rf _build/*/lib/lcli build/*/plugins/lcli

clean-all: clean
	@rm -rf _build
