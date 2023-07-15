PROJECT = lcli
ROOT_DIR = $(shell pwd)
REPO = $(shell git config --get remote.origin.url)
LFE_BIN = _build/default/lib/lfe/bin
LFE = $(LFE_BIN)/lfe

all: build

clean-all: clean clean-docs
	rebar3 lfe clean

include priv/make/code.mk
include priv/make/docs.mk

.PHONY: all clean-all

hex-publish:
	@echo "\nPublishing to hex.pm ...\n"
	rm -rf doc
	mkdir doc
	cp priv/html/docs-redirect.html doc/index.html
	rebar3 hex publish
	rm -rf doc
