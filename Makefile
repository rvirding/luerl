# Copyright (c) 2016 Robert Virding
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
#  limitations under the License.

# Makefile for Luerl
# Building from .xrl, .yrl and .erl
# Intermediaries from leex and yecc stay in ./src

BINDIR        = ./bin
EBINDIR       = ./ebin
SRCDIR        = ./src

LIB = luerl

# To run erl as bash
FINISH = -run init stop -noshell

# Scripts to be evaluated

GET_VERSION = '{ok,[App]}=file:consult("src/$(LIB).app.src"), \
	V=proplists:get_value(vsn,element(3,App)), \
	io:format("~p~n",[V])' \
	$(FINISH)


## The .erl, .xrl, .yrl and .beam files
ESRCS  := $(notdir $(wildcard $(SRCDIR)/*.erl))
XSRCS  := $(notdir $(wildcard $(SRCDIR)/*.xrl))
YSRCS  := $(notdir $(wildcard $(SRCDIR)/*.yrl))
EBINS = $(ESRCS:.erl=.beam) $(XSRCS:.xrl=.beam) $(YSRCS:.yrl=.beam)

ERLCFLAGS = -W1
ERLC ?= erlc

all: compile

.PHONY: all compile clean echo examples debug

compile: comp_opts.mk $(addprefix $(EBINDIR)/, $(EBINS))

$(EBINDIR)/%.beam: $(SRCDIR)/%.erl $(SRCDIR)/luerl.hrl comp_opts.mk
	@ mkdir -p $(EBINDIR)
	$(ERLC) $(ERLCFLAGS) -o $(EBINDIR) $(COMP_OPTS) $(ERLCFLAGS) $<

%.erl: %.xrl
	$(ERLC) -o $(SRCDIR) $<

%.erl: %.yrl
	$(ERLC) -o $(SRCDIR) $<

comp_opts.mk: get_comp_opts.escript
	escript get_comp_opts.escript

-include comp_opts.mk

clean:
	@ rm -f $(EBINDIR)/*.beam
	@ rm -f *.beam
	@ rm -f erl_crash.dump
	@ rm comp_opts.mk
	$(MAKE) -C examples clean

clean-all: clean
	rm -rf _build

echo: 
	echo $(OBJECTS) 

get-version:
	@echo
	@echo "Getting version info ..."
	@echo
	@echo -n app.src: ''
	@erl -eval $(GET_VERSION)

examples: all
	$(MAKE) -C examples

debug:
	ERLCFLAGS="+debug_info" make all

# this protects the intermediate .erl files from make's auto deletion
#.SECONDARY: $(XRL_INTERM) $(YRL_INTERM)

################
### RELEASES ###
################

hex-publish: clean-all compile
	rebar3 hex publish package

tags:
	git tag $(shell erl -eval $(GET_VERSION)|tr -d '"')
	git tag v$(shell erl -eval $(GET_VERSION)|tr -d '"')
	git push --tags
