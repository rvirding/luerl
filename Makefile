# Copyright (c) 2016-2023 Robert Virding
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

ERLCFLAGS = -W1 +debug_info
ERLC ?= erlc

all: compile

.PHONY: all compile clean echo examples debug docs

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

###############
### TESTING ###
###############

# XXX for some reason, the first pass of eunit doesn't run the tests?!
eunit:
	@rebar3 as test do compile,eunit,eunit

common-test:
	@rebar3 as test do compile,ct

ct: common-test

tests:
	@rebar3 as test do compile,eunit,eunit,ct

# this protects the intermediate .erl files from make's auto deletion
#.SECONDARY: $(XRL_INTERM) $(YRL_INTERM)

#####################
### DOCUMENTATION ###
#####################

# Targets for generating docs and man pages
DOCDIR = doc_legacy
DOCSRC = $(DOCDIR)/src
MANDIR = $(DOCDIR)/man
PDFDIR = $(DOCDIR)/pdf
EPUBDIR = $(DOCDIR)/epub
MANINSTDIR ?= $(PREFIX)/share/man

MAN1_SRCS = $(notdir $(wildcard $(DOCSRC)/*1.md))
MAN1S = $(MAN1_SRCS:.1.md=.1)
TXT1S = $(MAN1_SRCS:.1.md=.txt)
PDF1S = $(MAN1_SRCS:.1.md=.pdf)
MAN3_SRCS = $(notdir $(wildcard $(DOCSRC)/*3.md))
MAN3S = $(MAN3_SRCS:.3.md=.3)
PDF3S = $(MAN3_SRCS:.3.md=.pdf)
TXT3S = $(MAN3_SRCS:.3.md=.txt)
MAN7_SRCS = $(notdir $(wildcard $(DOCSRC)/*7.md))
MAN7S = $(MAN7_SRCS:.7.md=.7)
TXT7S = $(MAN7_SRCS:.7.md=.txt)
PDF7S = $(MAN7_SRCS:.7.md=.pdf)

# For pandoc for generating PDFs as it omly accepts a few options.
# xelatex is a reasonable default or wkhtmltopdf.
PANDOCPDF ?= xelatex

# Just generate the docs that are tracked in git
docs: docs-txt

docs-man: \
	$(addprefix $(MANDIR)/, $(MAN1S)) \
	$(addprefix $(MANDIR)/, $(MAN3S)) \
	$(addprefix $(MANDIR)/, $(MAN7S))


$(MANDIR)/%.1: $(DOCSRC)/%.1.md
	pandoc -f markdown -s -t man -o $@ $<

$(MANDIR)/%.3: $(DOCSRC)/%.3.md
	pandoc -f markdown -s -t man -o $@ $<

$(MANDIR)/%.7: $(DOCSRC)/%.7.md
	pandoc -f markdown -s -t man -o $@ $<

clean-docs:
	rm -f $(DOCDIR)/*.txt $(MANDIR)/*.[0-9] $(PDFDIR)/*.pdf $(EPUBDIR)/*.epub

docs-txt: docs-man \
	$(addprefix $(DOCDIR)/, $(TXT1S)) \
	$(addprefix $(DOCDIR)/, $(TXT3S)) \
	$(addprefix $(DOCDIR)/, $(TXT7S))
	@if [ -f $(DOCDIR)/luerl_guide.txt ]; then \
		cp $(DOCDIR)/luerl_guide.txt $(DOCDIR)/user_guide.txt ; \
	fi

$(DOCDIR)/%.txt: export GROFF_NO_SGR=1

$(DOCDIR)/%.txt: $(MANDIR)/%.1
	groff -t -e -mandoc -Tutf8 $< | col -bx > $@

$(DOCDIR)/%.txt: $(MANDIR)/%.3
	groff -t -e -mandoc -Tutf8 $< | col -bx > $@

$(DOCDIR)/%.txt: $(MANDIR)/%.7
	groff -t -e -mandoc -Tutf8 $< | col -bx > $@

$(PDFDIR):
	@$(INSTALL_DIR) $(PDFDIR)

docs-pdf: $(PDFDIR) \
	$(addprefix $(PDFDIR)/, $(PDF1S)) \
	$(addprefix $(PDFDIR)/, $(PDF3S)) \
	$(addprefix $(PDFDIR)/, $(PDF7S))

$(PDFDIR)/%.pdf: $(DOCSRC)/%.1.md
	pandoc -f markdown --pdf-engine=$(PANDOCPDF) -o $@ $<

$(PDFDIR)/%.pdf: $(DOCSRC)/%.3.md
	pandoc -f markdown --pdf-engine=$(PANDOCPDF) -o $@ $<

$(PDFDIR)/%.pdf: $(DOCSRC)/%.7.md
	pandoc -f markdown --pdf-engine=$(PANDOCPDF) -o $@ $<

################
### RELEASES ###
################

hex-publish: clean-all compile
	rebar3 hex publish package

tags:
	git tag $(shell erl -eval $(GET_VERSION)|tr -d '"')
	git tag v$(shell erl -eval $(GET_VERSION)|tr -d '"')
	git push --tags
