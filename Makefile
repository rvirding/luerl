# Copyright (c) 2014 Robert Virding
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

# Luerl Makefile
# Building from .xrl, .yrl and .erl
# Intermediaries from leex and yecc stay in ./src

SRCDIR        =./src
BEAMDIR       =./ebin
ERL_SOURCES  := $(wildcard $(SRCDIR)/*.erl)
XRL_SOURCES  := $(wildcard $(SRCDIR)/*.xrl)
YRL_SOURCES  := $(wildcard $(SRCDIR)/*.yrl)
ERL_MODULES  := $(ERL_SOURCES:$(SRCDIR)/%.erl=%)
XRL_MODULES  := $(XRL_SOURCES:$(SRCDIR)/%.xrl=%)
YRL_MODULES  := $(YRL_SOURCES:$(SRCDIR)/%.yrl=%)
XRL_INTERM   := $(XRL_MODULES:%=$(SRCDIR)/%.erl)
YRL_INTERM   := $(YRL_MODULES:%=$(SRCDIR)/%.erl)
MODULES      := $(XRL_MODULES) $(YRL_MODULES) $(ERL_MODULES)
OBJECTS      := $(MODULES:%=$(BEAMDIR)/%.beam)

all: $(OBJECTS)

$(BEAMDIR)/%.beam: $(SRCDIR)/%.erl $(SRCDIR)/luerl.hrl
	@ mkdir -p $(BEAMDIR) 
	erlc $(ERLCFLAGS) -o $(BEAMDIR) $<

%.erl: %.xrl
	erlc -o $(SRCDIR) $<

%.erl: %.yrl
	erlc -o $(SRCDIR) $<

clean:
	@ rm -rf $(BEAMDIR)
	@ rm -f *.beam
	@ rm -f erl_crash.dump
	@ rm -f $(XRL_INTERM)
	@ rm -f $(YRL_INTERM)
	$(MAKE) -C examples clean

echo: 
	echo $(OBJECTS) 

examples: all
	$(MAKE) -C examples

debug:
	ERLCFLAGS="+debug_info" make all

.PHONY: all examples clean

# this protects the intermediate .erl files from make's auto deletion
.SECONDARY: $(XRL_INTERM) $(YRL_INTERM)
