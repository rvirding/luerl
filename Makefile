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

all: $(OBJECTS) hello.beam

$(BEAMDIR)/%.beam: $(SRCDIR)/%.erl
	@ mkdir -p $(BEAMDIR) 
	erlc -o $(BEAMDIR) $<

%.erl: %.xrl
	erl -noinput -run leex file "$<" -run init stop -noshell

%.erl: %.yrl
	erl -noinput -run yecc file "$<" -run init stop -noshell

hello: all
	@ erl -pa ./ebin -s hello run -s init stop -noshell

hello.beam: hello.erl
	@ echo "----------------------------------" 
	@ echo "Compiling and running ./hello.erl:" 
	erlc hello.erl
	erl -pa ./ebin -s hello run -s init stop -noshell

hello2: all
	@ echo "-------------------------------------------" 
	@ echo "./examples/hello/hello2.erl:" 
	erlc -o ebin ./examples/hello/hello2.erl
	erl -pa ./ebin -s hello2 run -s init stop -noshell

bench: all bench.beam
	@ echo "(Do 'lua bench.lua' to see what you should see.)" 
	erl -pa ./ebin -s bench run -s init stop -noshell

bench.beam: bench.erl
	erlc bench.erl

clean:
	@ rm -rf $(BEAMDIR)
	@ rm -f *.beam
	@ rm -f erl_crash.dump
	@ rm -f $(XRL_INTERM)
	@ rm -f $(YRL_INTERM)

echo: 
	echo $(OBJECTS) 

.PHONY: all

# this protects the intermediate .erl files from make's auto deletion
.SECONDARY: $(XRL_INTERM) $(YRL_INTERM)
