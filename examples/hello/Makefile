EXAMPLES = hello \
	   hello2 \
	   hello_table \
	   hello_userdata \
	   hello_sandbox \
	   hello_funcalls

ROOTDIR = ../..
SRCDIR  = $(ROOTDIR)/src
BEAMDIR = $(ROOTDIR)/ebin

all: $(EXAMPLES)

clean:
	rm -f *.beam erl_crash.dump

.SECONDARY:

%.beam: %.erl $(SRCDIR)/*.hrl
	erlc -I $(SRCDIR) $<

%: %.beam
	erl -pa $(BEAMDIR) -s $@ run -s init stop -noshell

.PHONY: all clean
