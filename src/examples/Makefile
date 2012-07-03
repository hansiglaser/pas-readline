
EXECUTABLES = exwikipedia rltest rlversion histexamp rl
FPC_OPTS = -Fu..


all: $(EXECUTABLES)

%: %.pas
	fpc $(FPC_OPTS) $<

clean:
	rm -f *.o *.ppu *~ *.bak $(EXECUTABLES)
