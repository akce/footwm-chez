# Footwm Chez scheme GNUmakefile.
# Written by Jerry 2019-2021.
# SPDX-License-Identifier: Unlicense

# Path to chez scheme executable.
SCHEME := /usr/bin/chez-scheme

# Library destination directory. This should be an object directory contained in (library-directories).
# eg, set in CHEZSCHEMELIBDIRS environment variable.
PREFIX := ~
IRREGEXDIR := $(PREFIX)/lib/csv$(shell $(SCHEME) --version 2>&1)
BINDIR := $(PREFIX)/bin
CONFDIR := ~/.foot

# Scheme compile flags.
SFLAGS = -q

# Path to install executable.
INSTALL = /usr/bin/install

## Should be no need to edit anything below here.

BINS =	\
	bin/footdesk.ss	\
        bin/footkeys.ss	\
	bin/footsh.ss	\
	bin/footwin.ss	\
	bin/footwm.ss

SOS = $(BINS:.ss=.so)

# wso = whole-shared-object, but it could be called .app or something similar.
# These are the outputs of compile-whole-{library,program}.
# It's a custom extension created so as not to clash with regular .so shared objects.
WSOS = $(BINS:.ss=.wso)

# Using wildcard is a bit of a cheat but reduces maintenance.
# Just make sure to avoid creating dummy/test .sls files in the footwm sub-dir.
SRCS = $(wildcard footwm/*.sls)

IBINS = $(addprefix $(BINDIR)/,$(notdir $(BINS:.ss=)))

CONFIGS = \
	  etc/footkeysconfig.sls \
	  etc/footwmconfig.sls

ICONFIGS = $(addprefix $(CONFDIR)/,$(notdir $(CONFIGS)))

# Setting this stops GNU make from removing these intermediate files.
.SECONDARY: $(WSOS) $(BINS:.ss=.wpo)

all: $(WSOS)

%.wpo: %.ss $(SRCS)
	echo "(reset-handler abort) (compile-imported-libraries #t) (generate-wpo-files #t) (library-directories '(\".\" \"$(IRREGEXDIR)\")) (compile-program \"$<\")" | $(SCHEME) $(SFLAGS)

# Must set libs-visible? to #t so that footkeys configs can import footwm libs.
%.wso: %.wpo
	echo "(reset-handler abort) (compile-imported-libraries #t) (generate-wpo-files #t) (library-directories '(\".\" \"$(IRREGEXDIR)\")) (compile-whole-program \"$<\" \"$@\" #t)" | $(SCHEME) $(SFLAGS)
	chmod +x "$@"

$(BINDIR)/%: bin/%.wso
	$(INSTALL) -p $< $@

$(CONFDIR)/%.sls: etc/%.sls
	$(INSTALL) -D -p $< $@

install: $(IBINS)

install-config: $(ICONFIGS)

clean:
	$(RM) $(WSOS) $(SOS) $(BINS:.ss=.wpo) footwm/*.so footwm/*.wpo

clean-all: clean
	$(RM) $(IBINS)
