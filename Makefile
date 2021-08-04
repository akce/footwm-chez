# Footwm Chez scheme Makefile.
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
WSOS = $(BINS:.ss=.wso)

IBINS = $(addprefix $(BINDIR)/,$(notdir $(BINS:.ss=)))

CONFIGS = \
	  etc/footkeysconfig.sls \
	  etc/footwmconfig.sls

ICONFIGS = $(addprefix $(CONFDIR)/,$(notdir $(CONFIGS)))

.SECONDARY: $(WSOS) $(BINS:.ss=.wpo)

all: $(IBINS)

%.wpo: %.ss
	echo "(reset-handler abort) (compile-imported-libraries #t) (generate-wpo-files #t) (library-directories '(\".\" \"$(IRREGEXDIR)\")) (compile-program \"$<\")" | $(SCHEME) $(SFLAGS)

%.wso: %.wpo
	echo "(reset-handler abort) (compile-imported-libraries #t) (generate-wpo-files #t) (library-directories '(\".\" \"$(IRREGEXDIR)\")) (compile-whole-program \"$<\" \"$@\" #t)" | $(SCHEME) $(SFLAGS)
	chmod +x "$@"

$(BINDIR)/%: bin/%.wso
	$(INSTALL) $< $@

$(CONFDIR)/%.sls: etc/%.sls
	$(INSTALL) -D -p $< $@

install: all

install-config: $(ICONFIGS)

clean:
	rm -f $(WSOS) $(SOS) $(BINS:.ss=.wpo) $(IBINS) footwm/*.{so,wpo}
