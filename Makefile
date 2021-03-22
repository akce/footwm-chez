# Footwm Chez scheme Makefile.
# Written by Akce 2019-2020.
# SPDX-License-Identifier: Unlicense

# Path to chez scheme executable.
SCHEME := /usr/bin/scheme

# Library destination directory. This should be an object directory contained in (library-directories).
# eg, set in CHEZSCHEMELIBDIRS environment variable.
PREFIX := ~
LIBDIR := $(PREFIX)/lib/csv$(shell $(SCHEME) --version 2>&1)
BINDIR := $(PREFIX)/bin
CONFDIR := ~/.foot

# Scheme compile flags.
SFLAGS = -q

# Path to install executable.
INSTALL = /usr/bin/install

## Should be no need to edit anything below here.

# Libs need to go in use order or else there can be 'different compilation instance' exceptions.
LIBS =	\
	footwm/ftypes-util.sls	\
	footwm/util.sls		\
	footwm/xlib.sls		\
	footwm/gobject.sls	\
	footwm/icccm.sls	\
	footwm/ewmh.sls		\
	footwm/wm.sls		\
	footwm/footwm.sls	\
	footwm/gtk.sls		\
	footwm/keys.sls		\
	footwm/shell.sls	\
	footwm/hints.sls	\
	footwm/menugtk.sls

LIBSO =	\
	footwm/ftypes-util.so	\
	footwm/util.so		\
	footwm/xlib.so		\
	footwm/ewmh.so		\
	footwm/icccm.so		\
	footwm/wm.so		\
	footwm/footwm.so	\
	footwm/gobject.so	\
	footwm/gtk.so		\
	footwm/keys.so		\
	footwm/shell.so		\
	footwm/hints.so		\
	footwm/menugtk.so

BINS =	\
	$(BINDIR)/footdesk	\
        $(BINDIR)/footkeys	\
	$(BINDIR)/footsh	\
	$(BINDIR)/footwin	\
	$(BINDIR)/footwm

CONFIGS = \
	  etc/footkeysconfig.sls \
	  etc/footwmconfig.sls

ICONFIGS = $(addprefix $(CONFDIR)/,$(notdir $(CONFIGS)))

all: $(LIBSO)

%.so: %.sls
	echo '(reset-handler abort) (compile-library "'$<'")' | $(SCHEME) $(SFLAGS)

$(BINDIR)/%: bin/%.ss
	$(INSTALL) -D -p $< $@

$(CONFDIR)/%.sls: etc/%.sls
	$(INSTALL) -D -p $< $@

install-bin: $(BINS)

install-src: all
	$(INSTALL) -D -p -t $(LIBDIR)/footwm $(LIBS)

install-so: all
	$(INSTALL) -D -p -t $(LIBDIR)/footwm $(LIBSO)

install: install-src install-so install-bin

install-config: $(ICONFIGS)

clean:
	rm -f $(LIBSO)
