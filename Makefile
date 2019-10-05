# SPDX-License-Identifier: MIT
# Footwm Chez scheme Makefile.

# Library destination directory. This should be an object directory contained in (library-directories).
# eg, set in CHEZSCHEMELIBDIRS environment variable.
PREFIX := ~
LIBDIR := $(PREFIX)/lib
BINDIR := $(PREFIX)/bin
CONFDIR := ~/.foot

# Path to chez scheme executable.
SCHEME = /usr/bin/scheme

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
	footwm/xutil.sls	\
	footwm/globals.sls	\
	footwm/gobject.sls	\
	footwm/icccm.sls	\
	footwm/ewmh.sls		\
	footwm/op.sls		\
	footwm/wm.sls		\
	footwm/gtk.sls		\
	footwm/keys.sls		\
	footwm/shell.sls	\
	footwm/hints.sls	\
	footwm/menugtk.sls

LIBSO =	\
	footwm/ftypes-util.so	\
	footwm/util.so		\
	footwm/xlib.so		\
	footwm/xutil.so		\
	footwm/globals.so	\
	footwm/ewmh.so		\
	footwm/icccm.so		\
	footwm/op.so		\
	footwm/wm.so		\
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

all: $(LIBSO)

%.so: %.sls
	echo '(reset-handler abort) (compile-library "'$<'")' | $(SCHEME) $(SFLAGS)

$(BINDIR)/%: bin/%.ss
	$(INSTALL) -D -p $< $@

install-bin: $(BINS)

install-src: all
	$(INSTALL) -D -p -t $(LIBDIR)/footwm $(LIBS)

install-so: all
	$(INSTALL) -D -p -t $(LIBDIR)/footwm $(LIBSO)

install: install-src install-so install-bin

$(CONFDIR)/footkeysconfig.sls: footkeysconfig.sls.sample
	$(INSTALL) -D -p $< $@

install-config: $(CONFDIR)/footkeysconfig.sls

clean:
	rm -f $(LIBSO)
