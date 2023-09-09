# Footwm Chez scheme GNUmakefile.
# Written by Jerry 2019-2023.
# SPDX-License-Identifier: Unlicense

# Path to chez scheme executable.
SCHEME := /usr/bin/chez-scheme
SCHEMEVER = $(shell $(SCHEME) --version 2>&1)

# Library destination directory. This should be an object directory contained in (library-directories).
# eg, set in CHEZSCHEMELIBDIRS environment variable.
PREFIX := $(HOME)
LIBDIR := $(PREFIX)/lib/csv$(SCHEMEVER)
IRREGEXDIR := $(LIBDIR)
BINDIR := $(PREFIX)/bin
CONFDIR := ~/.foot

BUILDDIR = BUILD-csv$(SCHEMEVER)

# Scheme compile flags.
SFLAGS = -q

# Path to install executable.
INSTALL = /usr/bin/install

## Should be no need to edit anything below here.

# Compilation is handled by building the app and letting Chez Scheme manage dependants.

PROJ = footwm

BINSRC =	\
	bin/footdesk.ss	\
        bin/footkeys.ss	\
	bin/footsh.ss	\
	bin/footwin.ss	\
	bin/footwm.ss

BINOBJ = $(addprefix $(BUILDDIR)/,$(BINSRC:.ss=.so))
BINWPO = $(BINOBJ:.so=.wpo)
# EXE = whole program executable. ie, it's the output of compile-whole-program.
BINEXE = $(BINOBJ:.so=)

SUBSRC = \
	footwm/config.sls	\
	footwm/ewmh.sls		\
	footwm/footwm.sls	\
	footwm/ftypes-util.sls	\
	footwm/gobject.sls	\
	footwm/gtk.sls		\
	footwm/hints.sls	\
	footwm/icccm.sls	\
	footwm/keys.sls		\
	footwm/menugtk.sls	\
	footwm/shell.sls	\
	footwm/util.sls		\
	footwm/wm.sls		\
	footwm/xlib.sls

IBIN = $(addprefix $(BINDIR)/,$(notdir $(BINSRC:.ss=)))

CONFIGS = \
	  etc/footkeysconfig.sls \
	  etc/footwmconfig.sls

ICONFIGS = $(addprefix $(CONFDIR)/,$(notdir $(CONFIGS)))

ISUBSRC = $(addprefix $(LIBDIR)/,$(SUBSRC))

# Setting this stops GNU make from removing these intermediate files.
.SECONDARY: $(BINWPO)

all: build

# This rule is used for building the whole-program-optimisation (wpo) file for the app
# as well as any intermediate library "shared objects" and "whole program optimisations".
# NOTES:
#   - Compiling the program is a side-effect here so outputting to .tmp.
#   - compile-program won't make the destination bin dir, so we'll do that first.
#   - compile-program adds a .wpo suffix to output filename, remove it from ours so it's
#     not duplicated. ie, avoid "output.wpo.wpo".
$(BUILDDIR)/%.wpo: %.ss $(TOPSRC) $(SUBSRC)
	@mkdir -p "$(dir $@)"
	echo \
		"(reset-handler abort)" \
		"(compile-imported-libraries #t)" \
		"(generate-wpo-files #t)" \
		"(library-directories '"'(("." . "$(BUILDDIR)") ("$(LIBDIR)" . "$(BUILDDIR)") ("$(IRREGEXDIR)" . "$(BUILDDIR)")))' \
		'(compile-program "$<" "$(@:.wpo=).tmp")' | $(SCHEME) $(SFLAGS)

# Compile the all-in-one whole program optimised executable.
# This includes only the local build-dir as it requires the wpo generated above
# which in turn compiled all used shared objects and wpo's there.
# compile-whole-program (@ Chez Scheme v9.6.2) does not create executable files, hence
# chmod at the end.
# Set lib-visible? to true so that footkeys, footsh and others can import footwm libs.
%: %.wpo
	echo \
		"(reset-handler abort)" \
		"(library-directories '"'(("." . "$(BUILDDIR)")))' \
		"(compile-whole-program \"$<\" \"$@\" #t)" | $(SCHEME) $(SFLAGS)
	@chmod +x "$@"

# Install src libs without execute permissions.
$(LIBDIR)/%: %
	$(INSTALL) -m u=rw,go=r,a-s -p -D "$<" "$@"

$(BINDIR)/%: $(BUILDDIR)/bin/%
	$(INSTALL) -p -D "$<" "$@"

$(CONFDIR)/%.sls: etc/%.sls
	$(INSTALL) -D -p $< $@

build: $(BINEXE)

install: install-bin

install-bin: $(IBIN)

install-config: $(ICONFIGS)

install-lib: $(ITOPSRC) $(ISUBSRC)

clean:
	$(RM) -r $(BUILDDIR)

clean-install-bin:
	$(RM) $(IBIN)

clean-install-lib:
	$(RM) $(ITOPSRC) $(ISUBSRC)

clean-all: clean clean-install-bin clean-install-lib
