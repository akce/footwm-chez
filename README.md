# Welcome to footwm!

Footwm - Focus On One Thing Window Manager.

Footwm aims to efficiently manage lots of windows without the need for tab bars, or getting in the way.

It does this by:
- Storing windows and desktops in Most Recently Used (MRU) order
- Providing menus to find, filter and then perform actions on those windows/desktops.

Footwm is written in mostly [r6rs](http://www.r6rs.org/) scheme (using [Chez Scheme]) and implements standard [EWMH] and [ICCC] hints where possible.

Footwm functionality is divided among these applications:
- footwm: the window manager client
- footkeys: keyboard handler
- footwin: [gtk3] menu for selecting and closing windows
- footdesk: [gtk3] menu for selecting and closing desktops
- footsh: command line client for manipulating footwm/[EWMH]/[ICCC] hints

Footwm supports [EWMH] struts so it is possible to use status bars such as [tint2](https://gitlab.com/o9000/tint2).

Footwm does not provide an execution monitor. Using a terminal multiplexer like [tmux] is highly recommended instead (see HACKING below).

## How To Use

Or why footwm behaves as it does.

When I'm working, I tend to have a rough idea of the last few windows that I've been using. Storing the windows in most recently used order makes it easy to access.

The menus allow to search for a window/desktop just by typing. The menu removes items that don't match the text as it's being typed leaving the choice of options fairly quickly. Pressing ENTER while there are multiple matches pushes the current text to the filter stack allowing to AND join multiple texts. This can be done as many times as needed to arrive at the desired matches.

### Misc menu usage notes

footdesk & footwin: losing focus will close the menu.

footdesk & footwin: ESC will close the menu.

footwin: \<ALT>-DEL will close *every* window in the display list.

footdesk: typing the name of a desktop that does not exist and pressing ENTER will create that desktop.

## Dependencies

[Chez Scheme]

[irregex] (for footdesk & footwin)

[gtk3] (for footdesk & footwin)

## Building and Installing

`footwm` can be compiled to component program files and installed using the provided GNU makefile.

These will be installed to BINDIR, which is ~/bin by default.

eg, to install to /usr/local/bin
```sh
$ make
$ sudo make BINDIR=/usr/local/bin install
```

The makefile requires IRREGEXDIR to point to local [irregex] source. The default location is ~/lib/csv**chez-version**. eg, ~/lib/csv9.5.4

The makefile also has a target to install the sample config files. This should only be run once during the very first install.

Be very careful as it will overwrite any existing (and possibly customised) configs.

To use:

    $ make install-config

The footwm source can be run in-place, just add its path to CHEZSCHEMELIBDIRS and `bin` to your shell's PATH environment variables. However, this should probably only be done if you're modifying the code.

Note that footwm dependencies are not tracked, so if you make changes to any of the library files, you'll need to rebuild everything from scratch. ie,

    $ make clean && make

### Insert into .xinitrc.

The snippet below creates a new [tmux] session called *wm* and runs footwm and footkeys within it (as well as a couple of other useful services).

Note that you'll need to create a non-background process to stop xinit/startx from exiting.

```
    # Create wm tmux session, and hide the mouse.
    tmux new-session -s wm -d -n unclutter unclutter
    # Start the rxvt-unicode daemon.
    tmux new-window -t wm -n urxvtd urxvtd -o
    # Footwm keyboard manager.
    tmux new-window -t wm -n footkeys footkeys
    # Footwm window manager.
    tmux new-window -t wm -n footwm footwm

    # Non background process.
    env DISPLAY=:0 urxvt -name wm -e tmux attach-session -t wm
```

## Hacking

The main apps are all set to break into the chez debugger on error. Running them within [tmux] (or other terminal multiplexer) allows access to the prompt from any console.

On catastrophic error, you may need to switch out of X into a virtual terminal and then attach.

    <CTRL>-<ALT>-F2
    host login: <username>
    Password: *****
    $ tmux attach -t wm
    # fix issue...
    # restart footwm (note that you'll probably have to specify DISPLAY :0)
    # optionally detach from session
    <CTRL>-b d
    # switch back to X.. usually on 1st or 7th virtual terminal
    <ALT>-F7

Footwm has no state. Instead it gathers all needed information from X properties on demand, so it should be possible (depending on the error) to restart the footwm process and resume after code has been modified.

The foot shell (footsh) also allows for easy manipulation of window state. Invoking footsh without arguments will run a new Chez cafe (ie, a REPL) with the main footwm libraries pre-loaded and all (except xlib) prefixed.

NOTE: xlib is not prefixed as it uses ftypes and redefining exported symbols can cause issues there.

It's always best here to refer to the source code for what is possible.

A sample session. Numbers are printed in hexadecimal, comments are embedded:
```
$ footsh
> ; query for all windows
  (x-query-tree)
(#x1E00006 #x1600003 #x1400003 #x1200006 #x2400006 #x1C00006
 #x2C00003 #x2000006 #x1000006 #xA00004 #xC00001 #x800003
 #x800006 #x800009 #x800015 #x2C00001 #x2C00009 #x1800001
 #x1400001 #x1400009 #x1600001 #x1600009)
> ; query only for managed top level windows
  ewmh.client-list
(#x1E00006 #x1600003 #x1400003 #x1200006 #x2400006 #x2C00003
  #x1C00006 #x2000006 #x1000006 #x800009)
> ; get the list of managed window titles
  (map ewmh.window-name ewmh.client-list)
("footsh" "README.md + (~/marvels/footwm-focus) - GVIM1"
  "footsh.ss (~/marvels/footwm-focus/bin) - GVIM"
  "|> Everything / Anathema - PEACE"
  "Chez Scheme Version 10 User's Guide - Lynx"
  "GNUmakefile (~/marvels/footwm-focus) - GVIM2"
  "rlwrap gday-shell"
  " =INBOX [Msgs:30/7189 New:4 Flag:1 819M]|" "tmux" "tint2")
> ; move the current window to the second desktop
  (ewmh.window-desktop-set! (car ewmh.client-list) 1)
1
> ; have footwm react to changes and push to X server
  (shell.sync)
1
> ; run some shell commands directly
> (shell.desktops)
0 footwm
1 pea
2 comms
3 sys
> (shell.windows)
#x1E00006 0 0 shell st-256color footsh
#x1600003 1 0 gvim Gvim README.md + (~/marvels/footwm-focus) - GVIM1
#x1400003 2 0 gvim Gvim footsh.ss (~/marvels/footwm-focus/bin) - GVIM
#x2400006 3 0 lynx st-256color Chez Scheme Version 10 User's Guide - Lynx
#x2C00003 4 0 gvim Gvim GNUmakefile (~/marvels/footwm-focus) - GVIM2
#x1200006 5 1 peace st-256color |> Everything / Anathema - PEACE
#x1C00006 6 2 shell st-256color rlwrap gday-shell
#x2000006 7 2 hexwizard st-256color  =INBOX [Msgs:30/7189 New:4 Flag:1 819M]|
#x1000006 8 3 wm st-256color tmux
#x800009 9 FFFFFFFF tint2 Tint2 tint2

```
A limitation of footsh is that it won't manipulate/eval running footwm code, as experienced LISPers might expect.

Instead, alter X properties via `footsh` or some other app. `footwm` is stateless and loads from X on each event change. Or restart `footwm` itself if there's a code change.

For those perusing the code: the naming convention for functions tries to follow OBJECT-ATTRIBUTE. eg, (ewmh.window-desktop wid) refers to the desktop assigned to window wid.

## License

footwm is an [unlicensed](LICENSE) work released into the Public Domain.


[Chez Scheme]: https://cisco.github.io/ChezScheme/ "Chez Scheme"

[EWMH]: https://specifications.freedesktop.org/wm-spec/wm-spec-latest.html "EWMH"

[ICCC]: https://www.x.org/docs/ICCCM/icccm.pdf

[tmux]: https://github.com/tmux/tmux

[irregex]: http://synthcode.com/scheme/irregex/ "irregex"

[gtk3]: https://developer.gnome.org/gtk3/stable/ "gtk3"
