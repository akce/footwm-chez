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

NOTE NOTE: the compiled version of `footsh` does not pre-import footwm library bindings. This is due to a difference between [Chez Scheme] program mode vs script mode that i haven't quite figured out yet. Run `footsh` from within the source directory if you need this feature although hopefully it's not needed so much now that the code has matured.

It's always best here to refer to the source code for what is possible.

A sample session. Comments are embedded:
```
$ footsh
> ; query for all windows
  (x-query-tree)
(4194786 4196454 4195745 18874371 4196341 4195903 4196100
 8388614 10485762 10485765 10485768 10485780 25165825
 18874369 18874384 23068673 18874387 18874390 31457281
 33554433 35651585 18874409 20971521)
> ; query only for managed top level windows
  ewmh.client-list
(4194786 4196454 18874371 4196341 4195745 4195903 4196100
  8388614)
> ; get the list of managed window titles
  (map ewmh.name ewmh.client-list)
("footsh" "README.md + (~/marvels/footwm) - NVIM"
  "GitHub - akce/footwm-chez: Implementation of footwm in chez scheme — Mozilla Firefox"
  "icccm.sls (~/marvels/footwm/footwm) - NVIM"
  "|> Josie (Everything's Gonna Be Fine) / Blink 182 - PEACE"
  "ewmh.sls (~/marvels/footwm/footwm) - NVIM"
  "jerry@talisman:~" "tmux")
> ; move the current window to the second desktop
  (ewmh.window-desktop-set! (car ewmh.client-list) 1)
1
> ; have footwm react to changes and push to X server
  (shell.sync)
1
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
