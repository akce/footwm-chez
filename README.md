# Welcome to footwm!

Footwm - Focus On One Thing Window Manager.

Footwm aims to efficiently manage multitudes of windows without the need for tab bars or getting in the way.

It does this by:
- Storing windows and desktops in Most Recently Used (MRU) order
- Providing menus to find, filter and then perform actions on those windows/desktops.

[EWMH]: https://specifications.freedesktop.org/wm-spec/wm-spec-latest.html "EWMH"
[ICCC]: https://www.x.org/docs/ICCCM/icccm.pdf

Footwm is written in mostly [r6rs](http://www.r6rs.org/) scheme (using [Chez Scheme]) and implements standard [EWMH] and [ICCC] hints where possible.

Footwm functionality is divided among these applications:
- footwm: the window manager client
- footkeys: keyboard handler
- footwin: [gtk3] menu for selecting and closing windows
- footdesk: [gtk3] menu for selecting and closing desktops
- footsh: command line client for manipulating footwm/[EWMH]/[ICCC] hints

[tmux]: https://github.com/tmux/tmux

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

[Chez Scheme]: https://cisco.github.io/ChezScheme/ "Chez Scheme"
[irregex]: http://synthcode.com/scheme/irregex/ "irregex"
[gtk3]: https://developer.gnome.org/gtk3/stable/ "gtk3"

[Chez Scheme]

[irregex] (for footdesk & footwin)

[gtk3] (for footdesk & footwin)

## Installing

Clone the repository to *dir*.

Add *dir* to PATH environment variable.

Add *dir* to CHEZSCHEMELIBDIRS environment variable.

Copy and customise footkeys config.

    $ mkdir ~/.foot
    $ cp -vi *dir*/footkeysconfig.ss.sample ~/.foot/footkeysconfig.ss
    $ edit ~/.foot/footkeysconfig.ss

### Insert into .xinitrc.

The snippet below creates a new [tmux] session called *wm* and runs footwm and footkeys within it (as well as a couple of other useful services).

Note that you'll need to create a non-background process to stop xinit/startx from exiting.

```
    # Footwm window manager.
    tmux new-session -s wm -d -n footwm footwm.ss
    # Footwm keyboard manager.
    tmux new-window -t wm -n footkeys footkeys.ss
    # Hide the mouse.
    tmux new-window -t wm -n unclutter unclutter
    # Start the rxvt-unicode daemon.
    tmux new-window -t wm -n urxvtd urxvtd -o

    # Non background process.
    env DISPLAY=:0 urxvt -name wm -e tmux attach-session -t wm
```

Optional: Install remaining dependencies ([irregex] & [gtk3]) for the menu apps.

## Hacking

The main apps are all set to break into the chez debugger on error. Running them within [tmux] (or other terminal multiplexer) allows access to the prompt from any console.

On catastrophic error, you may need to switch out of X into a virtual terminal and then attach.

    <CTRL>-<ALT>-F2
    host login: <username>
    Password: *****
    $ tmux attach -t wm
    # fix issue...
    # optionally detach from session
    <CTRL>-b d
    # switch back to X.. usually on 1st or 7th virtual terminal
    <ALT>-F7

Footwm has no state, it gathers all needed information from X properties, so it should be possible (depending on the error) to restart the footwm process and resume after code has been modified.

## License

Copyright (c) 2019 Akce. Released under the MIT license. See LICENSE file for details.
