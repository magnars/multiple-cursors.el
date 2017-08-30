# multiple-cursors.el [![Build Status](https://secure.travis-ci.org/magnars/multiple-cursors.el.png)](http://travis-ci.org/magnars/multiple-cursors.el)

Multiple cursors for Emacs. This is some pretty crazy functionality, so yes,
there are kinks. Don't be afraid tho, I've been using it since 2011 with
great success and much merriment.

## Installation

I highly recommend installing multiple-cursors through `package.el`.

It's available on [MELPA](http://melpa.org/) and [MELPA Stable](http://stable.melpa.org):

    M-x package-install multiple-cursors

The package depends on the `cl-lib` package, so if you do not use
`package.el` or have a recent Emacs, you would need to install that
too: see [GNU ELPA](http://elpa.gnu.org/packages/cl-lib.html).

## Basic usage

Start out with:

    (require 'multiple-cursors)

Then you have to set up your keybindings - multiple-cursors doesn't presume to
know how you'd like them laid out. Here are some examples:

When you have an active region that spans multiple lines, the following will
add a cursor to each line:

    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

When you want to add multiple cursors not based on continuous lines, but based on
keywords in the buffer, use:

    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

First mark the word, then add more cursors.

To get out of multiple-cursors-mode, press `<return>` or `C-g`. The latter will
first disable multiple regions before disabling multiple cursors. If you want to
insert a newline in multiple-cursors-mode, use `C-j`.

## Important note

Multiple cursors does not do well when you invoke its commands with `M-x`. It needs to be bound to keys to work properly. Pull request to fix this is welcome.

## Video

You can [watch an intro to multiple-cursors at Emacs Rocks](http://emacsrocks.com/e13.html).

## Command overview

### Mark one more occurrence

 - `mc/mark-next-like-this`: Adds a cursor and region at the next part of the buffer forwards that matches the current region.
 - `mc/mark-next-like-this-word`: Adds a cursor and region at the next part of the buffer forwards that matches the current region, if  no region is selected it selects the word at the point.
 - `mc/mark-next-like-this-symbol`: Adds a cursor and region at the next part of the buffer forwards that matches the current region, if  no region is selected it selects the symbol at the point.
 - `mc/mark-next-word-like-this`: Like `mc/mark-next-like-this` but only for whole words.
 - `mc/mark-next-symbol-like-this`: Like `mc/mark-next-like-this` but only for whole symbols.
 - `mc/mark-previous-like-this`: Adds a cursor and region at the next part of the buffer backwards that matches the current region.
 - `mc/mark-previous-like-this-word`: Adds a cursor and region at the next part of the buffer backwards that matches the current region, if  no region is selected it selects the word at the point.
 - `mc/mark-previous-like-this-symbol`: Adds a cursor and region at the next part of the buffer backwards that matches the current region, if  no region is selected it selects the symbol at the point.
 - `mc/mark-previous-word-like-this`: Like `mc/mark-previous-like-this` but only for whole words.
 - `mc/mark-previous-symbol-like-this`: Like `mc/mark-previous-like-this` but only for whole symbols.
 - `mc/mark-more-like-this-extended`: Use arrow keys to quickly mark/skip next/previous occurances.
 - `mc/add-cursor-on-click`: Bind to a mouse event to add cursors by clicking. See tips-section.
 - `mc/mark-pop`: Set a cursor at the current point and move to the next (different) position on the mark stack.  This allows for fine grained control over the placement of cursors.

### Juggle around with the current cursors

 - `mc/unmark-next-like-this`: Remove the cursor furthest down in the buffer.
 - `mc/unmark-previous-like-this`: Remove the cursor furthest up in the buffer.
 - `mc/skip-to-next-like-this`: Remove the cursor furthest down, marking the next occurance down.
 - `mc/skip-to-previous-like-this`: Remove the cursor furthest up, marking the next occurance up.

### Mark many occurrences

 - `mc/edit-lines`: Adds one cursor to each line in the current region.
 - `mc/edit-beginnings-of-lines`: Adds a cursor at the start of each line in the current region.
 - `mc/edit-ends-of-lines`: Adds a cursor at the end of each line in the current region.
 - `mc/mark-all-like-this`: Marks all parts of the buffer that matches the current region.
 - `mc/mark-all-words-like-this`: Like `mc/mark-all-like-this` but only for whole words.
 - `mc/mark-all-symbols-like-this`: Like `mc/mark-all-like-this` but only for whole symbols.
 - `mc/mark-all-in-region`: Prompts for a string to match in the region, adding cursors to all of them.
 - `mc/mark-all-like-this-in-defun`: Marks all parts of the current defun that matches the current region.
 - `mc/mark-all-words-like-this-in-defun`: Like `mc/mark-all-like-this-in-defun` but only for whole words.
 - `mc/mark-all-symbols-like-this-in-defun`: Like `mc/mark-all-like-this-in-defun` but only for whole symbols.
 - `mc/mark-all-dwim`: Tries to be smart about marking everything you want. Can be pressed multiple times.

### Special

 - `set-rectangular-region-anchor`: Think of this one as `set-mark` except you're marking a rectangular region.
 - `mc/mark-sgml-tag-pair`: Mark the current opening and closing tag.
 - `mc/insert-numbers`: Insert increasing numbers for each cursor, top to bottom.
 - `mc/insert-letters`: Insert increasing letters for each cursor, top to bottom.
 - `mc/sort-regions`: Sort the marked regions alphabetically.
 - `mc/reverse-regions`: Reverse the order of the marked regions.

## Tips and tricks

- To get out of multiple-cursors-mode, press `<return>` or `C-g`. The latter will
  first disable multiple regions before disabling multiple cursors. If you want to
  insert a newline in multiple-cursors-mode, use `C-j`.

- `(define-key mc/keymap (kbd "<return>") nil)` will make `<return>` insert a
  newline; multiple-cursors-mode can still be disabled with `C-g`.

- Sometimes you end up with cursors outside of your view. You can
  scroll the screen to center on each cursor with `C-v` and `M-v` or you can
  press `C-'` to hide all lines without a cursor, press `C-'` again to unhide.

- Try pressing `mc/mark-next-like-this` with no region selected. It
  will just add a cursor on the next line.

- Try pressing `mc/mark-next-like-this-word` or
  `mc/mark-next-like-this-symbol` with no region selected. It will
  mark the word or symbol and add a cursor at the next occurance

- Try pressing `mc/mark-all-like-this-dwim` on a tagname in html-mode.

- Notice that the number of cursors active can be seen in the modeline.

- If you get out of multiple-cursors-mode and yank - it will yank only
  from the kill-ring of main cursor. To yank from the kill-rings of
  every cursor use yank-rectangle, normally found at C-x r y.

- You can use `mc/reverse-regions` with nothing selected and just one cursor.
  It will then flip the sexp at point and the one below it.

- When you use `mc/edit-lines`, you can give it a positive or negative
  prefix to change how it behaves on too short lines.

- If you would like to keep the global bindings clean, and get custom keybindings
  when the region is active, you can try [region-bindings-mode](https://github.com/fgallina/region-bindings-mode).

BTW, I highly recommend adding `mc/mark-next-like-this` to a key binding that's
right next to the key for `er/expand-region`.

### Binding mouse events

To override a mouse event, you will likely have to also unbind the
`down-mouse` part of the event. Like this:

    (global-unset-key (kbd "M-<down-mouse-1>"))
    (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

Or you can do like me and find an unused, but less convenient, binding:

    (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

## Unknown commands

Multiple-cursors uses two lists of commands to know what to do: the run-once list
and the run-for-all list. It comes with a set of defaults, but it would be beyond silly
to try and include all the known Emacs commands.

So that's why multiple-cursors occasionally asks what to do about a command. It will
then remember your choice by saving it in `~/.emacs.d/.mc-lists.el`. You can change
the location with:

    (setq mc/list-file "/my/preferred/file")

NB! Make sure to do so before requiring multiple-cursors.

It is possible to set multiple-cursors to "run-for-all" for every
command except for those that are listed in `mc/cmds-to-run-once`. To
enable this set `mc/always-run-for-all` to non-nil. Add commands to be
run once to `mc/cmds-to-run-once` in ".mc-lists.el".

## Known limitations

* isearch-forward and isearch-backward aren't supported with multiple cursors.
  If you want this functionality, you can use [phi-search](https://github.com/zk-phi/phi-search).
* Commands run with `M-x` won't be repeated for all cursors.
* All key bindings that refer to lambdas are always run for all cursors. If you
  need to limit it, you will have to give it a name.
* Redo might screw with your cursors. Undo works very well.


## Contribute

Yes, please do. There's a suite of tests, so remember to add tests for your
specific feature, or I might break it later.

You'll find the repo at:

    https://github.com/magnars/multiple-cursors.el

To fetch the test dependencies, install
[cask](https://github.com/rejeep/cask.el) if you haven't already,
then:

    $ cd /path/to/multiple-cursors
    $ cask

Run the tests with:

    $ ./run-tests.sh

## Contributors

* [Takafumi Arakaki](https://github.com/tkf) has contributed several small improvements
* [Marco Baringer](https://github.com/segv) contributed looping to `mc/cycle` and adding cursors without region for mark-more.
* [Ivan Andrus](https://github.com/gvol) added showing number of cursors in mode-line, and different options for how to handle short lines in `mc/edit-lines`.
* [Fuco](https://github.com/Fuco1) added the first version of `mc/mark-all-like-this-dwim`
* [Zach Kost-Smith](https://github.com/smithzvk) added `mc/mark-pop`
* [Maciej Katafiasz](https://github.com/mathrick) added `mc/mark-all-dwim`
* [Aleksey Fedotov](https://github.com/lexa) added `mc-hide-unmatched-lines-mode`
* [Jules Tamagnan](https://github.com/jtamagnan) added `mc/mark-next-like-this-word` and `mc/mark-next-like-this-symbol`
* [Ingo Lohmar](https://github.com/ilohmar) extended `mc/add-cursor-on-click` to toggle cursors.
* [Andrea Orru](https://github.com/AndreaOrru) added `mc/mark-previous-like-this-word`/`-symbol`

Thanks!

## License

Copyright (C) 2012-2016 Magnar Sveen

Author: Magnar Sveen <magnars@gmail.com>
Keywords: editing cursors

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
