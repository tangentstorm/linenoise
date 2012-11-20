# ln.pas

This began as a wrapper for, and then a direct port of [linenoise](https://github.com/antirez/linenoise) in free pascal.

The linenoise philosophy is to strip most of the terminal-specific logic from the line editing library and rely instead on a small subset of the standard [ANSI escape sequences](http://en.wikipedia.org/wiki/ANSI_escape_code#CSI_codes):

  * CHA (Cursor Horizontal Absolute)
    * Sequence: ESC [ n G
    * Effect: moves cursor to column n

  * EL (Erase Line)
    * Sequence: ESC [ n K
    * Effect: if n is 0 or missing, clear from cursor to end of line
    * Effect: if n is 1, clear from beginning of line to cursor
    * Effect: if n is 2, clear entire line

  * CUF (CUrsor Forward)
    * Sequence: ESC [ n C
    * Effect: moves cursor forward of n chars

  * cursorhome -- subset of CUP (CUrsor Position)
    * Sequence: ESC [ H
    * Effect: moves the cursor to upper left corner

  * ED2 (Clear entire screen)
    * Sequence: ESC [ 2 J
    * Effect: clear the whole screen

The pascal version diverged significantly from the c version, as free pascal offers many of the required tools in its standard library:

  * modules to deal with the terminal
  * a stringlist data structure, used for the command history and tab completion
  * many standard procedures to operate on the strings themselves.

However, the code still follows the overall structure of the C version, and some of the functions are indeed direct ports, so this should probably most accurately be called a fork.
