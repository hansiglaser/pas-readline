pas-readline -- Object Oriented wrapper for GNU Readline for Pascal
===================================================================

The `GNU Readline <http://cnswww.cns.cwru.edu/php/chet/readline/rltop.html>`_
library is used to prompt the user for textual input at the terminal. It
provides features like a history and completion.

This project provides Pascal header translations plus an object-oriented
wrapper for convenience. Code examples on the usage are included.

License
-------

    Copyright (C) 2012 Johann Glaser <Johann.Glaser@gmx.at>

    This program is free software; you can redistribute it and/or modify  
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or  
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc.,
    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


Directory Structure
-------------------

  ``src/``
    Header translations and OOP wrapper.

  ``src/examples/``
    Examples for the direct usage of the header translations as well as
    for the usage of the OOP wrapper. This directory also has a
    ``Makefile``.

Build
-----

::

  $ cd src/examples/
  $ make

Usage
-----

Simply add the units ``HistoryOOP`` and ``ReadlineOOP`` to the uses-clause
of your program. Then follow `src/examples/ootest.pas
<pas-readline/blob/master/src/examples/ootest.pas>`_.


Platform
--------

This project was compiled with `FreePascal <http://www.freepascal.org/>`_
2.6.0 on Linux.

Other Projects
--------------

**k7103-usb**
  The USB Interface of the Velleman k7103 PC Storage Oscilloscope
  http://k7103.sourceforge.net/ includes a command line tool to test the
  hardware and to program the CPLD. This is a bigger usage example which
  also includes a the `TCL scripting language with a Pascal wrapper
  <https://github.com/hansiglaser/pas-tcl>`_. See k7103usbtest.pas_.

  .. _k7103usbtest.pas: http://k7103.svn.sourceforge.net/viewvc/k7103/branch/usb/host-test/k7103usbtest.pas?view=markup
