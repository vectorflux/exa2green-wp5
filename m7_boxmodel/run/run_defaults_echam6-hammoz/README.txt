This is the default settings for echam6.1.0-ham2.1-moz0.9. It is a climatologic setup, at resolution T63L31GR15, with prescribed SOA.

The files are meant to be read and used by means of the jobscript toolkit (see
the jobscript toolkit documentation in:
https://redmine.hammoz.ethz.ch/projects/hammoz/wiki/4_jobscript_toolkit for
more details).

------
Note:
------

on blizzard (DKRZ, as of 20120930), the jobscript toolkit is currently
installed and available here:

/home/zmaw/m222045/bin/jobscript_toolkit

To use it, simply set your PATHS as:

declare -x PATH=/home/zmaw/m222045/bin/jobscript_toolkit/bin:$PATH
declare -x MANPATH=/home/zmaw/m222045/bin/jobscript_toolkit/share/man:$MANPATH
