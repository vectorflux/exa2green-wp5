This directory is meant to contain various run examples (to be extended in future), in order to easily create specific pre-defined experiments.

You may store your own run examples here.

The files are meant to be read and used by means of the jobscript toolkit (see the jobscript toolkit documentation in: https://redmine.hammoz.ethz.ch/projects/hammoz/wiki/4_jobscript_toolkit for more details).

Each subdirectory contains a README file which documents the purpose of the experiment.

------------------------------------
Usage (with the jobscript toolkit):
------------------------------------

prepare_run.sh -r [/path/to/run_examples/some_subdir_run_example] [my_exp]

------
Note:
------

on blizzard (DKRZ, as of 20120930), the jobscript toolkit is currently installed and available here:

/home/zmaw/m222045/bin/jobscript_toolkit

To use it, simply set your PATHS as:

declare -x PATH=/home/zmaw/m222045/bin/jobscript_toolkit/bin:$PATH
declare -x MANPATH=/home/zmaw/m222045/bin/jobscript_toolkit/share/man:$MANPATH
