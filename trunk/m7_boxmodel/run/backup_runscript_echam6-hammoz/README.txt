-------------
Introduction
-------------

This directory contains an example of a basic runscript for blizzard, DKRZ (as of 2012-09-30), and its accompanying ancillary files. The runtime settings and links to input files correspond to the release defauts of echam6.1.0-ham2.1-moz0.8 (2012-09).

It has been created as a 'backup solution' in case the echam(-hammoz) jobscript toolkit is not available on your platform. 

--> * It is strongly recommended to install and use the jobscript toolkit instead of this, as it has many features that allow much easier usage and much less errors!! * There will be only limited support for the present runscript example and its customization for alternate platforms (but full support for the jobscript toolkit).

For detailed instructions for installing the jobscript toolkit on your platform, please refer to the users- and administrator guides, posted on HAMMOZ redmine here:

https://redmine.hammoz.ethz.ch/projects/hammoz/wiki/4_jobscript_toolkit

Note: on blizzard, the jobscript toolkit is currently installed and available here:

/home/zmaw/m222045/bin/jobscript_toolkit

To use it, simply set your PATHS as:

declare -x PATH=/home/zmaw/m222045/bin/jobscript_toolkit/bin:$PATH
declare -x MANPATH=/home/zmaw/m222045/bin/jobscript_toolkit/share/man:$MANPATH

----------------------------
Contents of this directory:
----------------------------

* backup_runscript_echam6-hammoz.sh: main runscript for launching an echam6-hammoz job with defaults settings on blizzard.

* emi_spec_backup_runscript_echam6-hammoz.txt: default emissions specifications

* symlinks_backup_runscript_echam6-hammoz.sh: script executed by 'backup_runscript_echam6-hammoz.sh' to create the symbolic links to the relevant input files

* get_rundates_info.sh and get_input_file_choices.sh: ancillary scripts executed by 'backup_runscript_echam6-hammoz.sh'. Both should not need to be edited.

-------
Usage:
-------

1. create a subdirectory for your new experiment: [your echam6-hammoz]/run/[my_exp]

2. copy the present files in [your echam6-hammoz]/run/[my_exp]

3. edit backup_runscript_echam6-hammoz.sh to adjust it for your experiment (and for your batch system in case you are not running on blizzard)

4. optionnally: edit and adjust emi_spec_backup_runscript_echam6-hammoz.txt and symlinks_backup_runscript_echam6-hammoz.sh in case you need to use alternate emissions specifications and alternate input files.

5. launch your job with:

llsubmit backup_runscript_echam6-hammoz.sh

Note: the batch job launch command may be different than 'llsubmit' on your platform.
