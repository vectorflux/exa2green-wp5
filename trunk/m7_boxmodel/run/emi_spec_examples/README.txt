This directory is meant to contain various emission specification examples.

It must contain the three compulsory examples:

* emi_spec_aerocom_II+psoa.txt (echam6.1.0-ham2.1-moz0.8 default): aerocom_II emissions with prescribed SOA
* emi_spec_aerocom+psoa.txt: outdated aerocom emissions with prescribed SOA
* emi_spec_aerocom+isoa.txt: outdated aerocom emissions with interactive SOA

You may store your own emission specification examples here.

Note: in the current state of the jobscript toolkit, it is not possible to make prepare_run.sh fetch any emission specification file that is none of the three above mentionned compulsory ones.

In case you want to use an existing emission specifications file different from the three above mentionned ones, here is how to proceed:

1. prepare_run.sh -e other [my_exp]
2. copy manually any emission specification (e.g. stored here) in your my_exp/ subdirectory.
When using a new aerosol dataset:
3. adjust the lines where 'aero_dataset' appears in my_exp/symlinks_my_exp.sh
so that you make it possible to make new symbolic links pointing to your new aero dataset
4. adjust the variable 'aero_dataset' in my_exp/settings_my_exp accordingly

