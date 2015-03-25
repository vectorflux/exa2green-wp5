#!/usr/bin/env python

import re
import os
import sys
from math import sqrt, log10

ATOL = 1.0e-3
RTOL = 1.0e-4
EPS  = 2.2204460492503131E-016

regex = re.compile('^([+\-]?)([0-9.]+)e?([+\-])([0-9.]+)$')

def convert(s):
  m = re.search(regex, s)
  if m:
    s = ''.join([m.group(1), m.group(2), 'e', m.group(3), m.group(4)])
  try:
    fval = float(s)
  except ValueError:
    print '================> %s' % s
    fval = 0.0
  if fval < EPS:
    return 0.0
  else:
    return fval
      

if __name__ == '__main__':
  if len(sys.argv) < 3:
    print 'Usage: %s KPP_FILE.dat Kppa_file.dat' % sys.argv[0]
    sys.exit(1)

  try:
    kpp_file = open(sys.argv[1], 'r')
  except:
    print 'Error: cannot open %s for reading' % sys.argv[1]
    sys.exit(1)

  try:
    kppa_file = open(sys.argv[2], 'r')
  except:
    print 'Error: cannot open %s for reading' % sys.argv[2]
    sys.exit(1)

  # Skip initial concentration line in KPP file
  kpp_file.readline()

  sigPow = 0.0
  errPow = 0.0
  errCount = 0.0
  for line in kpp_file:
    parts = line.split()
    kpp_time = parts[0]
    kpp_conc = parts[1:]
    parts = kppa_file.readline().split()
    kppa_time = parts[0]
    kppa_conc = parts[3:]

    if len(kpp_conc) != len(kppa_conc):
      print 'ERROR: Concentration vectors have different length: KPP(%d) != Kppa(%d)' % (len(kpp_conc), len(kppa_conc))
      sys.exit(1)

    for i, conc in enumerate(kppa_conc):
      c1 = convert(conc)
      c2 = convert(kpp_conc[i])
      sigSq = c1*c1
      errSq = (c1 - c2) * (c1 - c2)
      try:
        rerr = sqrt(errSq) / abs(c2)
        if rerr > RTOL:
          print("%d: KPP(%g), Kppa(%g) : %g" % (i, c2, c1, rerr))
      except ZeroDivisionError:
        pass
      sigPow += sigSq
      errPow += errSq

  if errPow > 0:
    snr = 20 * log10(sigPow / errPow)
  else:
    snr = 'inf'
  print 'SNR: %sdb' % snr

  if kpp_file:
    kpp_file.close()
  if kppa_file:
    kppa_file.close()

