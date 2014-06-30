#!/usr/bin/env python3

import os
import sys
import time

data_dir = './data'

for canonical_event in sys.stdin:
  t = time.gmtime()
  month_utc_dir = data_dir + '/' + str(t.tm_year) + '/' + str(t.tm_mon)
  if not os.path.isdir(month_utc_dir):
    try:
      os.makedirs(month_utc_dir)
    except OSError as exc:
      if exc.errno == errno.EEXIST and os.path.isdir(path): pass
      else: raise
  day_tsv_path = month_utc_dir + '/' + str(t.tm_mday) + '.tsv'
  with open(day_tsv_path, 'a') as f:
    f.write(canonical_event)
