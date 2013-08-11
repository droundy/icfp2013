#!/usr/bin/python
import os
import sys
import time

#

size = sys.argv[1]
dirname = 'problems/%s' % (size)
probIDs = [name for name in os.listdir(dirname)]
os.system("./build-all.sh")
for problem in probIDs:
  if os.path.exists(os.path.join(dirname, problem, "solved")):
    print "problem %s is already solved" % problem
  else:
    os.system("sbatch -c4 batch_submit.sh %s %s" %(size, problem))
    #time.sleep(10)


