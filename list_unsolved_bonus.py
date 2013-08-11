#!/usr/bin/python
import subprocess
import os
import time
import sys

for size in [name for name in os.listdir('problems')]:
    dirname = "problems/%s" % size
    for problemid in [name for name in os.listdir('problems/%s'%(size))]:
            fastname = "%s/%s/fast"%(dirname,problemid)
            solvedname = "%s/%s/solved"%(dirname,problemid)
            if not os.path.exists(solvedname):
                if 'bonus' in open("%s/%s/operators" % (dirname,problemid)).read():
                    print "%s/%s"%(dirname,problemid)
