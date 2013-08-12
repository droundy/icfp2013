#!/usr/bin/python
import os
import sys
import time

def main():
        size = sys.argv[1]
        problems_str = "problem"
        dirname = 'problems/%s' % (size)
        probIDs = [name for name in os.listdir(dirname)]
        for problem in probIDs:
            if os.path.exists(os.path.join(dirname, problem, "solved")):
                print "problem %s is already solved" % problem
            else:
                os.system("./submit_parallel %s %s program master"%(size, problem))
                #time.sleep(10)

if __name__ == '__main__':
    main()
        


