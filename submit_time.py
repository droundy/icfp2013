#!/usr/bin/python
import subprocess
import os
import time
import sys

def main():
    if "get" in sys.argv:
        n = int(sys.argv[2])
        size = sys.argv[3]
        i = 0
        while i<n:
            os.system("./get_training %s"%(size))
            i+=1
    if "time" in sys.argv:
        problems_list = ""
        size = sys.argv[2]
        if "problems" in sys.argv:
            dirname = "problems/%s"%size
            problems_list = ["problem"]
            probIDs = [name for name in os.listdir('problems/%s'%(size))]
        else:
            dirname = "trainings/%s"%size
            problems_list = []
            probIDs = [name for name in os.listdir('trainings/%s'%(size))]
        for problem in probIDs:
            fastname = "%s/%s/fast"%(dirname,problem)
            solvdname = "%s/%s/solved"%(dirname,problem)
            if not os.path.exists(fastname) and not os.path.exists(solvedname):
                #we can do many of these at once if we so desire
                startTime = time.time()
                currentProcess = subprocess.Popen(["./make_guess",size,problem,"time"] + problems_list)
                done = False
                while ((time.time() - startTime) < 5*60) and (not done):
                    if (currentProcess.poll() == 0):
                        with open("%s/%s/fast"%(dirname,problem),"w") as notefile:
                            pass
                        done = True
                    time.sleep(1)
                try:
                    currentProcess.kill()
                    print "==== process did not finish ++++++++++++++++++++++++++++++++++++++++++++++++++"
                    with open("%s/%s/slow"%(dirname,problem),"w") as notefile:
                        pass
                except:
                    print "==== process finished --------------------------------------------------"
        return 0

if __name__ == '__main__':
    main()
