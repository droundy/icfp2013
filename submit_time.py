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
            #we can do many of these at once if we so desire
            startTime = time.time()
            currentProcess = subprocess.Popen(["./make_guess",size,problem,"time"] + problems_list)
            while (time.time() - startTime) < 4*60:
                if (currentProcess.poll() == 0):
                    with open("%s/%s/fast"%(dirname,problem),"w") as notefile:
                        pass
            try:
                currentProcess.kill()
                print "==== process did not finish =====+++++++++++++++"
                with open("%s/%s/slow"%(dirname,problem),"w") as notefile:
                    pass
            except:
                print "process finished"

if __name__ == '__main__':
    main()
