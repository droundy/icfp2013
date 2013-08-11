import sys
import os
import time

def main():
    if "get" in sys.argv:
        n = int(sys.argv[2])
        size = sys.argv[3]
        i = 0
        while i<n:
            os.system("./get_training %s"%(size))
            i+=1
    if "solve" in sys.argv:
        problems_str = ""
        size = sys.argv[2]
        if "problems" in sys.argv:
            problems_str = "problem"
            probIDs = [name for name in os.listdir('problems/%s'%(size))]
        else:
            problems_str = ""
            probIDs = [name for name in os.listdir('trainings/%s'%(size))]
        for problem in probIDs:
            init = time.time()
            os.system("./make_guess %s %s %s time"%(size, problem, problems_str))
            final = time.time()
            dt = final - init
            if dt > 240:
                with open("trainings/%s/%s/slow","w") as notefile:
                    pass

if __name__ == '__main__':
    main()
