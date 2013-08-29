import csv
from functools import partial
import gzip
#from itertools import product
import pickle
from sklearn.externals import joblib
from sklearn.svm import SVR
import sys
import time

# What to do? "angle", "yaw", or "speed"
task = sys.argv[1]

print "Task is %s." % task

# The files with training data and validation data sets.
trainfn = 'logs/torcs-driving-data-3.gz'
testfn = 'logs/torcs-driving-data-4.gz'

# Loads a data file. The mkdata and mktarget functions are supposed to create
# the input and output for a given example. The example is handed to mkdata and
# mktarget as dictionary with keys speedx[01] (m/s), speedx[01] (m/s), angle[01]
# (rad, positive value means car steers to the right), yaw[01] (rad, positive
# value means car steers to the left, does not take road angle into account),
# accel0 (range -1 to +1, -1 means full brake, 1 full acceleration), steer0
# (range -1 to +1, -1 means full right, +1 full left), and time (s). The values
# speed[xy]1, angle1, yaw1 refer to the car's state after accel0 and steer0
# take effect for time seconds.
def loaddata(mkdata, mktarget, fn):
        if fn.endswith('.gz'):
                o = gzip.open
        else:
                o = open
        with o(fn, 'rb') as csvfile:
                data = []
                target = []
                r = csv.reader(csvfile, delimiter='\t', quotechar='\\')
                for row in r:
                        numbers = map(lambda s: float(s.strip()), row)
                        example = {
                                        'speedx0' : numbers[0],
                                        'speedx1' : numbers[1],
                                        'speedy0' : numbers[2],
                                        'speedy1' : numbers[3],
                                        'angle0'  : numbers[4],
                                        'angle1'  : numbers[5],
                                        'yaw0'    : numbers[6],
                                        'yaw1'    : numbers[7],
                                        'gear0'   : numbers[8],
                                        'accel0'  : numbers[9],
                                        'brake0'  : numbers[10],
                                        'steer0'  : numbers[11],
                                        'time'    : numbers[12]
                                }
                        data.append(mkdata(example))
                        target.append(mktarget(example))
                return (data, target)

# Generates a lookup-table. The indexes should be either a list of tuples or of
# integers. In the former case the i-the element of the tuple stands for a
# range corresponding to the i-th dimension of the classifier. The former case
# is the special case of a one-dimensional tuple. Each tuple is handed to the
# third argument fun, which should convert the tuple, whose elements are
# integers, to the actual X values handed over to the classifier.
#
# Use lut2() for a single array literal LUT.
def lut(clf, indexes, fun):
        for index in indexes:
                if type(index) == tuple:
                        ilist = list(index)
                        x = list(fun(index))
                else:
                        ilist = [index]
                        x = [fun(index)]
                y = clf.predict(x)
                y = y[0]
                s = "lut"
                for i in ilist:
                        s += "["+str(i)+"]"
                s += " = "+str(y)+";"
                print s

# Generates a lookup-table as C array literal. The ranges parameter should be a
# list of ranges starting at zero. Their cartesian product is determined and
# element of it is given as tuple to the fun parameter. This function should
# compute a tuple which is then handed to the classifier clf for prediction.
# If there is only one range, the value is passed (not as tuple) to fun and
# fun should return a single value.
#
# The advantage over lut() is that this function returns a single literal.
# GHC/GCC is much faster with the literal.
def lut2(clf, ranges, fun):
	d = len(ranges)
	s = "static double lut"
	for r in ranges:
		s += "["+str(len(r))+"]"
        s += " = \n"
	s += lut_content(clf, ranges, [], fun)
        print s

def lut_content(clf, ranges, index, fun):
        if len(ranges) == 0:
                if len(index) == 1:
                        x = [fun(index[0])]
                else:
                        x = list(fun(tuple(index)))
                y = clf.predict(x)
                y = y[0]
                return str(y) +", "
        else:
                s = "{ "
                for i in ranges[0]:
                        indexcopy = list(index)
                        indexcopy.append(i)
                        s += lut_content(clf, ranges[1:], indexcopy, fun)
                s += " },\n"
                return s


# Effect of acceleration/brake:
# Input: speedx0, accel0
# Output: speedx1 - speedx0
def mkspeeddata(example):
        return [example['speedx0'], example['accel0'], example['brake0'], example['gear0']]
def mkspeedtarget(example):
        return example['speedx1'] - example['speedx0']

# Effect of steering:
# Input: steer0
# Output: angle1 - angle0
def mkangledata(example):
        return [example['steer0']]
def mkangletarget(example):
        return example['angle1'] - example['angle0']

# Effect of steering:
# Input: steer0
# Output: yaw1 - yaw0
def mkyawdata(example):
        return [example['steer0']]
def mkyawtarget(example):
        return example['yaw1'] - example['yaw0']

if task == "speed":
        load = partial(loaddata, mkspeeddata, mkspeedtarget)
elif task == "angle":
        load = partial(loaddata, mkangledata, mkangletarget)
elif task == "yaw":
        load = partial(loaddata, mkyawdata, mkyawtarget)

print "Loading training data ..."
(trainX, trainY) = load(trainfn)

print "Loading test data ..."
(testX, testY) = load(testfn)

if len(sys.argv) <= 2 or sys.argv[2] != "load":
        print "Training estimator (%d examples) ..." % len(trainX)
        clf = SVR(C=2.0, epsilon=0.01)
        clf.fit(trainX, trainY)

        print "Dumping estimator to file ..."
        joblib.dump(clf, task +".clf.pkl")
else:
        print "Loading estimator from file ..."
        clf = joblib.load(task +".clf.pkl")

print "Testing estimator on training data (%d examples) ..." % len(trainX)
err = 0
n = 0
for (x,y) in zip(trainX, trainY):
        p = clf.predict(x)
        err = err + abs(p-y)
        n = n + 1
err = err / n
print "Average error: ", err

print "Testing estimator on validation data (%d examples) ..." % len(testX)
err = 0
n = 0
start = time.clock()
for (x,y) in zip(testX, testY):
        p = clf.predict(x)
        err = err + abs(p-y)
        n = n + 1
elapsed = time.clock() - start
err = err / n
print "Average error: ", err
print "Elapsed: ", elapsed
print "Elapsed per example: ", (elapsed/n)

# if task == "speed":
#         lut2(clf, [range(300), range(21)], (lambda (x,y): (float(x)/3.6, float(y)/10-1)))
#         #lut(clf, product(range(300), range(21)), (lambda (x,y): (float(x)/3.6, float(y)/10-1)))
# elif task == "angle":
#         lut2(clf, [range(201)], (lambda x: float(x)/100-1))
#         #lut(clf, range(201), (lambda x: float(x)/100-1))
# # elif task == "yaw":
#         lut2(clf, [range(201)], (lambda x: float(x)/100-1))
#         #lut(clf, range(201), (lambda x: float(x)/100-1))
# 
