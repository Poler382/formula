from mpl_toolkits.mplot3d import Axes3D
import matplotlib.pyplot as plt
import numpy as np
import sys

cs = ['red','green','blue','cyan','magenta','yellow','lime','orange','brown','mediumspringgreen']

fn = sys.argv[1]
f = open(fn).readlines()
ps0 = [x.strip().split(",") for x in f]
ps = [[int(p[0]),float(p[1]),float(p[2]),float(p[3])] for p in ps0]

fig = plt.figure()
ax = fig.add_subplot(111,projection = '3d')

xs = []
ys = []
zs = []
for p in ps:
    xs.append(p[1])
    ys.append(p[2])
    zs.append(p[3])

ax.scatter(xs, ys, zs)
plt.show()
plt.savefig(fn + ".png")


