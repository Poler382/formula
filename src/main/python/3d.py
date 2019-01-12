import sys
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.axes3d import Axes3D
 
colors = ['red','blue','green','yellow','magenta','black','white','cyan',[0.5,0.5,0.5],[0.8,0.5,0.0]]
ds = [[[],[],[]] for i in range(10)]

fn = sys.argv[1]
lines = open(fn).readlines()
for line in lines:
  d = [float(i) for i in line.strip().split(',')]
  if len(d) == 4:
    n = int(d[0])
    for k in range(3):
      ds[n][k].append(d[k+1])
 
fig=plt.figure()
ax=Axes3D(fig)
 
for n in range(10):
  ax.scatter3D(ds[n][0], ds[n][1], ds[n][2], c=colors[n])

plt.show()
