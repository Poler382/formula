import matplotlib.pyplot as plt
import matplotlib.cm as cm
import sys

plt.ion()
fig = plt.figure()

fn = sys.argv[1]
n = int(sys.argv[2])
x = []
for i in range(n):
  x.append([k for k in range(n)])

y = []
for i in range(n):
  y.append([n-i for k in range(n)])

z = []
for i in range(n):
  z.append([0 for k in range(n)])

f = open(fn,'r').readlines()
g = [float(x) for x in f[0].split(',')]
H = max(g)
L = min(g)
h = [int(100 * (float(x) - L) / (H - L)) for x in f[0].split(',')]
for i in range(n):
  for j in range(n):
    z[i][j] = h[i*n+j]

print(str(z))

plt.pcolormesh(x,y,z,cmap=cm.Blues)
plt.colorbar()
plt.savefig(fn+".png")

