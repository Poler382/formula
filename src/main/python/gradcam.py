import matplotlib.cm as cm 
import matplotlib.pyplot as plt
import sys
path = sys.argv[1]
text = open(path).readlines()[0].split(",")
size = 26
x = [list(range(0, size)) for i in range(0, size)]
y = [[i]*size for i in range(size-1, -1, -1)]
z = [text[i*size:(i+1)*size] for i in range(0, size)]
z = [[float(a) for a in xs] for xs in z]

fig = plt.figure()
plt.pcolormesh(x,y,z,cmap=cm.Blues)
plt.colorbar()
plt.show()

