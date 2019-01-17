import sys
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.axes3d import Axes3D
from sklearn.manifold import MDS, TSNE
from sklearn.decomposition import PCA
import matplotlib.cm as cm
 
def plot_data(data, labels, filename):
    plt.figure(figsize=(12,9))
    plt.scatter(data[:,0], data[:,1], c=["w" for _ in labels])
    for d, l in zip(data,labels):
        plt.text(d[0], d[1], str(l), fontdict={"size":12, "color":cm.Paired(l)})
    plt.show()
    plt.savefig(filename)
    return data

def pca(vectors,k):
  return PCA(n_components=k).fit_transform(vectors)

def tsne(vectors,k):
    tsne = TSNE()
    if(len(vectors[0]) > 15):
        vectors = PCA(n_components=15).fit_transform(vectors)

    for i in range(k):
      tsne.fit_transform(vectors)
    return tsne.fit_transform(vectors)

fn = sys.argv[1]
k = int(sys.argv[2])
data = []
target = []
i = 0
lines = open(fn).readlines()
for line in lines:
  d = [float(i) for i in line.strip().split(',')]
  if len(d) > 0:
    data.append(d[1:])
    target.append(int(d[0]))
    i += 1
 
#data = plot_data(pca(data,k), target, "{0}.png".format(fn))
pca(data,k)

f = open("{0}.csv".format(fn),'w')
for j in range(len(data)):
  d = data[j]
  f.write(str(target[j])+',')
  for i in range(k):
    f.write(str(d[i]))
    if i == k - 1:
      f.write('\n')
    else:
      f.write(',')
f.close()
