import matplotlib.pyplot as plt
from sklearn import datasets
from sklearn.manifold import TSNE
import sys

digits = datasets.load_digits()

file = open(sys.argv[1])
data = file.readlines()
train = [float(i) for i in data[0].split(",")]


print (digits.data.shape)

x_reduced = TSNE(n_components = 2 , random_state = 0).fit_transform(digits.data)

plt.scatter(x_reduced[:,0],x_reduced[:,1],c = digits.target)
plt.colorbar()

plt.savefig("plot.png")
