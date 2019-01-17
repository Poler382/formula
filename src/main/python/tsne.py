# coding: UTF-8
import sys
from sklearn.datasets import load_digits, load_iris
from sklearn.manifold import MDS, TSNE
from sklearn.decomposition import PCA
from matplotlib import pyplot as plt
import matplotlib.cm as cm

def plot_data(data, labels, filename):
    plt.figure(figsize=(12,9))
    plt.scatter(data[:,0], data[:,1], c=["w" for _ in labels])
    for d, l in zip(data,labels):
        plt.text(d[0], d[1], str(l), fontdict={"size":12, "color":cm.Paired(l)})
    plt.savefig(filename)

def pca(vectors):
    pca = PCA(n_components=2)
    return pca.fit_transform(vectors)

def tsne(vectors):
    tsne = TSNE()
    if(vectors.shape[1] > 15):
        vectors = PCA(n_components=15).fit_transform(vectors)
    return tsne.fit_transform(vectors)

def main():
    iris = load_iris()
    digits = load_digits()
    data =
    for data, dname in zip([iris, digits], ["iris", "digits"]):
        plot_data(pca(data.data), data.target, "{0}_pca.png".format(dname))
        plot_data(tsne(data.data), data.target, "{0}_tsne.png".format(dname))

if __name__ == "__main__":
    main()