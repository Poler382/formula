# coding: UTF-8
import sys
from sklearn.datasets import load_digits, load_iris
from sklearn.manifold import MDS, TSNE
from sklearn.decomposition import PCA
from matplotlib import pyplot as plt
import matplotlib.cm as cm
import numpy as np

def plot_data(data, labels, filename):
    plt.figure(figsize=(12,9))
    plt.title(filename)
    plt.xlabel('epoch')
    plt.ylabel('frecency')
    plt.scatter(data[:,0], data[:,1], c=["w" for _ in labels])

    for d, l in zip(data,labels):
        plt.text(d[0], d[1], l, fontdict={"size":12}) #, "color":cm.Paired(l)

    #plt.legend()
    plt.savefig(filename)

def pca(vectors):
    pca = PCA(n_components=2)
    return pca.fit_transform(vectors)

def tsne(vectors):
    tsne = TSNE(perplexity=50,n_iter=1000)
    if(vectors.shape[1] > 15):
        vectors = PCA(n_components=15).fit_transform(vectors)
    return tsne.fit_transform(vectors)

def main():
    args = sys.argv
    fn = args[1]
    print(fn)
    innum = int(args[2])
    outnum = int(args[3])
    outputfn = args[4]
#    fn = "/Users/yuya/SourceTree/sbt/formula/Emvedding/SkipGram_formula_20x10.txt"
    lines = open(fn).readlines()
    ps0 = [x.strip().split(",") for x in lines]
    #data = np.array(ps0).reshape(20,10)
    #target = np.arange(innum)
    data = np.array(ps0).reshape(innum,outnum)
    moto = ["f","{","}","(",")","x","=","+","-","n","s"]
    target =np.array(["(",")","+","-","0","0.01","0.03","0.06","0.1","0.11","0.18","0.19","0.2","0.3","0.31","0.38","0.4","0.5","0.6","0.61","0.67","0.7","0.8","0.85","0.98","1","1.1","1.12","1.15","1.2","1.3","1.4","1.5","1.6","1.7","1.8","1.9","10","11","12","13","14","15","16","17","18","19","2","2.2","2.3","2.7","20","21","23","24","25","27","28","3","3.3","3.6","32","34","36","37","4","4.2","4.6","42","43","48","5","52","54","56","6","67","7","7.2","72","74","8","9","92","=","f","x","{","}"])
    #target = np.array(innum)

    plot_data(pca(data),target, "png/pca_"+outputfn+".png")
    plot_data(tsne(data),target,"png/tsne_"+outputfn+".png")



if __name__ == "__main__":
    main()
