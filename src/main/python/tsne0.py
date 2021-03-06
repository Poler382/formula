# coding: UTF-8
import sys
from sklearn.datasets import load_digits, load_iris
from sklearn.manifold import MDS, TSNE
from sklearn.decomposition import PCA
from matplotlib import pyplot as plt
import matplotlib.cm as cm
import numpy as np

def plot_data(data, labels, filename,title):
    plt.figure(figsize=(12,9))
    plt.title(title)
    plt.xlabel('x')
    plt.ylabel('y')
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
    #　入力ファイル名　入力ベクトル　出力ファイル名　出力ファイルにつけるラベル タイトル
    args = sys.argv
    fn = args[1]
    print(fn)
    input_vecsize = int(args[2])
    outputfn  = args[3]
    targetfn  = args[4] # targetとなるlabelを一行に書いてあるファイルを読む
    title     = args[5]
    #    fn = "/Users/yuya/SourceTree/sbt/formula/Emvedding/SkipGram_formula_20x10.txt"

    lines = open(fn).readlines()
    ps0 = [x.strip().split(",") for x in lines]
    ps0[0].pop(-1)
    inputExercises_size = int(len(ps0[0]) /  input_vecsize)
    data = np.array(ps0).reshape(inputExercises_size,input_vecsize)

    labellines = open(targetfn).readlines()
    ps1 = [x.strip().split(",") for x in labellines]

    target =np.array(ps1[0])

    plot_data( pca(data),target[:inputExercises_size], "png/pca_"+outputfn+".png","pca "+title)
    plot_data(tsne(data),target[:inputExercises_size],"png/tsne_"+outputfn+".png","Tsne "+title)



if __name__ == "__main__":
    main()
