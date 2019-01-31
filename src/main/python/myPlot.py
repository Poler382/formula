import numpy as np
import matplotlib.pyplot as plt
import matplotlib
import sys


file = open(sys.argv[1])
data = file.readlines()
xlabel = "epoch"

train_d = [float(i) for i in data[0].split(",")]
#test_d = [float(i) for i in data[3].split(",")]
ylabel = np.linspace(0,len(train_d),100)

plt.plot(train_d,label="train")
#plt.plot(test_d, label="test")

plt.xlabel(xlabel)
plt.ylabel(ylabel)

plt.legend()
plt.savefig("LOSSplot.png")
