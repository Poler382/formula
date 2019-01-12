
import sys
import matplotlib
from matplotlib.font_manager import FontProperties
import matplotlib.pyplot as plt

font_path = '/usr/share/fonts/ipa-pgothic/ipagp.ttf'

font_prop = FontProperties(fname=font_path)
matplotlib.rcParams['font.family'] = font_prop.get_name()


cs = ['red','green','blue','cyan','magenta','yellow','lime','orange','brown','mediumspringgreen']


fn = sys.argv[1]
f = open(fn).readlines()
ps0 = [x.strip().split(",") for x in f]
ps = [[int(p[0]),float(p[1]),float(p[2])] for p in ps0]

fig,ax = plt.subplots()
for i in range(10):
  xs = [p[1] for p in ps if p[0] == i]
  ys = [p[2] for p in ps if p[0] == i]
  ax.scatter(xs,ys,c=cs[i])

for p in ps:
  ax.annotate(str(p[0]),(p[1],p[2]), color=cs[p[0]])

plt.xlabel('$z_1$')
plt.ylabel('$z_2$')
plt.savefig(fn + ".png")

