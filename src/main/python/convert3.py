#!/usr/share/anaconda3/bin/ipython

import sys
from skimage import io

def convert(fn_in):
  im = io.imread(fn_in)
  height = im.shape[0]
  width = im.shape[1]
  m = ''
  for i in range(0,height):
    for j in range(0,width):
      for c in range(0,3):
        m += str(im[i,j,c])
        if i == height - 1 and j == width - 1 and c == 2:
          pass
        else:
          m += ','
  f = open(fn_in + '.txt', 'w')
  f.write(m)
  f.write('\n')
  f.close()
  print(m)

if __name__ == '__main__':
  fn_in = sys.argv[1]
  convert(fn_in)
