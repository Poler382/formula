#!/usr/share/anaconda3/bin/ipython

import sys
from skimage import io

def convert(fn_in, fn_out):
  im = io.imread(fn_in)
  height = im.shape[0]
  width = im.shape[1]
  f = open(fn_out, 'w')
  for i in range(0,height):
    for j in range(0,width):
      for k in range(0,3):
        c = im[i,j,k]
        f.write(str(c))
        if not (j == width - 1 and k == 2):
          f.write(',')
    f.write('\n')
  f.close()

if __name__ == '__main__':
  fn_in = sys.argv[1]
  fn_out = sys.argv[2]
  convert(fn_in,fn_out)
