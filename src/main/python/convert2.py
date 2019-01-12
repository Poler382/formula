#!/usr/share/anaconda3/bin/ipython

import sys
from skimage import io

def convert(fn_in, fn_out):
  im = io.imread(fn_in)
  height = im.shape[0]
  width = im.shape[1]
  print(width,height)
  if height < width:
    width = height
  else:
    height = width
    
  print(width,height)
  
  f = open(fn_out, 'w')
  for i in range(0,height):
    for j in range(0,width):
      c = im[i,j]
      f.write(str(c))
      f.write(',')
    # if not (j == width - 1):
      #  f.write(',')

  f.write('\n')
  f.close()

if __name__ == '__main__':
  fn_in = sys.argv[1]
  fn_out = sys.argv[2]

  convert(fn_in,fn_out)
