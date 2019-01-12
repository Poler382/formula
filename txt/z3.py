from z3 import *
import sys
x = Real("x")
s = Solver()
s.add(-4 * X + 9 == 1)
s.add(X / 10 < 3)
s.check()
m = s.model()
print(m[x].as_fraction(),is_rational_value(m[x]))
mother = m[x].denominator_as_long()
son = m[x].numerator_as_long()
path = '/Users/yuya/Programing/sbt/formula/anstemp.txt'
f = open(path)
with open(path,mode='w') as f:
 f.write(m[x].as_fraction())
