from z3 import *
import sys

x = Real("x")
s = Solver()
s.add(-4 * x + 9 == 1)
s.add(x / 10 < 3)
s.check()
m = s.model()
print(m[x].as_fraction(),is_rational_value(m[x]))

mother = m[x].denominator_as_long()
son = m[x].numerator_as_long()
