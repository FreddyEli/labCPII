from scipy.optimize import fsolve
import numpy as np

R= 0.1
L= 5.1e-06
C= 9.98e-05
V0= 5.14
I0= 1.1
alfa= 1/(2*R*C)
omega= (L*C)**(-0.5)
s1= -alfa + (alfa*alfa-omega*omega)**0.5
s2= -alfa - (alfa*alfa-omega*omega)**0.5
wd= (omega*omega-alfa*alfa)**0.5

def equations(variables):
    x, y = variables
    eq1 = x + y - I0
    eq2 = (s1*x + s2*y)*L - V0
    return [eq1, eq2]

x, y = fsolve(equations, (1, 1))

print("tA =", x)
print("tB=", y)

with open("paras.dat", "w") as dc:
        dc.write(f"ts1 = {s1}\n")
        dc.write(f"ts2 = {s2}\n")
        dc.write(f"tA = {x}\n")
        dc.write(f"tB = {y}\n")
