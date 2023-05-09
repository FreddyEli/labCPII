from scipy.optimize import fsolve
import numpy as np

R= 0.1
L= 5.1e-06
C= 9.98e-05
V0= 5.14
I0= 1.1
alfa=R/(2*L)
omega=(C*L)**(-0.5)
wd= (omega*omega-alfa*alfa)**0.5

def equations(variables):
    x, y = variables
    eq1 = x  * np.sin(y) - V0
    eq2 = x*(wd*np.cos(y)-alfa*np.sin(y))*C - I0
    return [eq1, eq2]

x, y = fsolve(equations, (1, 1))

print("tA =", x)
print("tphi =", y)
print("tphi =", x*C*(wd*np.cos(y)-alfa*np.sin(y)))

with open("paras.dat", "w") as dc:
        dc.write(f"tA = {x}\n")
        dc.write(f"tphi = {y}\n")
