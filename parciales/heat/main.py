import numpy as np
from math import pi, exp

numeric=0
NOfComponents=15

#Boundary conditions
T0 = 0
Tl = 0

t = 0.2
l = 1
m = 40
n = 100
h = l/n
k = t/m
alpha=1

# a= k/(c_p rho)
#{\displaystyle \alpha \equiv {\tfrac {k}{c_{p}\rho }}} is the thermal diffusivity, a material-specific quantity depending on the thermal conductivity {\displaystyle k}k, the specific heat capacity {\displaystyle c_{p}} c_p , and the mass density {\displaystyle \rho }\rho .

#Initial condition (t=0)
def initf(x):
    b=4
    #f = b*x*x-b*x
    #f = -b*x*x+b*x
    #f = (.25-(x-.5)**2)**0.5
    f=abs(x-.5)-0.5
    #f = np.sin(x*pi/l) + np.sin(2*x*pi/l)
    #f = -np.sin(2*x*pi/l) 
    #f = 0.5*np.sin(x*pi/l) + np.sin(2*x*pi/l)
    return f;

#print(initf(0))
#print(initf(l))
#print(initf(0.5))
#if (numeric == 0):

def simpson(number,step,points):
    x = np.linspace(0, step*points, points+1)
    y = initf(x)*np.sin(number*pi*x/(step*points))
   #print(initf(x))
    print(x)
   #print(y)
    int = (step/3)*(y[0] + 4*np.sum(y[1:-1:2]) + 2*np.sum(y[2:-1:2]) + y[-1])
    int = 2*int/l 
    return int;

def Coeff(nmax,step,points):
    return np.array([ simpson(k,step,points) for k in range(1, nmax+1) ])

Bn=Coeff(NOfComponents,h,n)
print(Bn)

#u(x,t) sum_0^n An cos npx/l e^n2pi2 alpha t/L + Bn sin npx/l e^n2pi2 alpha t/L

def heat(x,t,Bn,l):
    N = len(Bn) 
    sin_terms=np.array([Bn[n-1]*np.sin(n*pi*x/l)*exp(-n*n*pi*pi*alpha*t/(l*l)) for n in range(1,NOfComponents+1)])
    series = np.sum(sin_terms)
    return series

with open("analytic.dat", "w") as f:
   for j in range(0,m+1):
       for i in range(0,n+1):
           f.write(f"{i*h} {j*k} {heat(i*h,j*k,Bn,l)}\n")
       f.write(f" \n")
       f.write(f" \n")


#numeric
#else:

lam=alpha*alpha*k/(h*h)
w=np.zeros(n+1)
lm=np.zeros(n+1)
um=np.zeros(n+1)
zm=np.zeros(n+1)

for i in range(0,n+1):
    w[i]=initf(i*h)

with open("numeric.dat", "w") as g:
   for i in range(0,n+1):
       g.write(f"{i*h} {0} {w[i]}\n")
   g.write(f" \n")
   g.write(f" \n")

lm[1]=1+2*lam
um[1]=-lam/lm[1]

for i in range(2,n-1):
    lm[i]=1+2*lam+lam*um[i-1]
    um[i]=-lam/lm[i]

lm[n-1]=1+ 2*lam +lam*um[n-2]

with open("numeric.dat", "a") as g:
    for j in range(1,m+1):
        zm[1]=w[1]/lm[1]
        for i in range(2,n):
            zm[i]=(w[i]+lam*zm[i-1])/lm[i]
        w[n-1]=zm[n-1]
        for i in range(n-2,0,-1):
            w[i]=zm[i]-um[i]*w[i+1]

        for i in range(0,n+1):
               g.write(f"{i*h} {j*k} {w[i]}\n")
        g.write(f" \n")
        g.write(f" \n")




