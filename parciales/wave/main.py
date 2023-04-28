import numpy as np
l = 10
t = 10
m = 10
n = 10
h = l/(n+1)
k = t/(m)
#c = 3*10**8
c = 1
a = 1/c
lam = k*a/(h)

wave = np.empty((n+2,m+1))

#x = np.zeros
#t = []
#
#for i in range(0,n+3):
#    x[i]= i*h
#
#for i in range(0,m+1):
#    t[i]= i*k

def initf(x):
    if (x == 0 or x == l):
        f = 0
    elif (x <= 0.8*l):
        f = 1.25*x/l
    else:
        f = 5*(1-x/l)
    return f;

def initg(x):
    g = 0
    return g;

for i in range(m+1):
    wave[0,i]= 0
    wave[n+1,i]= 0

for i in range(1,n+1):
    wave[i,0]= initf(i*h)
    wave[i,1]= (1-lam*lam)*initf(i*h)+k*initg(i*h)+lam*lam*0.5*(initf((i+1)*h)+initf((i-1)*h))

for j in range(1, m):
    for i in range(1, n+1):
        wave[i,j+1]= 2*(1-lam*lam)*wave[i,j] +lam*lam*(wave[i+1,j]+wave[i-1,j])-wave[i,j-1]

#Output
with open("wave.dat", "w") as dc:
    for j in range(m+1):
        for i in range(n+2):
                # time, voltage in cap, current in cap, step
            dc.write(f"{j*k} {i*h} {wave[i,j]}\n")

        dc.write(f" \n")
        dc.write(f" \n")

