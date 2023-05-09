import numpy as np
import matplotlib.pyplot as plt

R= 0.1
L= 5.1e-06
C= 99.8e-6
V0= 5.14
I0= 1.10
alfa= 1/(2*R*C)
omega= (L*C)**(-0.5)
s1= -alfa + (alfa*alfa-omega*omega)**0.5
s2= -alfa - (alfa*alfa-omega*omega)**0.5
tA = 23.309355381613184
tB = -22.209355381613182
L= 5.1e-06

def current_parllel(x):
    return tA * np.exp(s1 * x) + B * np.exp(e2 * x) 

def voltage_parllel(x):
    return tA *s1 np.exp(s1 * x) + tB * s2 * np.exp(s2 * x) 

h = 1e-6 # time step in seconds
x = np.arange(0, 1e-3, h) # time array from 0 to 0.02 seconds
#y = decaying_sine(x, 5, 12, 0, 8) + np.random.normal(0, 0.1, size=100)

def decaying_noise(x, func, scale, decay_rate):
    noise = []
    for i in range(len(x)):
        mag = abs(func(x[i],A_fit, B_fit, e1_fit, e2_fit))
        #noise_scale = scale*np.exp(-decay_rate*x[i])
        #noise_val = noise_scale*np.random.randn()
        noise_val = scale*mag*np.random.randn()
        noise.append(noise_val)
    return noise

noise = decaying_noise(x, fun_test, 0.2, 4000.5)

# Add decaying noise to the function

y = current_parllel(x) + noise

yhat = fun_test(x,A_fit, B_fit, e1_fit, e2_fit)

dy = (fun_test(x,23.6, omega,-3.4, alfa) + decaying_noise(x, decaying_sine, 0.12, omega , 1) )
dyhat = decaying_sine(x,23.6, omega,-3.4, alfa)

with open('outputi_parallel', 'w') as f:
    for i in range(len(x)):
        f.write(f"{x[i]} {y[i]} {0.01*y[i]}\n")

with open('outputv_parallel', 'w') as f:
    for i in range(len(x)):
        f.write(f"{x[i]} {y[i]} {0.01*y[i]}\n")

with open('outputv_series', 'w') as f:
    for i in range(len(x)):
        f.write(f"{x[i]} {y[i]} {0.01*y[i]}\n")

with open('outputl_series', 'w') as f:
    for i in range(len(x)):
        f.write(f"{x[i]} {dy[i]} {0.01*dy[i]}\n")

plt.scatter(x,y)
plt.scatter(x,noise)
plt.plot(x,yhat)
plt.show()
