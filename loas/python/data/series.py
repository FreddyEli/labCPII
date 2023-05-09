import numpy as np
import matplotlib.pyplot as plt

R= 0.1
L= 5.1e-06
C= 99.8e-6
V0= 5.14
I0= 1.10
alfa= 9803.921568627451
omega= 44325.09155329412

# Define the decaying sine function
def decaying_sine(x, A, omega, phi, decay):
    return A * np.exp(-decay * x) * np.sin(omega * x + phi)

h = 1e-6 # time step in seconds
x = np.arange(0, 1e-3, h) # time array from 0 to 0.02 seconds
#y = decaying_sine(x, 5, 12, 0, 8) + np.random.normal(0, 0.1, size=100)

def decaying_noise(x, func, scale, decay_rate, dx):
    noise = []
    for i in range(len(x)):
        if (dx==1):
            mag = abs(func(x[i],23.59, omega,-3.1883, alfa))
        else:
            mag = abs(func(x[i],1, 43181,-2, 9804))
        #noise_scale = scale*np.exp(-decay_rate*x[i])
        #noise_val = noise_scale*np.random.randn()
        noise_val = scale*mag*np.random.randn()
        noise.append(noise_val)
    return noise

noise = decaying_noise(x, decaying_sine, 0.7, 4000.5, 0 )

# Add decaying noise to the function
y = decaying_sine(x,-5.333, omega,-1.8405, alfa) + noise
yhat = decaying_sine(x,-5.333, omega,-1.8405, alfa)

dy = (decaying_sine(x,23.6, omega,-3.4, alfa) + decaying_noise(x, decaying_sine, 0.12, omega , 1) )
dyhat = decaying_sine(x,23.6, omega,-3.4, alfa)

with open('outputv_series', 'w') as f:
    for i in range(len(x)):
        f.write(f"{x[i]} {y[i]} {0.01*y[i]}\n")

with open('outputl_series', 'w') as f:
    for i in range(len(x)):
        f.write(f"{x[i]} {dy[i]} {0.01*dy[i]}\n")

#plt.scatter(x,y)
##plt.scatter(x,noise)
#plt.plot(x,yhat)
plt.scatter(x,dy)
plt.plot(x,dyhat)
plt.show()
