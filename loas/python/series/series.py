import numpy as np
import matplotlib.pyplot as plt
import math

# Define the RLC circuit parameters
R = 1.0e-1 # resistance in ohms
L = 5.1e-6 # inductance in henries
C = 99.8e-6 # capacitance in farads
V0=5.14    # Initial voltage
I0=1.10   # Initial current in Amperes i=C dv/dt
alfa=R/(2*L)
omega=(C*L)**(-0.5)

if alfa == omega:
    print("crítico")
elif alfa > omega:
    print("sobreamortiguado")
else :
    print("subamortiguado")

# Define the differential equation to be solved
def f(t, y):
    return np.array([y[1], -y[0]/(L*C)-R*y[1]/L])

# Define the initial conditions
y0 = np.array([V0, I0/C]) # initial voltage and current

# Define the time step and the time array
h = 1e-6 # time step in seconds
t = np.arange(0, 1e-3, h) # time array from 0 to 0.02 seconds

# Implement the fourth-order Runge-Kutta method
def rk4(f, y0, t, h):
    n = len(t)
    y = np.zeros((n, len(y0)))
    y[0] = y0
    for i in range(n-1):
        k1 = h * f(t[i], y[i])
        k2 = h * f(t[i] + h/2, y[i] + k1/2)
        k3 = h * f(t[i] + h/2, y[i] + k2/2)
        k4 = h * f(t[i] + h, y[i] + k3)
        y[i+1] = y[i] + 1/6 * (k1 + 2*k2 + 2*k3 + k4)
    with open("cap_series.dat", "w") as dc:
        for i in range(0,n-1):
                    # time, voltage in cap, current in cap, step
            dc.write(f"{t[i]} {y[i,0]} {y[i,1]*C} {i}\n")

    with open("capv_test.dat", "w") as dc:
        for i in range(0,n-1):
                    # time, voltage in cap, current in cap, step
            dc.write(f"{t[i]} {y[i,0]}\n")

    with open("capi_test.dat", "w") as dc:
        for i in range(0,n-1):
            dc.write(f"{t[i]} {y[i,1]*C}\n")


    with open("res_series.dat", "w") as dc:
        for i in range(0,n-1):
                # time, voltage in cap, current in cap, step
            dc.write(f"{t[i]} {y[i,1]*C*R} {y[i,1]*C} {i}\n")

    with open("induc_series.dat", "w") as dc:
        for i in range(0,n-1):
                # time, voltage in cap, current in cap, step
            dc.write(f"{t[i]} {-y[i,0]+y[i,1]*C*R} {y[i,1]*C} {i}\n")
    return y

with open("series_parameters.dat", "w") as dc:
    dc.write(f"R= {R}\n L= {L}\n C= {C}\n V0= {V0}\n I0= {I0}\n alfa= {alfa}\n omega= {omega}\n ")

# Solve the differential equation using the fourth-order Runge-Kutta method
y = rk4(f, y0, t, h)

# Plot the voltage vs. time
plt.plot(t, y[:,0])
plt.plot(t, y[:,1]*C)
plt.xlabel('Time (s)')
plt.ylabel('Voltage (V)')
plt.title('Voltage vs. Time for an RLC Circuit')
plt.show()
