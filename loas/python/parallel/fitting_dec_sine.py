import numpy as np
from scipy.optimize import curve_fit

# Define the decaying sine function
def decaying_sine(x, A, omega, phi, decay):
    return A * np.exp(-decay * x) * np.cos(omega * x + phi)

# Load the dataset
data = np.loadtxt('cap_test.dat')  # replace 'data.txt' with the actual filename

# Extract the x and y values from the dataset
x = data[:, 0]
y = data[:, 1]

# Guess initial values for the parameters
A_guess = np.max(y) - np.min(y)
#A_guess = 1.
omega_guess = 2 * np.pi / (x[-1] - x[0])
phi_guess = 0
decay_guess = 3.

# Fit the decaying sine function to the data
popt, pcov = curve_fit(decaying_sine, x, y, p0=[A_guess, omega_guess, phi_guess, decay_guess])

# Extract the fitted parameters
A_fit, omega_fit, phi_fit, decay_fit = popt

# Print the results
print(f"Frequency: {omega_fit / (2 * np.pi)}")
print(f"Frequency: {omega_fit }")
print(f"Decaying constant: {decay_fit}")
print(f"wd: {(omega_fit*omega_fit+ decay_fit*decay_fit )**0.5}")
print(f"A: {A_fit}: {2**0.5}")
print(f"phi: {phi_fit}: {np.pi/4}")
