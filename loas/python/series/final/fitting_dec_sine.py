import numpy as np
from scipy.optimize import curve_fit

# Define the decaying sine function
def decaying_sine(x, A, omega, phi, decay):
    return A * np.exp(-decay * x) * np.sin(omega * x + phi)

# Load the dataset
data = np.loadtxt('capi_test.dat')  # replace 'data.txt' with the actual filename

# Extract the x and y values from the dataset
x = data[:, 0]
y = data[:, 1]

# Guess initial values for the parameters
A_guess = np.max(y) - np.min(y)
omega_guess = 2 * np.pi / (x[-1] - x[0])
phi_guess = 0
decay_guess = 0.1

# Fit the decaying sine function to the data
popt, pcov = curve_fit(decaying_sine, x, y, p0=[A_guess, omega_guess, phi_guess, decay_guess])

# Extract the fitted parameters
A_fit, omega_fit, phi_fit, decay_fit = popt


with open("series_parameters.dat", "a") as dc:
    dc.write(f"Current:\n")
    dc.write(f"A_fit= {A_fit}\n omega= {omega_fit}\n phi= {phi_fit}\n decay= {decay_fit}\n ")
    dc.write(f"\n")

# Print the results
print(f"{A_fit} {omega_fit} {phi_fit} {decay_fit}")
print(f"Frequency: {omega_fit / (2 * np.pi)}")
print(f"Decaying constant: {decay_fit}")
print(f"wd: {(omega_fit*omega_fit+ decay_fit*decay_fit )**0.5}")
