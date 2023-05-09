import numpy as np
from scipy.optimize import curve_fit
import matplotlib.pyplot as plt

# Define the decaying sine function
def func(x, A, B, e1, e2):
    return A * np.exp(-e1 * x) + B * np.exp(-e2 * x) 

# Load the dataset
data = np.loadtxt('capv_test.dat')  # replace 'data.txt' with the actual filename

# Extract the x and y values from the dataset
x = data[:, 0]
y = data[:, 1]

# Guess initial values for the parameters
A_guess = np.max(y) - np.min(y)
B_guess = np.max(y) - np.min(y)
e1_guess = 2 * np.pi 
e2_guess = 2 * np.pi

# Fit the decaying sine function to the data
popt, pcov = curve_fit(func, x, y, p0=[A_guess, B_guess, e1_guess, e2_guess])

# Extract the fitted parameters
A_fit, B_fit, e1_fit, e2_fit = popt

with open("parallel_parameter.dat", "w") as dc:
    dc.write(f"A= {A_fit}\n B= {B_fit}\n e1= {e1_fit}\n e2= {e2_fit}\n")

# Print the results
print(f"A: {A_fit}")
print(f"B: {B_fit}")
print(f"e1: {e1_fit}")
print(f"e2: {e2_fit}")
plt.plot(x, y)
plt.plot(x, func(x, A_fit, B_fit, e1_fit, e2_fit), linestyle='--')
plt.show()
