from matplotlib import pyplot as plt
import matplotlib
import numpy as np
from scipy import interpolate

x0 = [0.99, 0.95, 0.87, 0.77, 0.67, 0.56, 0.44, 0.30, 0.16, 0.01, -0.2, -0.11, 0.05, 0.16, 0.29, 0.42, 0.59, 0.73, 0.85, 0.93]
y0 = [0.39, 0.32, 0.27, 0.22, 0.18, 0.15, 0.13, 0.12, 0.13, 0.15, 0.22, 0.32, 0.36, 0.40, 0.41, 0.42, 0.43, 0.42, 0.41, 0.40]

tck = interpolate.splrep(x0, y0)
xx = np.linspace(min(x0), max(x0), 100)
yy = interpolate.splev(xx, tck, der=0)
print(xx)
