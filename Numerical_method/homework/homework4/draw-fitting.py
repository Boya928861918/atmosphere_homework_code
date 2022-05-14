from matplotlib import pyplot as plt
import numpy as np
from sympy.parsing.sympy_parser import parse_expr
from sympy import plot_implicit

ezplot = lambda exper:plot_implicit(parse_expr(exper))


# plt.rcParams['font.sans-serif'] = ['SimHei']
# plt.rcParams['axes.unicode_minus'] = False

x0 = np.array([1.02, 0.95, 0.87, 0.77, 0.67, 0.56, 0.44, 0.30, 0.16, 0.01])
y0 = np.array([0.39, 0.32, 0.27, 0.22, 0.18, 0.15, 0.13, 0.12, 0.13, 0.15])
dx = np.array([-0.0029, 0.0007, -0.0082, -0.0038, -0.0041, 0.0026, -0.0001, -0.0058, -0.0005, -0.0034])
dy = np.array([-0.0033, 0.0043, 0.0006, 0.0020, 0.0044, 0.0009, 0.0028, 0.0034, 0.0059, 0.0024])
b = np.array([-0.432948977, 0.551704109, 3.22282386, 0.142011061, -2.63235450])
fig, ax = plt.subplots(figsize=(12, 8), dpi=100)
# expression = '{} + {}*x + {}*y + {}*x*y + {}*y**2 - x**2'.format(b[0], b[1], b[2], b[3], b[4])
# ezplot(expression)
x = np.linspace(-2, 2, 100)
y = np.linspace(0, 3, 100)
x, y = np.meshgrid(x, y)
plt.xlabel('x')
plt.ylabel('y')
ax.scatter(x0, y0, c='gray', marker='x', s=100, label='散点')
z = b[0]+b[1]*x+b[2]*y+b[3]*x*y+b[4]*y**2-x**2
plt.contour(x, y, z, 0)
x0 = x0 + dx
y0 = y0 + dy
b = np.array([-0.459761441, 0.653293550, 3.12924409, -0.519451380, -1.15078771])
z = b[0]+b[1]*x+b[2]*y+b[3]*x*y+b[4]*y**2-x**2
plt.contour(x, y, z, 0, colors = 'skyblue')

plt.savefig('test1.png')