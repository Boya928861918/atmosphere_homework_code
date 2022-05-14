from matplotlib import pyplot as plt
import numpy as np
from sympy.parsing.sympy_parser import parse_expr
from sympy import plot_implicit

ezplot = lambda exper:plot_implicit(parse_expr(exper))


# plt.rcParams['font.sans-serif'] = ['SimHei']
# plt.rcParams['axes.unicode_minus'] = False

x0 = np.array([0.99, 0.95, 0.87, 0.77, 0.67, 0.56, 0.44, 0.30, 0.16, 0.01, -0.2, -0.11, 0.05, 0.16, 0.29, 0.42, 0.59, 0.73, 0.85, 0.93])
y0 = np.array([0.39, 0.32, 0.27, 0.22, 0.18, 0.15, 0.13, 0.12, 0.13, 0.15, 0.22, 0.32, 0.36, 0.40, 0.41, 0.42, 0.43, 0.42, 0.41, 0.40])
b = np.array([-0.616701186, 3.74078155E-02, 6.40329456, 2.64396524, -13.3011847])
fig, ax = plt.subplots(figsize=(12, 8), dpi=100)
# expression = '{} + {}*x + {}*y + {}*x*y + {}*y**2 - x**2'.format(b[0], b[1], b[2], b[3], b[4])
# ezplot(expression)
x = np.linspace(-0.3, 1, 20)
y = np.linspace(-0.3, 0.6, 20)
x, y = np.meshgrid(x, y)
plt.xlabel('x')
plt.ylabel('S(x)')

z = b[0]+b[1]*x+b[2]*y+b[3]*x*y+b[4]*y**2-x**2
plt.contour(x, y, z, 0)

# plt.show
ax.scatter(x0, y0, c='gray', marker='x', s=100, label='散点')
plt.savefig('test1.png')