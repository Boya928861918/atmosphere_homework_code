import matplotlib.pyplot as plt
import numpy as np
import sympy as sy

# 以x、y为坐标，mx和my是用homework2-Spline.f90程序计算出的x和y分别的二阶导数值，b是用homework2-fitting.f90计算出的最小二乘法的拟合参数b
b = np.array([-0.616701186, 3.74078155E-02, 6.40329456, 2.64396524, -13.3011847])
x0 = [0.99, 0.95, 0.87, 0.77, 0.67, 0.56, 0.44, 0.30, 0.16, 0.01, -0.2, -0.11, 0.05, 0.16, 0.29, 0.42, 0.59, 0.73, 0.85, 0.93, 0.99]
y0 = [0.39, 0.32, 0.27, 0.22, 0.18, 0.15, 0.13, 0.12, 0.13, 0.15, 0.22, 0.32, 0.36, 0.40, 0.41, 0.42, 0.43, 0.42, 0.41, 0.40, 0.39]
mx = np.array([-0.140131831, -1.78772118E-02, -2.83590928E-02, 1.13133304E-02, -1.68938730E-02, -3.73814115E-03, -2.81535052E-02, -3.64771765E-03,  4.27441970E-02, -0.227328926, 0.506571352, 1.04346313E-03, -9.07454938E-02, 6.19385280E-02, -3.70086394E-02, 8.60960335E-02, -6.73755333E-02, 3.40646319E-03, -6.62505552E-02, 2.15956252E-02, -0.140131831])
my = np.array([-9.81184021E-02, 5.93622401E-02, -1.93305090E-02, 1.79596134E-02, 7.49217113E-03, 1.20716458E-02, 4.22118977E-03, 3.10436692E-02, -8.39588884E-03, 6.25399649E-02, 5.82359433E-02, -0.115483731, 4.36989814E-02, -5.93123697E-02, 1.35504892E-02, 5.11040958E-03, -3.39919478E-02, 1.08571453E-02, -9.43645369E-03, 2.68886685E-02, -9.81184021E-02])
n = len(mx) - 1
len(mx)

# 创建底图
fig, ax = plt.subplots(figsize=(12, 8), dpi=100)

# 通过mx和my计算各个区间上的分段插值函数
X = sy.symbols('X')
xi = []
yi = []
xi_coeffs = []
yi_coeffs = []
for i in range(1, n + 1):
    # 因为采用了以t=1，2，3……为基底分别插值x和y,因此hi是常数1
    xhi = sy.simplify(mx[i-1] / 6 * (i+1-X)**3 + mx[i] / 6 * (X-i)**3 + (x0[i]-mx[i]/6) * (X-i) + (x0[i-1] - mx[i-1]/6) * (i+1-X))
    yhi = sy.simplify(my[i-1] / 6 * (i+1-X)**3 + my[i] / 6 * (X-i)**3 + (y0[i]-my[i]/6) * (X-i) + (y0[i-1] - my[i-1]/6) * (i+1-X))
    xhi = sy.Poly(xhi)
    yhi = sy.Poly(yhi)
    # 系数带入为多项式并存入xi中
    xi.append(np.poly1d(xhi.coeffs()))
    yi.append(np.poly1d(yhi.coeffs()))
    xi_coeffs.append(xhi.coeffs())
    yi_coeffs.append(yhi.coeffs())

# 标出各个坐标点
ax.scatter(x0, y0, marker='o', c='grey', label='points', s=30)

# 依次将S(x)绘制出来
for i in range(n):
    t = np.linspace(i + 1, i + 2, 100)
    plt.plot(xi[i](t), yi[i](t), color='black')

# 采用等高线的方式，将椭圆曲线方程绘制出来
x = np.linspace(-0.3, 1, 100)
y = np.linspace(0, 0.5, 100)
x, y = np.meshgrid(x, y)
plt.xlabel('x')
plt.ylabel('S(x)')
z = b[0]+b[1]*x+b[2]*y+b[3]*x*y+b[4]*y**2-x**2
plt.contour(x, y, z, 0)

# 出图参数设置
ax.grid()
plt.legend(loc='upper right')
plt.title('Spline and Fitting')
plt.savefig('homework2.png')
plt.show()
plt.close()
