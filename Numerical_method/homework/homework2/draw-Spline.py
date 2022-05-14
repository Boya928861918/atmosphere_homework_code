from matplotlib import pyplot as plt
import numpy as np
from mpl_toolkits.mplot3d import Axes3D

plt.rcParams['font.sans-serif'] = ['SimHei']
plt.rcParams['axes.unicode_minus'] = False

x0 = np.array([0.99, 0.95, 0.87, 0.77, 0.67, 0.56, 0.44, 0.30, 0.16, 0.01, -0.2, -0.11, 0.05, 0.16, 0.29, 0.42, 0.59, 0.73, 0.85, 0.93])
y0 = np.array([0.39, 0.32, 0.27, 0.22, 0.18, 0.15, 0.13, 0.12, 0.13, 0.15, 0.22, 0.32, 0.36, 0.40, 0.41, 0.42, 0.43, 0.42, 0.41, 0.40])
m = np.array([132.844467    ,    5.81807995   ,    0.498422235     ,   1.05121076    ,    1.29673004     ,   1.03535664   ,    0.145488381     ,   2.65380263   ,    -4.63825035      ,  17.9338799   ,    -52.4602661    ,   -1.75204337      ,  2.69261527    ,   -4.47153187   ,    0.998984575     ,  0.475593925     ,  -3.08129692      ,  7.48599291   ,    -29.4396877    ,   132.844467])
h = []

for t in range(0, 19):
    h.append(x0[t+1]-x0[t])
h = np.array(h)
def draw(x0_, y0_, m_, i_, h_):
    x_ = np.linspace(x0_[i_-1], x0_[i_], 5)
    # y_ = (x0_[i_]-x_)**3*m_[i_-1]/(6*h_[i_])+(x_-x0_[i_-1])**3*m_[i_]/(6*h_[i_])+(y0_[i_-1]-h_[i_]**2*m_[i_-1]/6)*(x_[i_]-x_)/h_[i_]+(y0_[i_]-h_[i_]**2*m_[i_]/6)*(x_-x_[i_-1])/h_[i_]
    y_ = m_[i-1]*(x0_[i_]-x_)**3/(6*h_[i_])+m_[i_]*(x_-x0_[i_-1])**3/(6*h_[i_])+(y0_[i_]/h_[i_]-h_[i_]*m_[i_]/6)*(x_-x0_[i_-1])+(y0_[i-1]/h_[i_]-h_[i_]*m_[i_-1]/6)*(x0_[i_]-x_)
    plt.xlabel('x')
    plt.ylabel('S(x)')
    plt.plot(x_, y_, marker = 'o')
    
    plt.show

for i in range(0, 19):
    draw(x0, y0, m, i, h)
plt.plot(x0, y0)
plt.savefig('test.png')