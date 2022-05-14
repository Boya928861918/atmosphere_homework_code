import numpy as np

a = np.array([[11, -6, 4, -10, -4], [-3, 5, -2, 4,  1], [-8, 12, -3, 12, 4], [1, 6, -2, 3, -1], [8, -18, 8, -14, -1]])

b ,c = np.linalg.eig(a)
print(b, c)