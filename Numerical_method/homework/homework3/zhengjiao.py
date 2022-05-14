from scipy import linalg
import numpy as np
A = np.array([0.16222142, 0.187666, 0.0543719, -0.39799566, 0.04201456], dtype=float)
Q = np.zeros_like(A)
m = Q.shape
cnt = 0
for a in A.T:
    u = np.copy(a)
    for i in range(0, cnt):
        u -= np.dot(np.dot(Q[i].T, a), Q[i]) # 减去待求向量在以求向量上的投影
    e = u / np.linalg.norm(u)  # 归一化
    Q[cnt] = e
    cnt += 1
print(Q)