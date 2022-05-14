from matplotlib import pyplot as plt
import numpy as np

y = np.array([1.0000000000000000,27.000000000000000,748.00000000000000,28374.999999999996,943656.00000000000,29070278.999999996,985194886.49999988,33872791094.999996,1099654541342.5000,35357439251992.000     ])

x = np.array(range(1, 11))
fig, ax = plt.subplots(figsize=(12, 8), dpi=100)
plt.xlabel('n')
plt.ylabel('ln10(cond)')
# plt.plot(x, y)
y = np.log10(y)
plt.plot(x, y)
# plt.show

plt.savefig('test3.png')