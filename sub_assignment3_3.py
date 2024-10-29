import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import fsolve

# 定义拟合参数
b0_1 = -0.43289413556012252
b1_1 = 0.55144676229765699
b2_1 = 3.2229395560481544
b3_1 = 0.14364761640108795
b4_1 = -2.6356268180692859

b0_2 = -0.459806353
b1_2 = 0.653319001
b2_2 = 3.12959719
b3_2 = -0.519694865
b4_2 = -1.15095520

# 定义观测数据点
x_obs = np.array([1.02, 0.95, 0.87, 0.77, 0.67, 0.56, 0.44, 0.30, 0.16, 0.01])
y_obs = np.array([0.39, 0.32, 0.27, 0.22, 0.18, 0.15, 0.13, 0.12, 0.13, 0.15])

# 定义函数，代入b0, b1, b2, b3, b4，方程形式
def equation(y, x, b0, b1, b2, b3, b4):
    return b0 + b1 * x + b2 * y + b3 * x * y + b4 * y**2 - x**2

# 用fsolve解决二次方程，找到每个x对应的y值
def solve_for_y(x, b0, b1, b2, b3, b4):
    initial_guess = 0.1
    return fsolve(equation, initial_guess, args=(x, b0, b1, b2, b3, b4))

# 为拟合曲线生成x值
x_fit = np.linspace(0.01, 1.2, 100)

# 计算两组参数的拟合曲线
y_fit_1 = np.array([solve_for_y(x, b0_1, b1_1, b2_1, b3_1, b4_1) for x in x_fit]).flatten()
y_fit_2 = np.array([solve_for_y(x, b0_2, b1_2, b2_2, b3_2, b4_2) for x in x_fit]).flatten()

# 创建图形
fig, ax = plt.subplots()

# 绘制拟合曲线
ax.plot(x_fit, y_fit_1, label='Fitted Curve 1 (r = 0.99988258580017786)', color='blue')
ax.plot(x_fit, y_fit_2, label='Fitted Curve 2 (r = 0.999781549)', color='orange')

# 绘制观测点散点图，使用"x"标记
ax.scatter(x_obs, y_obs, color='red', label='Observed Points', marker='x')

# 设置图形标签和标题
ax.set_xlabel('x')
ax.set_ylabel('y')
ax.set_title('Fitted Curves of the Equations')
ax.legend()

# 显示图形
plt.show()
