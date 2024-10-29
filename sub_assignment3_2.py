import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import fsolve

# 定义拟合参数
b0 = -0.43289413556012252
b1 = 0.55144676229765699
b2 = 3.2229395560481544
b3 = 0.14364761640108795
b4 = -2.6356268180692859

# 定义观测数据点
x_obs = np.array([1.02, 0.95, 0.87, 0.77, 0.67, 0.56, 0.44, 0.30, 0.16, 0.01])
y_obs = np.array([0.39, 0.32, 0.27, 0.22, 0.18, 0.15, 0.13, 0.12, 0.13, 0.15])

# 定义函数，代入b0, b1, b2, b3, b4，方程形式： f(x, y) = b0 + b1 * x + b2 * y + b3 * x * y + b4 * y^2 - x^2
def equation(y, x):
    return b0 + b1 * x + b2 * y + b3 * x * y + b4 * y**2 - x**2

# 用fsolve解决二次方程，找到每个x对应的y值
def solve_for_y(x):
    # 以观测点为初始猜测值
    initial_guess = 0.1
    return fsolve(equation, initial_guess, args=(x))

# 为拟合曲线生成x值
x_fit = np.linspace(0.01, 1.2, 100)
y_fit = np.array([solve_for_y(x) for x in x_fit]).flatten()

# 创建图形
fig, ax = plt.subplots()

# 绘制拟合曲线
ax.plot(x_fit, y_fit, label='Fitted Curve', color='blue')

# 绘制观测点散点图，使用"x"标记
ax.scatter(x_obs, y_obs, color='red', label='Observed Points', marker='x')

# 设置图形标签和标题
ax.set_xlabel('x')
ax.set_ylabel('y')
ax.set_title('Fitted Curve of the Equation')
ax.legend()

# 显示图形
plt.show()
