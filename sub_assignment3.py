import numpy as np
import matplotlib.pyplot as plt

# 读取拟合参数
params = None
with open('fitted_results.txt', 'r') as f:
    line = f.readline().strip()
    if "Fitted parameters:" in line:
        # 提取拟合参数并转换为浮点数数组
        params = np.array([float(x) for x in line.split()[2:]])

# 如果没有找到拟合参数，报错提示
if params is None:
    raise ValueError("拟合参数未找到或文件格式不正确")
print(params)

# 读取观测数据
observed_data = np.loadtxt('observed_data.txt')

x_obs = observed_data[:, 0]
y_obs = observed_data[:, 1]

# 定义拟合方程： b0 + b1*x + b2*y + b3*x*y + b4*(y^2) = x^2
def fitted_ellipse(x, y, params):
    b0, b1, b2, b3, b4 = params
    return np.sqrt(b0 + b1*x + b2*y + b3*x*y + b4*y**2)

# 在范围内生成拟合的椭圆轨道
x_fit = np.linspace(min(x_obs), max(x_obs), 100)
# 为了匹配 x_fit 的大小，我们可以用插值扩展 y_obs
y_fit_interp = np.interp(x_fit, x_obs, y_obs)

# 计算拟合的椭圆轨道
y_fit = fitted_ellipse(x_fit, y_fit_interp, params)

# 绘制图形
plt.figure()
plt.plot(x_fit, y_fit, label='Fitted Ellipse', color='blue')  # 拟合曲线
plt.scatter(x_obs, y_obs, color='red', label='Observed Data', marker='x')  # 观测数据
plt.xlabel('X')
plt.ylabel('Y')
plt.legend()
plt.title('Planet Orbit Fit')

# 显示图形
plt.show()
