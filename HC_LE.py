import pandas as pd
import matplotlib.pyplot as plt

# 读取数据
lishui_data = pd.read_excel(r'C:\Users\Chen Yong\Desktop\作业3所需数据\Lishui_grass_2013.xls')
dx_data = pd.read_excel(r'C:\Users\Chen Yong\Desktop\作业3所需数据\党校2013-26m.xlsx')

# 将 Lishui_grass_2013.xls 中的时间格式化为日期时间类型
lishui_data['Date_Time'] = pd.to_datetime(lishui_data['Date_Time'], format='%Y/%m/%d %H:%M', errors='coerce')

# 将 党校2013-26m.xlsx 中的时间格式化为日期时间类型，并处理可能存在的格式不一致问题
dx_data['T(local)'] = pd.to_datetime(dx_data['T(local)'], infer_datetime_format=True, errors='coerce')

# 检查是否有无法解析的时间
print("Lishui data invalid dates:", lishui_data['Date_Time'].isna().sum())
print("DX Urban data invalid dates:", dx_data['T(local)'].isna().sum())

# 筛选 2013.7.8 0:00 到 2013.7.15 23:30 的数据
start_date = '2013-07-08 00:00:00'
end_date = '2013-07-15 23:30:00'

lishui_selected = lishui_data[(lishui_data['Date_Time'] >= start_date) & (lishui_data['Date_Time'] <= end_date)]
dx_selected = dx_data[(dx_data['T(local)'] >= start_date) & (dx_data['T(local)'] <= end_date)]

# 检查筛选结果
print("Lishui Data selected rows:", lishui_selected.shape)
print("DX Urban Data selected rows:", dx_selected.shape)

# 定义函数来剔除异常值
def remove_outliers(data, column):
    mean = data[column].mean()
    std_dev = data[column].std()
    lower_limit = mean - 2 * std_dev
    upper_limit = mean + 2 * std_dev
    return data[(data[column] >= lower_limit) & (data[column] <= upper_limit)]

# 去除 Lishui 和 DX 数据中的异常值
lishui_filtered = remove_outliers(lishui_selected, 'Hc(W/m2)')
dx_filtered = remove_outliers(dx_selected, 'Hs_26.5m')

# 检查去除异常值后的结果
print("Filtered Lishui Data rows:", lishui_filtered.shape)
print("Filtered DX Urban Data rows:", dx_filtered.shape)

# 定义图例和颜色
labels = ['LS_grass', 'DX_urban']
colors = ['blue', 'green']

# 画图函数
def plot_radiation(y1, y2, ylabel, title):
    plt.figure(figsize=(10, 6))
    plt.plot(lishui_filtered['Date_Time'], y1, label=labels[0], color=colors[0])
    plt.plot(dx_filtered['T(local)'], y2, label=labels[1], color=colors[1])
    plt.xlabel('Date')
    plt.ylabel(ylabel)
    plt.title(title)
    plt.legend()
    plt.grid(True)
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.show()

# 绘制DLR, DSR, ULR, USR折线图
plot_radiation(lishui_filtered['Hc(W/m2)'], dx_filtered['Hs_26.5m'], 'Hc(W/m^2)', 'Sensible Heat Flux')
plot_radiation(lishui_filtered['LE_wpl(W/m2)'], dx_filtered['Le_26.5m'], 'Le(W/m^2)', 'Latent Heat Flux')
