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

# 定义图例和颜色
labels = ['LS_grass', 'DX_urban']
colors = ['blue', 'green']

# 画图函数
def plot_radiation(x_label, y1, y2, ylabel, title):
    plt.figure(figsize=(10, 6))
    plt.plot(lishui_selected['Date_Time'], y1, label=labels[0], color=colors[0])
    plt.plot(dx_selected['T(local)'], y2, label=labels[1], color=colors[1])
    plt.xlabel('Date')
    plt.ylabel(ylabel)
    plt.title(title)
    plt.legend()
    plt.grid(True)
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.show()

# 绘制DLR, DSR, ULR, USR折线图
plot_radiation('Date', lishui_selected['DL(W/m2)'], dx_selected['DLR_26.5m'], 'DLR (W/m^2)', 'Downward Longwave Radiation (DLR)')
plot_radiation('Date', lishui_selected['DS(W/m2)'], dx_selected['DR_26.5m'], 'DSR (W/m^2)', 'Downward Shortwave Radiation (DSR)')
plot_radiation('Date', lishui_selected['UL(W/m2)'], dx_selected['ULR_26.5m'], 'ULR (W/m^2)', 'Upward Longwave Radiation (ULR)')
plot_radiation('Date', lishui_selected['US(W/m2)'], dx_selected['UR_26.5m'], 'USR (W/m^2)', 'Upward Shortwave Radiation (USR)')
