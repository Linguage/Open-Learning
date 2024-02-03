import pandas as pd
import csv
from collections import defaultdict
from datetime import datetime, timedelta

# 读取并整理数据
daily_traffic = defaultdict(lambda: defaultdict(float))  # 以天为键，存储每天每个节点的流量消耗

with open('merged_data_sorted.csv', 'r', newline='') as csvfile:
    reader = csv.reader(csvfile)
    next(reader)  # 跳过标题行
    for row in reader:
        node_name, rate, origin_traffic, traffic, log_time = row
        # 解析日期时间并提取日期部分
        log_date = datetime.strptime(log_time, '%Y-%m-%d %H:%M:%S').date()
        # 处理流量数据
        if traffic.endswith('GB'):
            traffic_mb = float(traffic[:-2]) * 1024
        elif traffic.endswith('MB'):
            traffic_mb = float(traffic[:-2])
        elif traffic.endswith('KB'):
            traffic_mb = float(traffic[:-2]) / 1024
        elif traffic.endswith('B'):
            traffic_mb = float(traffic[:-1]) / 1024 / 1024
        else:
            raise ValueError("Invalid traffic unit")

        # 累加每天每个节点的流量消耗
        daily_traffic[log_date][node_name] += traffic_mb

# 创建整合的数据框
columns = ['Date', 'Total Traffic(MB)']
for i in range(1, 6):
    columns.extend([f'Rank-{i}-Node', f'Traffic-{i}(MB)'])

merged_data = []

# 生成整合的数据
for date, node_traffic in daily_traffic.items():
    # 每一天的数据列表
    day_data = [date, round(sum(node_traffic.values()), 2)]

    # 获取流量前五的节点
    sorted_nodes = sorted(node_traffic.items(), key=lambda x: x[1], reverse=True)[:5]

    # 按照格式添加节点名称和流量消耗量
    for node, traffic in sorted_nodes:
        day_data.extend([node, round(traffic, 2)])

    # 添加到整合数据中
    merged_data.append(day_data)

# 将整合的数据写入 CSV 文件
with open('daily_traffic_summary.csv', 'w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(columns)  # 写入标题行
    writer.writerows(merged_data)  # 写入数据

print("Daily traffic summary has been saved to 'daily_traffic_summary.csv'")
