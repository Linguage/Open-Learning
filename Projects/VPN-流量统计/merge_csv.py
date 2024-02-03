import os
import csv

# 指定输入和输出文件夹路径
input_folder = 'input_csv_folder'
output_file = 'merged_data_sorted.csv'

# 创建一个集合来存储数据
merged_data_unique = set()

# 遍历指定文件夹中的所有文件
for filename in os.listdir(input_folder):
    if filename.endswith('.csv'):
        file_path = os.path.join(input_folder, filename)
        # 读取每个 CSV 文件中的数据
        with open(file_path, 'r', newline='') as csvfile:
            reader = csv.reader(csvfile)
            # 跳过第一行（标题行）
            next(reader)
            # 将每行数据加入到集合中
            for row in reader:
                merged_data_unique.add(tuple(row))

# 将集合中的数据转换为列表
merged_data_list = list(merged_data_unique)

# 根据 'log_time' 列进行顺序排序
merged_data_sorted = sorted(merged_data_list, key=lambda x: x[4])

# 将整理后的数据写入新的 CSV 文件
with open(output_file, 'w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    # 写入标题行
    writer.writerow(['node_name', 'rate', 'origin_traffic', 'traffic', 'log_time'])
    # 逐行写入数据
    for row in merged_data_sorted:
        writer.writerow(row)

print(f"Sorted and unique merged data has been saved to '{output_file}'")
