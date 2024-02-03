# VPN流量统计

执行顺序

将新导出的流量数据放入文件夹 'VPN-流量统计/input_csv_folder'

在终端窗口依次执行以下命令：

```bash
# 将数据文件汇总去重复并按时间顺序排序
python3 merge_csv.py  
# 提取不同节点的流量消耗情况，以每3个小时为时间单位统计
python3 traffic_sumary.py  
# 提取流量消耗超过10M的节点的数据
python3 sorted_traffice_summary.py 
```
可查看汇总之后的数据‘VPN-流量统计/sorted_traffic_summary.csv’

如要查看总流量的消耗变化图，可执行
```bash
python3 traffic_time.py   
```