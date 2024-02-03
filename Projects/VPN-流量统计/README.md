# VPN流量统计

执行顺序

将新导出的流量数据放入文件夹 '/input_csv_folder/'

在终端窗口依次执行以下命令：

```bash
# 将数据文件汇总去重复并按时间顺序排序
python3 merge_csv.py  
# 提取不同节点的流量消耗情况，以每3个小时为时间单位统计
python3 traffic_sumary.py  
# 提取流量消耗超过10M的节点的数据
python3 sorted_traffic_summary.py 
# 生成流量消耗的日报
python3 daily_traffic.py
```
可查看汇总之后的数据‘/sorted_traffic_summary.csv’

如要查看总流量的消耗变化图，可执行
```bash
python3 traffic_time.py   
```

## 待完善的功能

- [ ] 将节点的汇总结果根据流量消耗大小排序，在汇总表中呈现
- [ ] 设置流量监测的可视化方案，用户可以自行点选
  - [ ] 图片类型
  - [ ] 时间跨度
  - [ ] 时间间隔
- [ ] 设置流量预警功能