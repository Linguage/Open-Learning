import csv
from collections import defaultdict
from datetime import datetime, timedelta
import operator

# Read the traffic summary CSV file
traffic_summary = []
with open('traffic_summary.csv', newline='') as csvfile:
    reader = csv.reader(csvfile)
    for row in reader:
        traffic_summary.append(row)

# Calculate total traffic for each node
node_traffic_totals = defaultdict(float)
for row in traffic_summary[1:]:
    for i, traffic in enumerate(row[1:], start=1):
        node_traffic_totals[traffic_summary[0][i]] += float(traffic)

# Insert a new row after the first row with total traffic for each node
total_traffic_row = ['Total Traffic'] + [str(node_traffic_totals[node]) for node in traffic_summary[0][1:]]
traffic_summary.insert(1, total_traffic_row)

# Filter out nodes with total traffic less than 10MB
filtered_nodes = [node for node, total_traffic in node_traffic_totals.items() if total_traffic >= 10]

# Rearrange traffic summary based on filtered nodes
sorted_traffic_summary = [traffic_summary[0]]
for node in filtered_nodes:
    for row in traffic_summary[2:]:
        if row[0] == node:
            sorted_traffic_summary.append(row)

# Write the sorted and filtered traffic summary to a new CSV file
with open('sorted_traffic_summary.csv', 'w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    for row in sorted_traffic_summary[2:]:
        writer.writerow(row)

print("Sorted and filtered traffic summary has been saved to 'sorted_traffic_summary.csv'")
