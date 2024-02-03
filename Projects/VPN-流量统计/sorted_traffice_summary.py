import csv
from collections import defaultdict

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
sorted_traffic_summary = []
for col_idx, col_data in enumerate(zip(*traffic_summary)):
    if col_idx == 0 or col_data[0] in filtered_nodes:
        sorted_traffic_summary.append(list(col_data))

# Rearrange traffic summary based on filtered nodes and transpose the matrix
sorted_traffic_summary_transposed = []

# Transpose the data except for the first row and first column
for i, row in enumerate(sorted_traffic_summary):
    if i == 0:  # Preserve the first row
        sorted_traffic_summary_transposed.append(row)
    else:
        transposed_row = [row[0]] + [f'{float(value):.2f}' for value in row[1:]]  # Preserve the first column, convert the rest to floats
        sorted_traffic_summary_transposed.append(transposed_row)

# Write the sorted and filtered traffic summary to a new CSV file
with open('sorted_traffic_summary.csv', 'w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    writer.writerows(zip(*sorted_traffic_summary_transposed))

print("Sorted and filtered traffic summary has been saved to 'sorted_traffic_summary.csv'")
