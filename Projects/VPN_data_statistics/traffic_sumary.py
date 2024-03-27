import csv
from collections import defaultdict
from datetime import datetime, timedelta
import operator

# Data storage dictionary
node_traffic = defaultdict(float)
traffic_statistics = defaultdict(lambda: defaultdict(float))

# Read the CSV file and perform traffic statistics
with open('merged_data_sorted.csv', newline='') as csvfile:
    reader = csv.reader(csvfile)
    # Skip the header
    next(reader)
    for row in reader:
        # Parse the timestamp
        log_time = datetime.strptime(row[4], '%Y-%m-%d %H:%M:%S')

        # Extract traffic data and convert to MB
        traffic = row[3]
        if traffic.endswith('KB'):
            traffic_mb = float(traffic[:-2]) / 1024
        elif traffic.endswith('MB'):
            traffic_mb = float(traffic[:-2])
        elif traffic.endswith('GB'):
            traffic_mb = float(traffic[:-2]) * 1024
        elif traffic.endswith('B'):
            traffic_mb = float(traffic[:-1]) / (1024 * 1024)
        else:
            traffic_mb = 0

        # Accumulate traffic for each node and time period
        node_traffic[row[0]] += traffic_mb
        # Consider only the last 168 hours of data
        if log_time >= datetime.now() - timedelta(hours=168):
            # Round the timestamp to the nearest three hours
            log_time = log_time - timedelta(hours=log_time.hour % 3, minutes=log_time.minute, seconds=log_time.second)
            # Accumulate traffic
            traffic_statistics[row[0]][log_time] += traffic_mb

# Convert the statistics into a sorted list
sorted_traffic = sorted(traffic_statistics.items(), key=lambda x: sum(x[1].values()), reverse=True)

# Get all unique time periods
all_time_periods = set()
for node_data in traffic_statistics.values():
    all_time_periods.update(node_data.keys())

# Sort time periods
sorted_time_periods = sorted(all_time_periods)

# Write the results to a CSV file
with open('traffic_summary.csv', 'w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    # Write the header row
    header_row = ['Time Period'] + list(node_traffic.keys())
    writer.writerow(header_row)

    # Write the traffic for each time period
    for time_period in sorted_time_periods:
        traffic_values = []
        for node_name in node_traffic.keys():
            traffic_values.append(f'{traffic_statistics[node_name][time_period]:.2f}' if time_period in traffic_statistics[node_name] else '0.00')
        writer.writerow([time_period.strftime('%Y-%m-%d %H:%M')] + traffic_values)

print("Traffic summary has been saved to 'traffic_summary.csv'")
