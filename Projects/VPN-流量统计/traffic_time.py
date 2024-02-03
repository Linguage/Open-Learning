import csv
from collections import defaultdict
from datetime import datetime, timedelta
import matplotlib.pyplot as plt

# Data storage dictionary
traffic_statistics = defaultdict(float)

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

        # Consider only the last 72 hours of data
        if log_time >= datetime.now() - timedelta(hours=72):
            # Round the timestamp to 5 minutes
            log_time = log_time - timedelta(minutes=log_time.minute % 5, seconds=log_time.second)
            # Accumulate traffic
            traffic_statistics[log_time] += traffic_mb

# Convert the statistics into the format required for the line plot
x = [time_slot.strftime('%Y-%m-%d %H:%M') for time_slot in sorted(traffic_statistics.keys())]
y = [traffic for traffic in traffic_statistics.values()]

# Plot the fluctuation curve
plt.figure(figsize=(12, 6))
plt.plot(x, y, color='blue', linestyle='-')
plt.xlabel('Time Period')
plt.ylabel('Traffic (MB)')
plt.title('Traffic Fluctuation in the Last 72 Hours')
# Set the x-axis ticks to hourly intervals
hourly_ticks = [time_slot.strftime('%Y-%m-%d %H:%M') for time_slot in sorted(traffic_statistics.keys()) if time_slot.minute == 0 and time_slot.hour % 6 == 0]
plt.xticks(hourly_ticks, rotation=45, ha='right', fontsize=8)
plt.tight_layout()
plt.grid(True)
plt.show()
