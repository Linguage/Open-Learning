# 本程序的功能为：
# 1. 定义 Kalman 滤波器类
# 2. 实例化 Kalman 滤波器对象
# 3. 生成模拟数据
# 4. 运行 Kalman 滤波器
# 5. 绘图

# 导入必要的库
import numpy as np
import matplotlib.pyplot as plt

# 定义 Kalman 滤波器类
class KalmanFilter:
    # Constructor

    # 输入：
    # initial_state_mean：初始状态的均值
    # initial_state_covariance：初始状态的协方差
    # process_covariance：系统过程噪声的协方差
    # measurement_covariance：测量噪声的协方差
    def __init__(self, initial_state_mean, initial_state_covariance, process_covariance, measurement_covariance):
        self.state_mean = initial_state_mean
        self.state_covariance = initial_state_covariance
        self.process_covariance = process_covariance
        self.measurement_covariance = measurement_covariance
    
    # Prediction and update steps 预测和更新步骤
    # 预测步骤：根据系统过程噪声的协方差，预测下一时刻的状态
    # 更新步骤：根据测量值和系统测量噪声的协方差，更新状态的均值和协方差

    def predict(self):
        # Prediction step
        self.state_mean = 0  # 一维状态的线性系统简化为 0
        self.state_covariance += self.process_covariance

    def update(self, measurement):
        # Update step
        kalman_gain = self.state_covariance / (self.state_covariance + self.measurement_covariance)
        self.state_mean += kalman_gain * (measurement - self.state_mean)
        self.state_covariance *= (1 - kalman_gain)

# 实例化 Kalman 滤波器对象
        

# 定义系统参数
initial_state_mean = 0  # 初始状态的均值
initial_state_covariance = 1  # 初始状态的协方差
process_variance = 0.1  # 系统过程噪声的方差
measurement_variance = 0.5  # 测量噪声的方差

# 创建 Kalman 滤波器对象
kf = KalmanFilter(initial_state_mean, initial_state_covariance, process_variance, measurement_variance)

# 生成模拟数据
#     真实状态：服从正态分布 N(0, 0.1)
#     观测值：真实状态 + 服从正态分布 N(0, 0.5)

np.random.seed(42)
true_states = np.random.normal(loc=0, scale=0.1, size=100)  # 真实状态
measurements = true_states + np.random.normal(loc=0, scale=0.5, size=100)  # 观测值

# 运行 Kalman 滤波器
#     预测步骤：根据系统过程噪声的协方差，预测下一时刻的状态
#     更新步骤：根据测量值和系统测量噪声的协方差，更新状态的均值和协方差
filtered_states = []
for z in measurements:
    kf.predict()
    kf.update(z)
    filtered_states.append(kf.state_mean)

# 绘图
plt.plot(range(len(true_states)), true_states, label='True States', color='blue')
plt.plot(range(len(measurements)), measurements, label='Measurements', color='red', marker='o', linestyle='')
plt.plot(range(len(filtered_states)), filtered_states, label='Filtered States', color='green')
plt.xlabel('Time')
plt.ylabel('Value')
plt.title('Kalman Filter Demo')
plt.legend()
plt.show()
