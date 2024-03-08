import numpy as np

def calculate_vibration_formula_coefficient(frequency, direction):
    """
    根据频率和振动方向计算振动公式系数 F(f)
    
    参数：
    - frequency：频率
    - direction：振动方向，'vertical'（垂向）或 'horizontal'（横向）
    
    返回值：
    - F(f)：振动公式系数
    """
    if direction == 'vertical':
        if 0.5 <= frequency <= 5.9:
            return 0.325 * frequency ** 2
        elif 5.9 < frequency <= 20:
            return 400 / frequency ** 2
        else:
            return 1
    elif direction == 'horizontal':
        if 0.5 <= frequency <= 5.4:
            return 0.8 * frequency ** 2
        elif 5.4 < frequency <= 26:
            return 650 / frequency ** 2
        else:
            return 1
    else:
        return None

# 假设你已经有了时域的样本数据 At
# At = [your time domain signal]

# 对时域信号进行 FFT 转换
freq_domain = np.fft.fft(At)
Nt = len(At)  # 时域样本点数

# 提取频率和对应的幅值
freq = np.fft.fftfreq(Nt)[:Nt//2]  # 提取正频率部分
Amp = np.abs(freq_domain)[:Nt//2] * 2 / Nt  # 计算幅值，并进行修正

# 替换原有代码中的 your_vibration_formula(f) 函数
# 重新计算 W 值的部分代码如下：
W_values = []

# 遍历每个频率点，计算对应的 W 值
for f, A in zip(freq, Amp):
    # 根据频率 f 和幅值 A 计算 W 值
    F_f = calculate_vibration_formula_coefficient(f, direction)  
    # 根据振动方向确定计算的振动公式系数
    W = 7.08 * ((A ** 3) / f * F_f) ** 0.1
    W_values.append(W)

# 计算最终指标 W
final_W = np.power(sum(np.power(W_values, 10)), 0.1)

# 输出最终指标 W
print("Final W value:", final_W)
