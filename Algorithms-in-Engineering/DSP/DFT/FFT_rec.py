import numpy as np
import matplotlib.pyplot as plt

# 从文件导入时域信号
signal = np.loadtxt('1.txt')

# 使用FFT将时域信号转换为频域信号
spectrum = np.fft.fft(signal)

# 使用IFFT将频域信号转换回时域信号
reconstructed_signal = np.fft.ifft(spectrum).real

# 计算复原信号与原始信号之间的误差
error = signal - reconstructed_signal

# 保存频域信号到文本文件
np.savetxt('spectrum.txt', spectrum)

# 绘制原始信号图像
plt.figure(figsize=(10, 7))
plt.subplot(411)
plt.plot(signal, label='Original Signal')
plt.xlabel('Time')
plt.ylabel('Amplitude')
plt.title('Original Signal')
plt.legend()

# 绘制频域信号图像
plt.subplot(412)
plt.plot(np.abs(spectrum), label='Frequency Domain Signal')
plt.xlabel('Frequency')
plt.ylabel('Amplitude')
plt.title('Frequency Domain Signal')
plt.legend()

# 绘制复原信号图像
plt.subplot(413)
plt.plot(reconstructed_signal, label='Reconstructed Signal')
plt.xlabel('Time')
plt.ylabel('Amplitude')
plt.title('Reconstructed Signal')
plt.legend()

# 绘制误差图
plt.subplot(414)
plt.plot(error, label='Error', color='red')
plt.xlabel('Time')
plt.ylabel('Amplitude')
plt.title('Error')
plt.xticks([])
plt.legend()

plt.tight_layout()
plt.show()
