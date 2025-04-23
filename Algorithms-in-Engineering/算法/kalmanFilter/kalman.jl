############################################################
# 一维卡尔曼滤波器实例（位置估计）
# 假设有一组带噪声的位置信息，使用卡尔曼滤波器进行平滑
using Random
using Plots
using DelimitedFiles

# 生成带噪声的观测数据
function kalman_filter_demo()
    # 生成带噪声的观测数据
    n = 50
    true_position = 10.0
    process_noise_std = 0.1
    measurement_noise_std = 1.0

    Random.seed!(123)
    # 模拟真实位置和观测值
    positions = [true_position + randn() * process_noise_std for _ in 1:n]
    measurements = [pos + randn() * measurement_noise_std for pos in positions]

    # 卡尔曼滤波参数初始化
    x_est = 0.0                   # 初始估计
    p_est = 1.0                   # 初始估计协方差
    q = process_noise_std^2       # 过程噪声方差
    r = measurement_noise_std^2   # 观测噪声方差

    # 用于存储结果
    x_estimates = Float64[]
    p_estimates = Float64[]

    for z in measurements
        # 预测步骤
        x_pred = x_est            # 状态预测（这里假设系统为常值模型）
        p_pred = p_est + q        # 协方差预测

        # 更新步骤
        k = p_pred / (p_pred + r) # 卡尔曼增益
        x_est = x_pred + k * (z - x_pred) # 状态更新
        p_est = (1 - k) * p_pred          # 协方差更新

        push!(x_estimates, x_est)
        push!(p_estimates, p_est)
    end

    # 保存数据供Python绘图
    writedlm("kalmanFilter/kf_measurements.txt", measurements)
    writedlm("kalmanFilter/kf_positions.txt", positions)
    writedlm("kalmanFilter/kf_estimates.txt", x_estimates)

    # 调用Python脚本绘图（切换到henri_env环境）
    run(`conda run -n henri_env python kalmanFilter/plot_with_chinese.py`)
end

kalman_filter_demo()
