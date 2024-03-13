% 本程序的功能：计算Sperling指标
% 输入：acc：加速度信号；dt：采样时间；unit：单位；direc：方向
% 输出：W：Sperling指标
%
% 程序的框架：
% 1. 定义函数getSperling
% 2. 输入参数：acc、dt、unit、direc
% 3. 计算加速度信号的单位转换
% 4. 计算采样频率、FFT参数
% 5. 计算频谱的幅值和频率
% 6. 计算Sperling指标
% 7. 输出Sperling指标
%
% 调用程序的命令：
% [W] = getSperling(acc,dt,unit,direc)
%
% 程序输出文件的结构：
% W：Sperling指标


% 定义函数
function W = getSperling(acc,dt,unit,direc)
if(unit=='g')  % 后续处理的系数是m/s^2
    acc = acc*9.81;
end
N = length(acc);  % calculating length of series
fs = 1/dt;      % calculating sampling frequency
nfft = 2^nextpow2(N);
df = fs/nfft;   % calculating frequency resolution
X = fft(acc,nfft);   % apply FFT
A = abs(X(1:nfft/2))*2/N;  % Amplitude spectrum
f = (0:nfft/2-1)*df;       % the frequency series

W = 0;

% 计算f(i): 0.5Hz <= f < 40Hz
% 方向为Z时，计算Fi: 0.325*f^2 for 0.5Hz <= f < 5.9Hz
%          400/f^2 for 5.9Hz <= f < 20Hz
%          1 for 20Hz <= f < 40Hz
% 方向为Y时，计算Fi: 0.8*f^2 for 0.5Hz <= f < 5.4Hz
%          650/f^2 for 5.4Hz <= f < 26Hz
%          1 for 26Hz <= f < 40Hz



if(direc == 'Z' || direc == 'z')
    for i = 1:length(f)
        if (f(i) >= 0.5) && (f(i) < 40)  % only calculating  0.5Hz <= f < 40Hz
            if f(i) < 5.9
                Fi = 0.325*f(i)^2;
            elseif f(i) < 20
                Fi = 400/f(i)^2;
            else
                Fi = 1;
            end
            wi = 3.57*(A(i)^3/f(i)*Fi)^0.1;  % calculating every Wi
            W = W+wi^10;
        end
    end
elseif(direc == 'Y' || direc == 'y')
    for i = 1:length(f)
        if (f(i) >= 0.5) && (f(i) < 40)  % only calculating  0.5Hz <= f < 40Hz
            if f(i) < 5.4
                Fi = 0.8*f(i)^2;
            elseif f(i) < 26
                Fi = 650/f(i)^2;
            else
                Fi = 1;
            end
            wi = 3.57*(A(i)^3/f(i)*Fi)^0.1;  % calculating every Wi
            W = W+wi^10;
        end
    end
end
W = W^0.1;
end