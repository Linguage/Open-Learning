% 这个文件是用来测试FSWT的，主要是为了验证FSWT的效果，并画出时频图。



%% 加载信号
clear; clc; close all;
%%load('ecg.mat');
s = load('1.txt'); % 这个ECG信号采样率是250Hz
s = 0.2*s
Fs = 100; % 采样频率，需要根据实际信号改
N = length(s); % 序列长度
%s = s-sum(s)/N; % 减去信号中的直流部分

%% 进行频率切片小波变换(FSWT)
% [f1,f2] 是想观察到的频率范围
f1 = 0; % 最低频率A
f2 = 60; % 最高频率, 因心电信号有用频段是60Hz以下，所以设了60

k1 = fix(f1*N/Fs-0.5);
k2 = fix(f2*N/Fs-0.5); % k1、k2是计算频率点数
df = 1;  % 想观测的频率步长,频率间隔

if (k2>N/2+1)
    k2=N/2+1;
end

fp = fix(k1:df:k2);   % fp大概是指定一下频率坐标轴
nl = length(fp);
kapa = sqrt(2)/2/0.025;  % kapa 是时频分辨率因子

Tn = 512; % 时域数值，可以改，影响不是特别大

[A] = GetFSWT(s,Fs,fp,kapa,Tn); % 对信号s进行频率切片小波变换

B = sqrt(A.*conj(A));
mx = max(max(B));
B = fix(B*128/mx); % 这个B存放的就是时频分析的结果了，后边就是画出来就可以了

% 原信号
figure;
subplot(221);
plot((0:N-1)/Fs,s); xlabel('Time (s)'); ylabel('Amplitude'); title('原信号');

% FFT频谱
subplot(222)
Y = fft(s,N);
Yk = Y.*conj(Y);
Yk = sqrt(Yk);
Yk(1) = 0;
Yk(2) = 0;
K1 = fix(f2*N/Fs);
fp1 = [0 : K1-1]/N*Fs;
plot(fp1,Yk(1:K1));
xlabel('Frequency (Hz)'); ylabel('X(k)'); title('FFT频谱');

% FSWT时频图
subplot(223)
t = (0:Tn-1)*N/Fs/Tn;
[x,y] = meshgrid(t,fp*Fs/N);
[na,nb] = size(y);
mesh(x,y,B');
xlabel('Time (s)'); ylabel('Frequency (Hz)'); zlabel('Amplitude'); title('FSWT时频图');

% FSWT时频图 图片形式
subplot(224)
pcolor(x,y,B');
shading interp; axis tight;
xlabel('Time (s)');ylabel('Frequency (Hz)'); title('FSWT时频图(图片形式)');

