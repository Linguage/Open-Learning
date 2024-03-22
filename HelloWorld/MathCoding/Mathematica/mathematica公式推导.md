# 模态叠加法模拟钢轨振动

$$EI_{Y}\frac{\partial^{4}Z_{t}(x,t)}{\partial x^{4}}+m_{\mathrm{r}}\frac{\partial^{2}Z_{t}(x,t)}{\partial t^{2}}=-\sum_{i=1}^{N}F_{\mathrm{rs}i}(t)\partial(x-x_{i})+\sum_{j=1}^{4}p_{j}\delta(x-x_{\mathrm{w}j})$$

$$F_{\mathrm{rs}i}(t)=K_{\mathrm{p}i}\begin{bmatrix}Z_{\mathrm{r}}(x_i,t)-Z_{\mathrm{s}i}(t)\end{bmatrix}+C_{\mathrm{p}i}\begin{bmatrix}\dot{Z}_{\mathrm{r}}(x_i,t)-\dot{Z}_{\mathrm{s}i}(t)\end{bmatrix}$$

$$\begin{cases}x_{\mathrm{wl}}\left(t\right)=x_0+2(l_{\mathrm{c}}+l_{\mathrm{t}})+vt\\x_{\mathrm{w2}}\left(t\right)=x_0+2l_{\mathrm{c}}+vt\\x_{\mathrm{w3}}\left(t\right)=x_0+2l_{\mathrm{t}}+vt\\x_{\mathrm{w4}}\left(t\right)=x_0+vt\end{cases}$$

$$x_{i}=il_{s}\quad(i{=}1{\sim}N)$$

$$Z_k(x)=\sqrt{\frac{2}{m_\text{r}l}}\sin\frac{k\pi x}{l}$$

$$Z_{\mathrm{r}}(x,t)=\sum_{k=1}^{NM}Z_{k}(x)q_{k}(t)$$

$$\begin{aligned}&\sum_{k=1}^{NM}EI_{Y}\frac{\mathrm{d}^{4}Z_{k}(x)}{\mathrm{d}x^{4}}q_{k}(t)+\sum_{k=1}^{NM}m_{r}Z_{k}(x)\ddot{q}_{k}(t)\\=&-\sum_{i=1}^{N}F_{\mathrm{rs}i}(t)\delta(x-x_{i})+\sum_{j=1}^{4}p_{j}(t)\delta(x-x_{\mathrm{w}j})\end{aligned}$$


$$\begin{aligned}
&\int_{0}^{l}EI_{Y}\frac{\mathrm{d}^{4}Z_{k}(x)}{\mathrm{d}x^{4}}Z_{k}(x)q_{k}(t)\mathrm{d}x+\int_{0}^{l}m_{r}Z_{k}(x)Z_{k}(x)\ddot{q_{k}}(t)\mathrm{d}x \\
=& -\sum_{i=1}^{N}\int_{0}^{l}F_{\mathrm{rs}i}(t)Z_{k}(x)\delta(x-x_{i})\mathrm{d}x  \\
&+\sum_{j=1}^{4}\int_{0}^{l}p_{j}\left(t\right)Z_{k}(x)\delta(x-x_{\mathrm{w}j})\mathrm{d}x\quad(k=1\sim NM)
\end{aligned}$$

$$\begin{aligned}&\ddot{m_t}\ddot{q_k}(t)\int_0^lZ_k^2(x)\mathrm{d}x+EI_Yq_k(t)\int_0^lZ_k(x)\frac{\mathrm{d}^4Z_k(x)}{\mathrm{d}x^4}\mathrm{d}x\\=&-\sum_{i=1}^NF_{\text{rs}i}(t)Z_k(x_i)+\sum_{j=1}^4p_j(t)Z_k(x_{\text{w}j})\quad(k=1\sim N\text{M})\end{aligned}$$

$$\int_0^lZ_k^2\left(x\right)\mathrm{d}x=\frac1{m_\mathrm{r}}$$

$$\begin{aligned}
\int_0^lZ_k(x)\frac{\mathrm{d}^4Z_k(x)}{\mathrm{d}x^4}\mathrm{d}x& =\int_0^l\frac2{m_\text{r}l}\left.\left(\frac{k\pi}l\right)^4\sin^2\frac{k\pi x}l\mathrm{d}x\right.   \\
&=\left(\frac{k\pi}l\right)^4\int_0^lZ_k^2\left(x\right)\mathrm{d}x \\
&=\frac1{m_{\mathrm{r}}}\left(\frac{k\pi}l\right)^4
\end{aligned}$$

$$\begin{aligned}&\ddot{q}_k(t)+\frac{EI_Y}{m_\mathrm{r}}\left(\frac{k\pi}{l}\right)^4q_k(t)\\=&-\sum_{i=1}^NF_\mathrm{rsi}(t)Z_k(x_i)+\sum_{j=1}^4p_j(t)Z_k(x_\mathrm{wj})\quad(k=1\sim NM)\end{aligned}$$

$$\begin{aligned}F_{\mathrm{rsi}}(t)=C_{\mathrm{pi}}\sum_{h=1}^{\mathrm{NM}}Z_{h}(x_{i})\dot{q}_{h}(t)+K_{\mathrm{pi}}\sum_{h=1}^{\mathrm{NM}}Z_{h}(x_{i})q_{h}(t)-C_{\mathrm{pi}}\dot{Z}_{\mathrm{si}}(t)-K_{\mathrm{pi}}Z_{\mathrm{si}}(t)\end{aligned}$$

$$\begin{aligned}
&\ddot{q}_{k}(t)+\sum_{i=1}^{N}C_{pi}Z_{k}(x_{i})\sum_{h=1}^{NM}Z_{h}(x_{i})\dot{q}_{h}(t)+\frac{EI_{Y}}{m_{_r}}\left(\frac{k\pi}{l}\right)^{4}q_{k}(t) \\
&+\sum_{i=1}^{N}K_{pi}Z_{k}(x_{i})\sum_{h=1}^{NM}Z_{h}(x_{i})q_{h}(t)-\sum_{i=1}^{N}C_{pi}Z_{k}(x_{i})\dot{Z}_{si}(t) \\
&-\sum_{i=1}^{N}K_{\mathrm{pi}}Z_{k}(x_{i})Z_{\mathrm{si}}(t)=\sum_{j=1}^{4}p_{j}(t)Z_{k}(x_{\mathrm{wj}})\quad(k=1\sim NM) 
\end{aligned}$$


