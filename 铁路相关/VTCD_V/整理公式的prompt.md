
```chatgpt
请整理如下latex公式，主要修改下标.

下标中的t s c p w 以及数字均有物理实体含义，应当使用\mathrm命令，而x y z表示方向，应为斜体，不需要添加\mathrm命令。

例如，M_c\ddot{Z}_c改为M_\mathrm{c}\ddot{Z}_\mathrm{c}, K_{\mathrm{pz}}应当修改为K_{\mathrm{p}z}，C_{sz}应当修改为C_{\mathrm{s}z}.

每一组公式取消换行，也取消公式编号（比如\quad(2.10）),但输出的时候需要每组都通过$$..$$的形势输出：
```

## 工程中的数学公式符号分类

代数符号
  - 加法：$\pm$
  - 减法：$\mp$
  - 乘法：$\times$
  - 除法：$\div$
  - 根号：$\sqrt{}$
  - 开方：$\sqrt[]{}$
  - 绝对值：$\mid{} \mid$
  - 向上取整：$\lfloor{} \rfloor$
  - 向下取整：$\lceil{} \rceil$
  - 集合：$\emptyset$, $\in$, $\notin$, $\subset$, $\supset$, $\subseteq$, $\supseteq$
  - 逻辑符号：$\land$, $\lor$, $\lnot$, $\implies$, $\iff$
  - 集合运算符：$\cup$, $\cap$, $\setminus$, $\bigcup$, $\bigcap$, $\bigodot$, $\bigotimes$, $\bigoplus$    
  - 箭头：$\leftarrow$, $\rightarrow$, $\leftrightarrow$, $\mapsto$, $\to$, $\leftarrowtail$, $\rightarrowtail$, $\twoheadleftarrow$, $\twoheadrightarrow$, $\hookleftarrow$, $\hookrightarrow$, $\nearrow$, $\searrow$, $\swarrow$, $\nwarrow$
  - 希腊字母：$\alpha$, $\beta$, $\gamma$, $\delta$, $\epsilon$, $\zeta$, $\eta$, $\theta$, $\iota$, $\kappa$, $\lambda$, $\mu$, $\nu$, $\xi$, $\omicron$, $\pi$, $\rho$, $\sigma$, $\tau$, $\upsilon$, $\phi$, $\chi$, $\psi$, $\omega$    
  - 特殊符号：$\forall$, $\exists$, $\nexists$, $\varnothing$, $\aleph$, $\top$, $\bot$, $\pm$, $\mp$, $\ldots$, $\cdots$, $\vdots$, $\ddots$, $\forall$, $\exists$, $\nexists$, $\varnothing$, $\aleph$, $\top$, $\bot$, $\pm$, $\mp$, $\ldots$, $\cdots$, $\vdots$, $\ddots$

微积分符号
  - 导数：$\frac{d}{dx}$, $\frac{d^2}{dx^2}$, $\frac{d^3}{dx^3}$, $\frac{d^n}{dx^n}$
  - 积分：$\int$, $\iint$, $\iiint$,, $\oint$
  - 微分方程：$\frac{d}{dt}y(t) = f(t,y(t))$
  - 偏导数：$\frac{\partial}{\partial x}$, $\frac{\partial^2}{\partial x^2}$, $\frac{\partial^3}{\partial x^3}$, $\frac{\partial^n}{\partial x^n}$
  - 泰勒展开：$\sin x = \sum_{n=0}^{\infty}\frac{(-1)^n}{(2n+1)!}x^{2n+1}$
  - 指数：$e^x$, $10^x$, $\log_a x$, $\ln x$, $\lg x$, $\exp x$, $\sinh x$, $\cosh x$, $\tanh x$, $\coth x$, $\arcsin x$, $\arccos x$, $\arctan x$, $\arccot x$, $\arccsc x$, $\arcsec x$, $\arg x$, $\dim x$, $\ker x$, $\det x$, $\Pr x$, $\gcd(a,b)$, $\lcm(a,b)$, $\inf x$, $\sup x$, $\lim_{x\to a}f(x)$, $\sum_{i=1}^n a_i$, $\prod_{i=1}^n a_i$
  - 矩阵：$\begin{bmatrix}a_{11} & a_{12} & \cdots & a_{1n}\\a_{21} & a_{22} & \cdots & a_{2n}\\\vdots & \vdots & \ddots & \vdots\\a_{m1} & a_{m2} & \cdots & a_{mn}\end{bmatrix}$
  - 向量：$\vec{a}$, $\vec{ab}$, $\vec{abc}$, $\vec{1}$, $\vec{x}$, $\vec{y}$, $\vec{z}$, $\vec{i}$, $\vec{j}$, $\vec{k}$
  - 张量：$\mathcal{T}_a$, $\mathcal{T}_ab$, $\mathcal{T}_abc$, $\mathcal{T}_1$, $\mathcal{T}_x$, $\mathcal{T}_y$, $\mathcal{T}_z$, $\mathcal{T}_i$, $\mathcal{T}_j$, $\mathcal{T}_k$
  - 希腊字母：$\alpha$, $\beta$, $\gamma$, $\delta$, $\epsilon$, $\zeta$, $\eta$, $\theta$, $\iota$, $\kappa$, $\lambda$, $\mu$, $\nu$, $\xi$, $\omicron$, $\pi$, $\rho$, $\sigma$, $\tau$, $\upsilon$, $\phi$, $\chi$, $\psi$, $\omega$    
  - 特殊符号：$\forall$, $\exists$, $\nexists$, $\varnothing$, $\aleph$, $\top$, $\bot$, $\pm$, $\mp$, $\ldots$, $\cdots$, $\vdots$, $\ddots$, $\forall$, $\exists$, $\nexists$, $\varnothing$, $\aleph$, $\top$, $\bot$, $\pm$, $\mp$, $\ldots$, $\cdots$, $\vdots$, $\ddots$

几何符号
  - 点：$\cdot$, $\times$, $\div$, $\ast$, $\star$, $\circ$, $\bullet$, $\cdot$, $\times$, $\div$, $\ast$, $\star$, $\circ$, $\bullet$
  - 向量：$\vec{a}$, $\vec{ab}$, $\vec{abc}$, $\vec{1}$, $\vec{x}$, $\vec{y}$, $\vec{z}$, $\vec{i}$, $\vec{j}$, $\vec{k}$
  - 线段：$\overline{AB}$, $\overline{ABCD}$, $\underline{AB}$, $\underline{ABCD}$
  - 平面：$\overrightarrow{AB}$, $\overrightarrow{ABCD}$, $\overleftarrow{AB}$, $\overleftarrow{ABCD}$
  - 角度：$\angle AOB$, $\angle AOBC$, $\angle AOC$, $\angle AOC$, $\angle ABD$, $\angle ABD$, $\angle ACD$, $\angle ACD$, $\angle BCD$, $\angle BCD$
  - 圆：$\overline{AB}$, $\overline{ABCD}$, $\underline{AB}$, $\underline{ABCD}$
  - 空间：$\overrightarrow{AB}$, $\overrightarrow{ABCD}$, $\overleftarrow{AB}$, $\overleftarrow{ABCD}$
  - 特殊符号：$\forall$, $\exists$, $\nexists$, $\varnothing$, $\aleph$, $\top$, $\bot$, $\pm$, $\mp$, $\ldots$, $\cdots$, $\vdots$, $\ddots$, $\forall$, $\exists$, $\nexists$, $\varnothing$, $\aleph$, $\top$, $\bot$, $\pm$, $\mp$, $\ldots$, $\cdots$, $\vdots$, $\ddots$

统计学符号
  - 期望：$\mathbb{E}(X)$
  - 方差：$\mathbb{V}(X)$
  - 标准差：$\sigma(X)$
  - 协方差：$\mathbb{C}(X,Y)$
  - 相关系数：$\rho(X,Y)$
  - 统计量：$\bar{X}$, $\hat{X}$, $\tilde{X}$, $\widehat{X}$, $\widetilde{X}$
  - 统计符号：$\pm$, $\mp$, $\times$, $\div$, $\ast$, $\star$, $\circ$, $\bullet$, $\cdot$, $\times$, $\div$, $\ast$, $\star$, $\circ$, $\bullet$
  - 特殊符号：$\forall$, $\exists$, $\nexists$, $\varnothing$, $\aleph$, $\top$, $\bot$, $\pm$, $\mp$, $\ldots$, $\cdots$, $\vdots$, $\ddots$, $\forall$, $\exists$, $\nexists$, $\varnothing$, $\aleph$, $\top$, $\bot$, $\pm$, $\mp$, $\ldots$, $\cdots$, $\vdots$, $\ddots$

物理学符号
  - 力：$\vec{F}$, $\vec{F}_1$, $\vec{F}_2$, $\vec{F}_3$, $\vec{F}_4$, $\vec{F}_5$, $\vec{F}_6$, $\vec{F}_7$, $\vec{F}_8$, $\vec{F}_9$, $\vec{F}_{10}$
  - 力矩：$\vec{T}$, $\vec{T}_1$, $\vec{T}_2$, $\vec{T}_3$, $\vec{T}_4$, $\vec{T}_5$, $\vec{T}_6$, $\vec{T}_7$, $\vec{T}_8$, $\vec{T}_9$, $\vec{T}_{10}$
  - 位移：$\vec{v}$, $\vec{v}_1$, $\vec{v}_2$, $\vec{v}_3$, $\vec{v}_4$, $\vec{v}_5$, $\vec{v}_6$, $\vec{v}_7$, $\vec{v}_8$, $\vec{v}_9$, $\vec{v}_{10}$
  - 速度：$\vec{v}$, $\vec{v}_1$, $\vec{v}_2$, $\vec{v}_3$, $\vec{v}_4$, $\vec{v}_5$, $\vec{v}_6$, $\vec{v}_7$, $\vec{v}_8$, $\vec{v}_9$, $\vec{v}_{10}$
  - 加速度：$\vec{a}$, $\vec{a}_1$, $\vec{a}_2$, $\vec{a}_3$, $\vec{a}_4$, $\vec{a}_5$, $\vec{a}_6$, $\vec{a}_7$, $\vec{a}_8$, $\vec{a}_9$, $\vec{a}_{10}$
  - 角速度：$\vec{\omega}$, $\vec{\omega}_1$, $\vec{\omega}_2$, $\vec{\omega}_3$, $\vec{\omega}_4$, $\vec{\omega}_5$, $\vec{\omega}_6$, $\vec{\omega}_7$, $\vec{\omega}_8$, $\vec{\omega}_9$, $\vec{\omega}_{10}$
  - 角加速度：$\vec{\alpha}$, $\vec{\alpha}_1$, $\vec{\alpha}_2$, $\vec{\alpha}_3$, $\vec{\alpha}_4$, $\vec{\alpha}_5$, $\vec{\alpha}_6$, $\vec{\alpha}_7$, $\vec{\alpha}_8$, $\vec{\alpha}_9$, $\vec{\alpha}_{10}$
  - 时间：$t$, $t_1$, $t_2$, $t_3$, $t_4$, $t_5$, $t_6$, $t_7$, $t_8$, $t_9$, $t_{10}$
  - 频率：$\omega$, $\omega_1$, $\omega_2$, $\omega_3$, $\omega_4$, $\omega_5$, $\omega_6$, $\omega_7$, $\omega_8$, $\omega_9$, $\omega_{10}$
  - 功率：$P$, $P_1$, $P_2$, $P_3$, $P_4$, $P_5$, $P_6$, $P_7$, $P_8$, $P_9$, $P_{10}$
  - 电流：$I$, $I_1$, $I_2$, $I_3$, $I_4$, $I_5$, $I_6$, $I_7$, $I_8$, $I_9$, $I_{10}$
  - 电压：$U$, $U_1$, $U_2$, $U_3$, $U_4$, $U_5$, $U_6$, $U_7$, $U_8$, $U_9$, $U_{10}$
  - 电导率：$\rho$, $\rho_1$, $\rho_2$, $\rho_3$, $\rho_4$, $\rho_5$, $\rho_6$, $\rho_7$, $\rho_8$, $\rho_9$, $\rho_{10}$
  - 电荷：$Q$, $Q_1$, $Q_2$, $Q_3$, $Q_4$, $Q_5$, $Q_6$, $Q_7$, $Q_8$, $Q_9$, $Q_{10}$ 
  
  