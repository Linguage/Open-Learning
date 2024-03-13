eq1 = Mc*Zc''[t] + 2*Csz*Zc'[t] + 2*Ksz*Zc[t] - Csz*Zt1'[t] - 
    Ksz*Zt1[t] - Csz*Zt2'[t] - Ksz*Zt2[t] == Mc*g;
eq2 = Jc*\[Beta]c''[t] + 2*Csz*lc^2*\[Beta]c'[t] + 
    2*Ksz*lc^2*\[Beta]c[t] + Csz*lc*Zt1'[t] - Csz*lc*Zt2'[t] + 
    Ksz*lc*Zt1[t] - Ksz*lc*Zt2[t] == 0;
eq3 = Mt*Zt1''[t] + (2*Cpz + Csz)*Zt1'[t] + (2*Kpz + Ksz)*Zt1[t] - 
    Csz*Zc'[t] - Ksz*Zc[t] - Cpz*Zw1'[t] - Cpz*Zw2'[t] - Kpz*Zw1[t] - 
    Kpz*Zw2[t] + Csz*lc*\[Beta]c'[t] + Ksz*lc*\[Beta]c[t] == Mt*g;
eq4 = Jt*\[Beta]t1''[t] + 2*Cpz*lt^2*\[Beta]t1'[t] + 
    2*Kpz*lt^2*\[Beta]t1[t] + Cpz*lt*Zw1'[t] - Cpz*lt*Zw2'[t] + 
    Kpz*lt*Zw1[t] - Kpz*lt*Zw2[t] == 0;
eq5 = Mt*Zt2''[t] + (2*Cpz + Csz)*Zt2'[t] + (2*Kpz + Ksz)*Zt2[t] - 
    Csz*Zc'[t] - Ksz*Zc[t] - Cpz*Zw3'[t] - Cpz*Zw4'[t] - Kpz*Zw3[t] - 
    Kpz*Zw4[t] - Csz*lc*\[Beta]c'[t] - Ksz*lc*\[Beta]c[t] == Mt*g;
eq7 = Mw*Zw1''[t] + Cpz*Zw1'[t] + Kpz*Zw1[t] - Cpz*Zt1'[t] - 
    Kpz*Zt1[t] + Cpz*lt*\[Beta]t1'[t] + Kpz*lt*\[Beta]t1[t] + 
    2*p1[t] - Mw*g == F01[t];
eq8 = Mw*Zw2''[t] + Cpz*Zw2'[t] + Kpz*Zw2[t] - Cpz*Zt1'[t] - 
    Kpz*Zt1[t] - Cpz*lt*\[Beta]t1'[t] - Kpz*lt*\[Beta]t1[t] + 
    2*p2[t] - Mw*g == F02[t];
eq9 = Mw*Zw3''[t] + Cpz*Zw3'[t] + Kpz*Zw3[t] - Cpz*Zt2'[t] - 
    Kpz*Zt2[t] + Cpz*lt*\[Beta]t2'[t] + Kpz*lt*\[Beta]t2[t] + 
    2*p3[t] - Mw*g == F03[t];
eq10 = Mw*Zw4''[t] + Cpz*Zw4'[t] + Kpz*Zw4[t] - Cpz*Zt2'[t] - 
    Kpz*Zt2[t] - Cpz*lt*\[Beta]t2'[t] - Kpz*lt*\[Beta]t2[t] + 
    2*p4[t] - Mw*g == F04[t];

以下是重新渲染后的 LaTeX 代码：

$$
\begin{pmatrix}
2K_{\mathrm{sz}} & 0 & -K_{\mathrm{sz}} & 0 & -K_{\mathrm{sz}} & 0 & 0 & 0 & 0 & 0 \\
0 & 2K_{\mathrm{sz}} l_\mathrm{c}^2 & K_{\mathrm{sz}} l_\mathrm{c} & 0 & -K_{\mathrm{sz}} l_\mathrm{c} & 0 & 0 & 0 & 0 & 0 \\
-K_{\mathrm{sz}} & K_{\mathrm{sz}} l_\mathrm{c} & 2K_{\mathrm{pz}} + K_{\mathrm{sz}} & 0 & 0 & 0 & -K_{\mathrm{pz}} & -K_{\mathrm{pz}} & 0 & 0 \\
0 & 0 & 0 & 2K_{\mathrm{pz}} l_\mathrm{t}^2 & 0 & 0 & K_{\mathrm{pz}} l_\mathrm{t} & -K_{\mathrm{pz}} l_\mathrm{t} & 0 & 0 \\
-K_{\mathrm{sz}} & -K_{\mathrm{sz}} l_\mathrm{c} & 0 & 0 & 2K_{\mathrm{pz}} + K_{\mathrm{sz}} & 0 & 0 & 0 & -K_{\mathrm{pz}} & -K_{\mathrm{pz}} \\
0 & 0 & 0 & 0 & 0 & 2K_{\mathrm{pz}} l_\mathrm{t}^2 & 0 & 0 & K_{\mathrm{pz}} l_\mathrm{t} & -K_{\mathrm{pz}} l_\mathrm{t} \\
0 & 0 & -K_{\mathrm{pz}} & K_{\mathrm{pz}} l_\mathrm{t} & 0 & 0 & K_{\mathrm{pz}} & 0 & 0 & 0 \\
0 & 0 & -K_{\mathrm{pz}} & -K_{\mathrm{pz}} l_\mathrm{t} & 0 & 0 & 0 & K_{\mathrm{pz}} & 0 & 0 \\
0 & 0 & 0 & 0 & -K_{\mathrm{pz}} & K_{\mathrm{pz}} l_\mathrm{t} & 0 & 0 & K_{\mathrm{pz}} & 0 \\
0 & 0 & 0 & 0 & -K_{\mathrm{pz}} & -K_{\mathrm{pz}} l_\mathrm{t} & 0 & 0 & 0 & K_{\mathrm{pz}} \\
\end{pmatrix}
$$

现在，矩阵中的下标和上标已经根据物理实体含义使用了正确的命令进行渲染，并且方向变量被斜体显示，符合规范。