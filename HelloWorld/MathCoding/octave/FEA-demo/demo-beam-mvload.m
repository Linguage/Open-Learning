% 定义桥梁的几何参数
L = 10; % 桥梁长度
H = 1;  % 桥梁高度
num_elements = 20; % 分割成的单元数量

% 创建有限元网格
nodes = linspace(0, L, num_elements+1);
num_nodes = length(nodes);

% 创建有限元单元
elements = zeros(num_elements, 2);
for i = 1:num_elements
    elements(i, :) = [i, i+1];
end

% 定义材料性质和截面属性
E = 10^7; % 弹性模量
A = 1;    % 截面面积

% 初始化刚度矩阵和载荷向量
K = zeros(num_nodes, num_nodes);
F = zeros(num_nodes, 1);

% 组装刚度矩阵和载荷向量
for i = 1:num_elements
    n1 = elements(i, 1);
    n2 = elements(i, 2);
    length_element = nodes(n2) - nodes(n1);
    ke = (E * A / length_element) * [1, -1; -1, 1]; % 单元刚度矩阵
    K(n1:n2, n1:n2) += ke;
end

% 施加边界条件（简支梁桥的边界条件）
fixed_node = 1;
K(fixed_node, :) = 0;
K(:, fixed_node) = 0;
K(fixed_node, fixed_node) = 1;
F(fixed_node) = 0;

% 定义移动荷载
moving_load = @(x) 1000 * sin(pi * x / L); % 此处假设荷载以正弦形式变化

% 施加移动荷载
for i = 1:num_elements
    n1 = elements(i, 1);
    n2 = elements(i, 2);
    length_element = nodes(n2) - nodes(n1);
    midpoint = (nodes(n1) + nodes(n2)) / 2;
    F(n1:n2) += (length_element / 2) * moving_load(midpoint);
end

% 求解位移
displacements = K \ F;

% 输出结果或后续分析
disp('位移向量：');
disp(displacements);

% 绘制变形图等
