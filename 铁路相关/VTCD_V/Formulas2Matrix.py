# 本程序旨在将LaTeX格式的动力学方程中的常量变量提取并转换为矩阵形式。
# 例如：方程如下：
# $$
# J_{c}\ddot{\beta}_{c}+2C_{sz}l_{c}^2\dot{\beta}_{c}+2K_{sz}l_{c}^2\beta_{c}+C_{sz}l_{c}\dot{Z}_{t1}-C_{sz}l_{c}\dot{Z}_{t2}+K_{sz}l_{c}Z_{t1}-K_{sz}l_{c}Z_{t2}=0
# $$
# 程序将提取出以下变量：
#  \ddot{\beta}_{c},\dot{\beta}_{c},\beta_{c},\dot{Z}_{t1},\dot{Z}_{t2},Z_{t1},Z_{t2} 
# 和如下常量：
# J_{c} +2C_{sz}l_{c}^2 +2K_{sz}l_{c}^2 +C_{sz}l_{c} -C_{sz}l_{c} +K_{sz}l_{c} -K_{sz}l_{c} 
  
# 变量中的\ddot{}表示加速度(二阶导)，\dot{}表示速度(一阶导)，
# 变量中的希腊字母\beta为转动角，Z表示竖直方向的位移
# _{}表示变量的下标，如\beta_{c}表示物体c的转动角


import re

# 定义正则表达式
# 匹配单词边界的单词，如\beta_{c}中的\beta和_{c}，不匹配\beta和c，因为\beta和c是变量名，_{c}是下标
pattern = r'(?=\\dd?ot|Z_|\\beta).+?(?=[+-])'

# 输入LaTeX格式的动力学方程
equation = r"J_{c}\ddot{\beta}_{c}+2C_{sz}l_{c}^2\dot{\beta}_{c}+2K_{sz}l_{c}^2\beta_{c}+C_{sz}l_{c}\dot{Z}_{t1}-C_{sz}l_{c}\dot{Z}_{t2}+K_{sz}l_{c}Z_{t1}-K_{sz}l_{c}Z_{t2}=0"


# 提取变量和常量
variables = re.findall(pattern, equation)
# 匹配数字，包括整数和小数，不包括符号， 
#(?<=[+\-*/])表示前面有加减乘除符号，(?=[+\-*/])表示后面有加减乘除符号
constants = re.findall(r'(?=[+-]).*?(?=\\d|Z|\\beta)', equation)

# 输出变量和常量
print("变量：", variables)
print("常量：", constants)


# 定义矩阵形式的方程
matrix_equation = ""
for i in range(len(variables)):
    matrix_equation += variables[i] + " "
matrix_equation += "= 0"


# 输出矩阵形式的方程
print("矩阵形式的方程：", matrix_equation) 
