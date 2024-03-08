# 编写一个julia的程序，实现fibonacci数列的计算。

function fibonacci(n::Int)   # 定义函数，输入参数为Int类型
    if n == 0
        return 0
    elseif n == 1
        return 1
    else                 # 递归计算
        return fibonacci(n-1) + fibonacci(n-2)
    end
end

println(fibonacci(10))   # 调用函数，输出第10个fibonacci数
