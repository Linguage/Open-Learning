# 编写一个fibonacci数列的函数，要求输入参数为n，返回第n个fibonacci数。

function fibonacci(n)
    if n == 1
        return 0
    elseif n == 2   
        return 1
    else
        return fibonacci(n-1) + fibonacci(n-2)
    end
end

# 测试
print(fibonacci(1))    # 0
print(fibonacci(2))    # 1
print(fibonacci(3))    # 1
print(fibonacci(4))    # 2
print(fibonacci(5))    # 3
print(fibonacci(6))    # 5
print(fibonacci(7))    # 8
print(fibonacci(8))    # 13
print(fibonacci(9))    # 21
print(fibonacci(10))   # 34
