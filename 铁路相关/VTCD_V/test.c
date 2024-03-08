# 编写一个程序，要求用户输入一个整数，然后输出该整数的各位数字的和。

#include <stdio.h>

int main()
{
    int num, sum = 0;
    printf("请输入一个整数：");
    scanf("%d", &num);
    while (num!= 0)
    {
        sum += num % 10;
        num /= 10;
    }
    printf("各位数字的和为：%d", sum);
    return 0;
}   
