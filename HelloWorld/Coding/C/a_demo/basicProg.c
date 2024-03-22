#include <stdio.h>
int main()
{
   int i = 1;
   int sum = 0;
   while(i <= 10){
      sum += i;
      i++;
   }
   int c;
   int a = 10;
   c = a++; 

   // printf的输出默认输出字符串
   // 如果输出数字，则需要对输出格式进行定义
   
   printf("先赋值后运算：\n");
   printf("Line 1 - c 的值是 %d\n", c );
   printf("Line 2 - a 的值是 %d\n", a );
   printf("%d\n",sum);

   // printf() 中字符串需要引号
   printf("Hello, World!");
   
   return 0;

}