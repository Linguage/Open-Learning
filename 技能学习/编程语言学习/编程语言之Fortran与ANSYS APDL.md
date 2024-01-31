编程语言之Fortran
[toc]

# Fortran的基本语法
## HelloFortran

**Fortran的HelloWorld**
最简单的fortran程序包含三部分：
1. 声明部分
2. 执行部分
3. 终止部分

``` Fortran
program HelloWorld
write(*,*) "Hello, World!"
end program
```


**一般Fortran编程原则**

   1.  保留字都大写，如PROGRAM、READ和WRITE
   2.  程序的变量用小写字母表示。
   3.  名字中的下划线出现在两个字之间。
   4.  大写字母作为常量名。

由于大写和小写字母在Fortran中作用相当，所以程序按任何一种方式来书写都可以。

## 基本结构

### 基本语句

内容来自网页：[Fortran教程](https://www.yiibai.com/fortran/fortran_cycle.html)

**if 判断语句**

``` fortran
if (logical expression 1) then 
   ! block 1   
else if (logical expression 2) then       
   ! block 2   
else       
   ! block 3   
end if

```

**do循环语句**(类似于for循环)

``` fortran
do var = start, stop [,step]    
   ! statement(s)
end do
```

**do-while循环语句**
```fortran
do while (logical expr) 
   statements
end do
```



**输入与输出**

``` fortran
READ(*.*) input_list
WRITE(*.*) output_list
```


### 编程模块

**四类声明**
1. program
2. module
3. function
4. subroutime

**调用语句**

- use
- call
-  

## 数据类型


![89fe93834dc3285b584c32b3ee3f92a3.png](89fe93834dc3285b584c32b3ee3f92a3.png)

## 内置的函数



# Fortran的编程范式

内容来自网页：[现代Fortran的推荐范式](https://zhuanlan.zhihu.com/p/100615040)

1. 现代Fortran一定是以主程序program和多个module组成的。
2. 所有的函数function和subroutine应该封装在module中。这样既可以避免同名函数导致的链接错误，更能使编译器在链接时帮忙检查参数列表。


``` Fortran

subroutine f (x) 
  ! 野函数，不推荐, 编译器不会检查x是不是real，是不是一维
  implicit none
  real(dp), intent(in) :: x(:)
end subroutine f

module a
contains
  subroutine f (x) 
  ! 推荐, 编译器会检查传入的x是否合法
    implicit none
    real(dp), intent(in) :: x(:)
  end subroutine f
end module

```




# Fortran的矩阵计算




# ANSYS的APDL语言

APDL语言风格集成了ANSYS，可以认为是一种分支。



## ANSYS仿真的基本模块


## APDL的拓展：二次开发与联合仿真


**知乎“蒙特卡遇见罗”的经验分享：**
- [ANSYS APDL与二次开发(1) —— APDL语言的常用功能](https://zhuanlan.zhihu.com/p/266431989)
- [ANSYS APDL与二次开发(2) —— 联合仿真分析基础](https://zhuanlan.zhihu.com/p/268513036)
- [ANSYS APDL与二次开发(3) ——基于ANSYS、MATLAB和C++的联合仿真分析](https://zhuanlan.zhihu.com/p/285175072)


## 多任务自动化仿真