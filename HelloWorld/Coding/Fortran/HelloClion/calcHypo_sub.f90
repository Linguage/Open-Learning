SUBROUTINE calc_hypotenuse(side_1 , side_2 , hypotenuse)  ! 子例程
    IMPLICIT NONE
    REAL , INTENT(IN)::side_1    ! 第一条直边长度，输入，INTENT用法见下述
    REAL , INTENT(IN)::side_2    ! 第二条直边长度，输入
    REAL , INTENT(OUT)::hypotenuse   ! 斜边长度，输出

    REAL::temp    ! 声明局部变量
    temp = side_1**2 + side_2**2
    hypotenuse = SQRT( temp )
END SUBROUTINE calc_hypotenuse
