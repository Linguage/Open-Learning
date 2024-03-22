

PROGRAM calc_hypotenuse_test   ! 主程序
    IMPLICIT NONE
    REAL :: s1
    REAL :: s2
    REAL :: hypot

    WRITE(*,*) '斜边长的计算：'
    WRITE(*,*)'b1：'
    READ(*,*) s1
    WRITE(*,*)'b2：'
    READ(*,*) s2

    CALL calc_hypotenuse( s1, s2 , hypot )   ! 调用子例程

    WRITE( *, 100 ) hypot
    100 FORMAT('c：' , F10.4)

    STOP
END PROGRAM calc_hypotenuse_test
