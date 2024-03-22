! 定义外部子程序
! external :: f, jacobi

program main
! 声明数组和变量
real :: y(3,8), s(3,10), ym(3), d(3), p(3,3), z(3,30)
real :: s02(3), er(3), t(30), is(3), jjs(3)
real :: a, b, h, hmin, hmax
integer :: n, m

! 初始化变量
y = 1.0
a = 0.0
b = -1.0

! 输出格式和读取输入
write(*, '(1X, ''H,HMIN,HMAX,EPS?'')') 
read(*, *) h, hmin, hmax, eps
write(*, *)

! 设置循环次数和维度
n = 30
m = 3

! 调用子程序
call ggear(a, b, h, hmin, hmax, eps, m, n, y, t, z, kf, jacobi, d, p, s, s02, ym, er, is, jjs)
write(*, '(1X, ''KF-'', I4)') kf

! 打印标题
write(*, '(7X, ''T '', 14X, ''Y(1)'', 11X, ''Y(2)'', 11X, ''Y(3)'')')

! 打印结果
do i = 1, n
    write(*, '(I1, F10.6, 5X, 3D15.6)') t(i), z(1,i), z(2,i), z(3,i)
end do

contains

! 定义子程序F
subroutine f(t, y, m, d)
    integer, intent(in) :: m
    real, intent(in) :: t
    real, dimension(m), intent(in) :: y
    real, dimension(m) :: d

    d(1) = -21.0 * y(1) + 19.0 * y(2) - 20.0 * y(3)
    d(2) = -19.0 * y(1) - 21.0 * y(2) + 20.0 * y(3)
    d(3) = 40.0 * y(1) - 40.0 * y(2) - 40.0 * y(3)
end subroutine f

! 定义子程序JACOBI
subroutine jacobi(t, yp, m)
    integer, intent(in) :: m
    real, intent(in) :: t
    real, dimension(m, m), intent(out) :: yp

    yp(1, 1) = -21.0
    yp(1, 2) = 19.0
    yp(1, 3) = -20.0
    yp(2, 1) = 19.0
    yp(2, 2) = -21.0
    yp(2, 3) = -20.0
    yp(3, 1) = 40.0
    yp(3, 2) = -40.0
    yp(3, 3) = -40.0
end subroutine jacobi

end program
