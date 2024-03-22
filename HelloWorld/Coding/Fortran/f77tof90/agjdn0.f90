program main
    implicit none  ! 隐式声明所有变量
    integer, parameter :: N = 4, M = 2  ! 定义常量N和M
    real, dimension(N, N) :: A  ! 声明并初始化矩阵A
    real, dimension(N, M) :: B  ! 声明矩阵B
    integer, dimension(N) :: JS  ! 声明数组JS
    integer :: L  ! 声明变量L用于存储子程序的返回值
    real :: B_output(N, M)  ! 用于存储输出的B矩阵

    ! 初始化矩阵A和B
    A = reshape([1.0, 7.0, 9.0, -2.0, 3.0, 2.0, 15.0, -2.0, 2.0, 1.0, 3.0, 11.0, 13.0, -2.0, -2.0, 5.0], shape(A))
    B = reshape([9.0, 6.0, 11.0, -2.0, 0.0, 4.0, 7.0, -1.0], shape(B))

    ! 调用子程序AGJDN
    call agjdn(A, B, N, M, L, JS)

    ! 如果L不等于0，输出变换后的B矩阵
    if (L .ne. 0) then
        print *, (B_output(I, J), I = 1, N, J = 1, M)
    end if

end program main