! Example demonstrating using MKL
subroutine mkl_demo
    use mkl_service
    implicit none

    integer, parameter :: N = 100
    real*8 :: A(N,N), B(N,N), C(N,N)
    integer :: i, j

    ! Initialize matrices
    do i = 1, N
        do j = 1, N
            A(i, j) = 1.0
            B(i, j) = 2.0
        end do
    end do

    ! Use MKL to perform matrix multiplication
    call dgemm('N', 'N', N, N, N, 1.0, A, N, B, N, 0.0, C, N)

    print *, 'Matrix multiplication completed.'

end subroutine mkl_demo