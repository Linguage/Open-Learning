program main
    use omp_lib
    implicit none

    integer, parameter :: N = 1000000
    real*8 :: a(N), b(N), c(N)
    integer :: i

    ! Initialize arrays
    do i = 1, N
        a(i) = i
        b(i) = N - i + 1
    end do

    ! Parallelize the loop using OpenMP
    !$OMP PARALLEL DO
    do i = 1, N
        c(i) = a(i) + b(i)
    end do
    !$OMP END PARALLEL DO

    print *, 'Computation completed.'

end program main