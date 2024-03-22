subroutine agjdn(a, b, n, m, l, js)
    implicit none  ! 声明所有变量必须显式声明
    integer, intent(in) :: n, m  ! 输入参数n和m
    integer, intent(out) :: l  ! 输出参数l
    real, dimension(n, n), intent(inout) :: a  ! 输入输出数组a
    real, dimension(n, m), intent(inout) :: b  ! 输入输出数组b
    integer, dimension(n), intent(out) :: js  ! 输出数组js

    integer :: k, i, j, is
    real :: q, d
    logical :: fail

    l = 1  ! 初始化l
    do k = 1, n
        q = 0.0
        do i = k, n
            do j = k, n
                if (abs(a(i, j)) > q) then
                    q = abs(a(i, j))
                    js(k) = j
                    is = i
                end if
            end do
        end do
        fail = (q + 1.0e0) == 1.0
        if (fail) then
            print '(1X, A)', '  FAIL   '
            l = 0
            return
        end if

        do j = k, n
            a(k, j) = a(is, j)
        end do
        do j = 1, m
            b(k, j) = b(is, j)
        end do
        do i = 1, n
            a(i, k) = a(i, js(k))
        end do
        do j = k + 1, n
            a(k, j) = a(k, j) / a(k, k)
        end do
        do j = 1, m
            b(k, j) = b(k, j) / a(k, k)
        end do
        do i = 1, n
            if (i .ne. k) then
                do j = k + 1, n
                    a(i, j) = a(i, j) - a(i, k) * a(k, j)
                end do
                do j = 1, m
                    b(i, j) = b(i, j) - a(i, k) * b(k, j)
                end do
            end if
        end do
    end do
    do k = n, 1, -1
        do j = 1, m
            d = b(k, j)
            b(k, j) = b(js(k), j)
            b(js(k), j) = d
        end do
    end do
end subroutine agjdn