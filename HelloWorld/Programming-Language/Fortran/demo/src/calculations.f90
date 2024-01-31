module calculations_module
    implicit none

contains

    ! 计算函数
    function calculate(x, y)
        integer, intent(in) :: x, y
        integer :: calculate

        calculate = x + y
    end function calculate

end module calculations_module
