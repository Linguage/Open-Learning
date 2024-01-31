module output_module
    implicit none

contains

    ! 输出函数
    subroutine output_result(result)
        integer, intent(in) :: result
        integer :: unit_num

        ! 打开文件
        open(unit=unit_num, file='output.txt', status='replace')

        ! 写入结果到文件
        write(unit_num, '(A, I0)') "Result: ", result

        ! 关闭文件
        close(unit_num)
    end subroutine output_result

end module output_module
