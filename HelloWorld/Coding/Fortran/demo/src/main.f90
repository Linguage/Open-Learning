program main
    use calculations_module
    use output_module

    implicit none

    integer :: result

    ! 调用计算函数
    result = calculate(5, 10)

    ! 输出结果到文本文件
    call output_result(result)

    ! 打印成功信息
    print *, "Output generated successfully!"

end program main
