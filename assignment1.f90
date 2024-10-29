program main
    parameter(n = 160, m = 71)
    integer :: iyb = 1951
    integer :: Temperature(n, m)
    real :: Precipitation(n, m)
    real :: MeanMonth01TOfGZ(1951:2021), MeanMonth01PrecipitationOfGZ(1951:2021)
    character(len = 100) :: filename
    integer :: i, j, k, unit_number

    ! 读取温度数据
    filename = 'C:\\Users\\Chen Yong\\Desktop\\1951.1-2021.7\\temperature\\t1601.txt'
    open(unit = 11, file = filename, status = 'old', action = 'read')
    read(11, *)((Temperature(i, j), i = 1, n), j = 1, m)
    close(11)

    ! 读取降水数据
    filename = 'C:\\Users\\Chen Yong\\Desktop\\1951.1-2021.7\\precipitation\\r1601.txt'
    open(unit = 12, file = filename, status = 'old', action = 'read')
    read(12, *)((Precipitation(i, j), i = 1, n), j = 1, m)
    close(12)

    ! 计算平均值
    do k = 1, 71
        MeanMonth01TOfGZ(1951 + k - 1) = Temperature(76 + k - 1, k)
        MeanMonth01PrecipitationOfGZ(1951 + k - 1) = Precipitation(76 + k - 1, k)
    end do

    ! 写入数据到文本文件
    call write_data('data.txt', MeanMonth01TOfGZ, MeanMonth01PrecipitationOfGZ)

    ! 调用GNU Plot绘制图形
    call system('gnuplot -p -e "load ''plot_script.gp''"')

contains

    subroutine write_data(filename, data1, data2)
        character(len=*), intent(in) :: filename
        real, dimension(:), intent(in) :: data1, data2
        integer :: i, unit_number

        open(unit = 21, file = filename, status = 'replace', action = 'write')

        do i = 1, size(data1)
            write(21, '(I5, 1X, F10.2, 1X, F10.2)') 1951 + i - 1, data1(i), data2(i)
        end do

        close(unit = 21)
    end subroutine write_data

end program main