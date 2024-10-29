program main
    parameter(n = 160, m = 71)
    integer :: iyb = 1951
    integer Temperature(n, m)
    real :: Precipitation(n, m)
    real :: MeanMonth01TOfGZ(1951:2021), MeanMonth01PrecipitationOfGZ(1951:2021)
    character(len = 100) :: filename
    filename = 'C:\\Users\\Chen Yong\\Desktop\\1951.1-2021.7\\temperature\\t1601.txt'
    open(unit = 11,file = filename, status = 'old', action = 'read')
    read(11, *)((Temperature(i, j), i = 1, n), j = 1, m)
    close(11)
    open(12, file = 'C:\\Users\\Chen Yong\\Desktop\\1951.1-2021.7\\precipitation\\r1601.txt')
    read(12, *)((Precipitation(i, j), i = 1, n), j = 1, m)
    close(12)

    do k = 1, 71
        MeanMonth01TOfGZ(1951 + k - 1) = Temperature(76 + k - 1, k)
        MeanMonth01PrecipitationOfGZ(1951 + k - 1) = Precipitation(76 + k - 1, k)
    end do

    print *, MeanMonth01TOfGZ
    print *, MeanMonth01PrecipitationOfGZ

end program main