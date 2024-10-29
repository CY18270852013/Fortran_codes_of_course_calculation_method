program interpolation
    implicit none
    integer, dimension(10) :: height
    integer :: i, k
    real :: temp, Lx = 0
    character :: x
    real, dimension(10) :: temperature, l
    l(1 : 10) = 1.0
    height = (/ 0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 10000 /)
    temperature = (/ 15.0, 8.5, 2.0, -4.5, -10.0, -16.5, -23.0, -30.5, -37.5, -50.0 /)
    do k = 1, 10
        do i = 1, 10
            temp =  (x - height(i)) / (height(k) - height(i))
                if(k /= i) then
                    temp = 1
            end if
            l(k) = temp * l(k)
            write(*, *)l(k)
        end do
    end do
    do k = 1, 10
        temp = temperature(k) * l(k)
        Lx = Lx + temp
    end do
    write(*, '(A)')Lx
end program interpolation