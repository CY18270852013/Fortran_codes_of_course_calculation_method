module interpolationModule
    implicit none
contains
    function lagrangeInterpolation(x, height, temperature) result(Lx)
        implicit none
        real, intent(in) :: x
        integer, dimension(:), intent(in) :: height
        real, dimension(:), intent(in) :: temperature
        real :: Lx, temp
        real, dimension(size(height)) :: l
        integer :: i, k

        ! 初始化l数组
        l = 1.0

        ! 计算拉格朗日基函数
        do k = 1, size(height)
            do i = 1, size(height)
                if (i /= k) then
                    temp = (x - height(i)) / (height(k) - height(i))
                    l(k) = l(k) * temp
                end if
            end do
        end do

        ! 计算插值
        Lx = sum(temperature * l)
    end function lagrangeInterpolation
end module interpolationModule

program interpolation
    use interpolationModule
    implicit none
    integer, parameter :: n = 10
    integer, dimension(n) :: height
    real, dimension(n) :: temperature
    real :: x, interpolatedValue

    ! 初始化height和temperature数组
    height = (/ 0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 10000 /)
    temperature = (/ 15.0, 8.5, 2.0, -4.5, -10.0, -16.5, -23.0, -30.5, -37.5, -50.0 /)

    ! 获取用户输入的x值
    print *, "Enter the value of x for interpolation: "
    read *, x

    ! 调用拉格朗日插值函数
    interpolatedValue = lagrangeInterpolation(x, height, temperature)

    ! 输出插值结果
    print *, "Interpolated value at x = ", x, " is: ", interpolatedValue
end program interpolation