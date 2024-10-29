module interpolation_mod
    implicit none
contains

    ! 子程序：生成均匀分布节点
    subroutine linspace(start, end_, n, arr)
        real, intent(in) :: start, end_
        integer, intent(in) :: n
        real, dimension(n), intent(out) :: arr
        integer :: i
        do i = 1, n
            arr(i) = start + (end_ - start) * (i - 1) / (n - 1)
        end do
    end subroutine linspace

    ! 子程序：生成递增顺序的切比雪夫节点
    subroutine chebyshev_nodes(start, end_, n, arr)
        real, intent(in) :: start, end_
        integer, intent(in) :: n
        real, dimension(n), intent(out) :: arr
        integer :: i
        real :: pi_val
        pi_val = 4.0 * atan(1.0)  ! 计算 π

        do i = 1, n
            arr(i) = 0.5 * (start + end_) - 0.5 * (end_ - start) * &
                     cos((2.0 * i - 1.0) * pi_val / (2.0 * n))
        end do
    end subroutine chebyshev_nodes

    ! 函数：拉格朗日插值
    real function lagrange_interpolation(x, y, x_new)
        real, dimension(:), intent(in) :: x, y
        real, intent(in) :: x_new
        integer :: n, i, j
        real :: term

        n = size(x)
        lagrange_interpolation = 0.0

        ! 拉格朗日插值公式
        do i = 1, n
            term = y(i)
            do j = 1, n
                if (i /= j) then
                    term = term * (x_new - x(j)) / (x(i) - x(j))
                end if
            end do
            lagrange_interpolation = lagrange_interpolation + term
        end do
    end function lagrange_interpolation

    ! 子程序：分段三次样条插值（保持不变）
    subroutine cubic_spline(x, y, x_new, y_new)
        real, dimension(:), intent(in) :: x, y, x_new
        real, dimension(:), intent(out) :: y_new
        integer :: n, i, j
        real, dimension(:), allocatable :: a, b, c, d, h, alpha, l, mu, z
        real :: tmp

        n = size(x) - 1
        allocate(a(1:n), b(1:n), c(1:n), d(1:n), h(1:n), alpha(1:n-1), l(1:n), mu(1:n), z(1:n))

        ! Step 1: 计算 h 和 alpha
        do i = 1, n
            h(i) = x(i+1) - x(i)
        end do

        do i = 2, n
            alpha(i-1) = 3.0 * (y(i+1) - y(i)) / h(i) - 3.0 * (y(i) - y(i-1)) / h(i-1)
        end do

        ! Step 2: 构建三对角矩阵系统
        l(1) = 1.0
        mu(1) = 0.0
        z(1) = 0.0
        do i = 2, n
            l(i) = 2.0 * (x(i+1) - x(i-1)) - h(i-1) * mu(i-1)
            mu(i) = h(i) / l(i)
            z(i) = (alpha(i-1) - h(i-1) * z(i-1)) / l(i)
        end do
        l(n) = 1.0
        z(n) = 0.0
        c(n) = 0.0

        ! Step 3: 求解系数 c, b, d
        do i = n-1, 1, -1
            c(i) = z(i) - mu(i) * c(i+1)
            b(i) = (y(i+1) - y(i)) / h(i) - h(i) * (c(i+1) + 2.0 * c(i)) / 3.0
            d(i) = (c(i+1) - c(i)) / (3.0 * h(i))
            a(i) = y(i)
        end do

        ! Step 4: 对新点进行插值
        do i = 1, size(x_new)
            tmp = x_new(i)
            ! 查找 tmp 所在区间
            if (tmp < x(1)) then
                y_new(i) = y(1)
            else if (tmp > x(n+1)) then
                y_new(i) = y(n+1)
            else
                do j = 1, n
                    if (tmp >= x(j) .and. tmp <= x(j+1)) then
                        y_new(i) = a(j) + b(j)*(tmp - x(j)) + c(j)*(tmp - x(j))**2 + d(j)*(tmp - x(j))**3
                        exit
                    end if
                end do
            end if
        end do

        deallocate(a, b, c, d, h, alpha, l, mu, z)
    end subroutine cubic_spline

end module interpolation_mod

program interpolation
    use interpolation_mod
    implicit none

    ! 声明变量和数组
    real, dimension(10) :: h = [0.0, 1000.0, 2000.0, 3000.0, 4000.0, 5000.0, 6000.0, 7000.0, 8000.0, 10000.0]
    real, dimension(10) :: T = [15.0, 8.5, 2.0, -4.5, -10.0, -16.5, -23.0, -30.5, -37.5, -50.0]
    real, dimension(20) :: h_new, h_chebyshev, T_lagrange_even, T_lagrange_chebyshev
    integer :: i
    character(len=100) :: filename_even, filename_chebyshev
    integer :: output_unit_even, output_unit_chebyshev

    ! 生成 20 个均匀分布的高度节点
    call linspace(0.0, 10000.0, 20, h_new)

    ! 生成 20 个切比雪夫节点（递增顺序）
    call chebyshev_nodes(0.0, 10000.0, 20, h_chebyshev)

    ! 使用均匀节点进行拉格朗日插值
    do i = 1, 20
        T_lagrange_even(i) = lagrange_interpolation(h, T, h_new(i))
    end do

    ! 使用切比雪夫节点进行拉格朗日插值
    do i = 1, 20
        T_lagrange_chebyshev(i) = lagrange_interpolation(h, T, h_chebyshev(i))
    end do

    ! 将均匀节点插值结果写入文件
    filename_even = 'interpolation_runge_even.txt'
    open(unit=output_unit_even, file=filename_even, status='replace')

    ! 写入列标题
    write(output_unit_even, '(A, 3X, A)') 'Height (m)', 'T_Lagrange_Even (°C)'

    ! 写入数据
    do i = 1, 20
        write(output_unit_even, '(F10.2, 3X, F10.2)') h_new(i), T_lagrange_even(i)
    end do

    close(output_unit_even)

    ! 将切比雪夫节点插值结果写入文件
    filename_chebyshev = 'interpolation_runge_chebyshev.txt'
    open(unit=output_unit_chebyshev, file=filename_chebyshev, status='replace')

    ! 写入列标题
    write(output_unit_chebyshev, '(A, 3X, A)') 'Height (m)', 'T_Lagrange_Chebyshev (°C)'

    ! 写入数据
    do i = 1, 20
        write(output_unit_chebyshev, '(F10.2, 3X, F10.2)') h_chebyshev(i), T_lagrange_chebyshev(i)
    end do

    close(output_unit_chebyshev)

    print *, 'Interpolation results written to ', filename_even, ' and ', filename_chebyshev

end program interpolation
