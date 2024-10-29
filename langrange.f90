module interpolation_mod
    implicit none
contains

    subroutine linspace(start, end_, n, arr)
        real, intent(in) :: start, end_
        integer, intent(in) :: n
        real, dimension(n), intent(out) :: arr
        integer :: i
        do i = 1, n
            arr(i) = start + (end_ - start) * (i - 1) / (n - 1)
        end do
    end subroutine linspace

    real function lagrange_interpolation(x, y, x_new)
        real, dimension(:), intent(in) :: x, y
        real, intent(in) :: x_new
        integer :: n, i, j
        real :: term

        n = size(x)
        lagrange_interpolation = 0.0

        ! Lagrange interpolation formula
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

    subroutine cubic_spline(x, y, x_new, y_new)
        real, dimension(:), intent(in) :: x, y, x_new
        real, dimension(:), intent(out) :: y_new
        integer :: n, i, j
        real, dimension(:), allocatable :: a, b, c, d, h, alpha, l, mu, z
        real :: tmp

        n = size(x) - 1
        allocate(a(1:n), b(1:n), c(1:n), d(1:n), h(1:n), alpha(1:n-1), l(1:n), mu(1:n), z(1:n))

        ! Step 1: Calculate h and alpha
        do i = 1, n
            h(i) = x(i+1) - x(i)
        end do

        do i = 2, n
            alpha(i-1) = 3.0 * (y(i+1) - y(i)) / h(i) - 3.0 * (y(i) - y(i-1)) / h(i-1)
        end do

        ! Step 2: Setup the tridiagonal system
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

        ! Step 3: Solve for coefficients c, b, and d
        do i = n-1, 1, -1
            c(i) = z(i) - mu(i) * c(i+1)
            b(i) = (y(i+1) - y(i)) / h(i) - h(i) * (c(i+1) + 2.0 * c(i)) / 3.0
            d(i) = (c(i+1) - c(i)) / (3.0 * h(i))
            a(i) = y(i)
        end do

        ! Step 4: Interpolate the new points
        do i = 1, size(x_new)
            tmp = x_new(i)
            ! Find the interval for x_new(i)
            if (tmp < x(1)) then
                y_new(i) = y(1)
            else if (tmp > x(n)) then
                y_new(i) = y(n)
            else
                do j = 1, n-1
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

    ! Declare variables and arrays
    real, dimension(10) :: h = [0.0, 1000.0, 2000.0, 3000.0, 4000.0, 5000.0, 6000.0, 7000.0, 8000.0, 10000.0]
    real, dimension(10) :: T = [15.0, 8.5, 2.0, -4.5, -10.0, -16.5, -23.0, -30.5, -37.5, -50.0]
    real, dimension(20) :: h_new, T_lagrange, T_spline
    integer :: i
    character(len=100) :: filename
    integer :: output_unit

    ! 1. Generate 20 evenly spaced nodes for height
    call linspace(0.0, 10000.0, 20, h_new)

    ! 2. Calculate temperature using Lagrange interpolation
    do i = 1, 20
        T_lagrange(i) = lagrange_interpolation(h, T, h_new(i))
    end do

    ! 3. Calculate temperature using cubic spline interpolation
    call cubic_spline(h, T, h_new, T_spline)

    ! 4. Output results to a file for comparison
    filename = 'interpolation_results.txt'
    open(unit=output_unit, file=filename, status='replace')

    ! Output column headers for the data
    write(output_unit, '(A, 3X, A, 3X, A)') 'Height (m)', 'T_Lagrange (°C)', 'T_Spline (°C)'

    do i = 1, 20
        write(output_unit, '(F10.2, 3X, F10.2, 3X, F10.2)') h_new(i), T_lagrange(i), T_spline(i)
    end do

    close(output_unit)
    
    print *, 'Interpolation results written to ', filename

end program interpolation
