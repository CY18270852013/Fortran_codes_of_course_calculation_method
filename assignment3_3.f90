program fit_ellipse
    implicit none
    ! 声明变量
    integer, parameter :: n = 10
    real :: x_obs(n) = [1.02, 0.95, 0.87, 0.77, 0.67, 0.56, 0.44, 0.30, 0.16, 0.01]
    real :: y_obs(n) = [0.39, 0.32, 0.27, 0.22, 0.18, 0.15, 0.13, 0.12, 0.13, 0.15]
    real :: delta_x(n) = [-0.0029, 0.0007, -0.0082, -0.0038, -0.0041, 0.0026, -0.0001, -0.0058, -0.0005, -0.0034]
    real :: delta_y(n) = [-0.0033, 0.0043, 0.0006, 0.0020, 0.0044, 0.0009, 0.0028, 0.0034, 0.0059, 0.0024]
    real :: x_obs_perturbed(n), y_obs_perturbed(n)
    real :: A(n, 5), b(n), coeffs(5)
    real :: r2, ss_res, ss_tot, y_mean
    integer :: i
    character(len=100) :: filename

    ! 计算扰动数据
    do i = 1, n
        x_obs_perturbed(i) = x_obs(i) + delta_x(i)
        y_obs_perturbed(i) = y_obs(i) + delta_y(i)
    end do

    ! 构造 A 矩阵和 b 向量用于最小二乘问题
    do i = 1, n
        A(i, 1) = 1.0
        A(i, 2) = x_obs_perturbed(i)
        A(i, 3) = y_obs_perturbed(i)
        A(i, 4) = x_obs_perturbed(i) * y_obs_perturbed(i)
        A(i, 5) = y_obs_perturbed(i)**2
        b(i) = x_obs_perturbed(i)**2
    end do

    ! 使用最小二乘法求解系数
    call least_squares(A, b, coeffs)

    ! 计算拟合的相关系数 R²
    ss_res = 0.0
    ss_tot = 0.0
    y_mean = sum(y_obs_perturbed) / n

    do i = 1, n
        ss_res = ss_res + (b(i) - (coeffs(1) + coeffs(2) * x_obs_perturbed(i) + &
                                     coeffs(3) * y_obs_perturbed(i) + &
                                     coeffs(4) * x_obs_perturbed(i) * y_obs_perturbed(i) + &
                                     coeffs(5) * y_obs_perturbed(i)**2))**2
        ss_tot = ss_tot + (b(i) - y_mean)**2
    end do

    r2 = 1.0 - ss_res / ss_tot

    ! 打印结果
    print *, 'Fitted coefficients: ', coeffs
    print *, 'Coefficient of determination (R^2): ', r2

contains

    subroutine least_squares(A, b, coeffs)
        real, intent(in) :: A(:, :), b(:)
        real, intent(out) :: coeffs(:)
        real :: AT(5, n), ATA(5, 5), ATb(5)
        integer :: i, j

        ! 转置 A 矩阵
        do i = 1, 5
            do j = 1, n
                AT(i, j) = A(j, i)
            end do
        end do

        ! 计算 ATA 和 ATb
        ATA = matmul(AT, A)
        ATb = matmul(AT, b)

        ! 使用高斯消元法（或其他方法）求解系数
        call gaussian_elimination(ATA, ATb, coeffs)
    end subroutine least_squares

    subroutine gaussian_elimination(A, b, x)
        real, intent(inout) :: A(:,:), b(:)
        real, intent(out) :: x(size(b))
        integer :: i, j, k, n
        real :: factor

        n = size(b)

        ! 前向消元
        do i = 1, n - 1
            do j = i + 1, n
                factor = A(j, i) / A(i, i)
                A(j, :) = A(j, :) - factor * A(i, :)
                b(j) = b(j) - factor * b(i)
            end do
        end do

        ! 回代
        do i = n, 1, -1
            x(i) = (b(i) - sum(A(i, i + 1:n) * x(i + 1:n))) / A(i, i)
        end do
    end subroutine gaussian_elimination

end program fit_ellipse
