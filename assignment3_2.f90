program orbit_fitting_with_perturbation
  implicit none
  integer, parameter :: n = 10, p = 5
  real(8) :: A(n,p), X(n), b(p), b_new(p)
  real(8) :: x_vals(n), y_vals(n), x_new(n), y_new(n)
  real(8) :: delta_x(n), delta_y(n)
  integer :: i

  ! 观测值
  data x_vals /1.02, 0.95, 0.87, 0.77, 0.67, 0.56, 0.44, 0.30, 0.16, 0.01/
  data y_vals /0.39, 0.32, 0.27, 0.22, 0.18, 0.15, 0.13, 0.12, 0.13, 0.15/

  ! 扰动值 Δx 和 Δy
  data delta_x /-0.0029, 0.0007, -0.0082, -0.0038, -0.0041, 0.0026, -0.0001, -0.0058, -0.0005, -0.0034/
  data delta_y /-0.0033, 0.0043,  0.0006,  0.0020,  0.0044, 0.0009,  0.0028,  0.0034,  0.0059,  0.0024/

  ! 构造原始A矩阵和X向量
  call construct_matrix(x_vals, y_vals, A, X, n, p)

  ! 计算原始拟合参数
  call least_squares_fit(A, X, b, n, p)

  ! 输出原始拟合参数
  print *, 'Original Fitted parameters:'
  print *, b

  ! 叠加扰动值，生成新的观测数据
  do i = 1, n
    x_new(i) = x_vals(i) + delta_x(i)
    y_new(i) = y_vals(i) + delta_y(i)
  end do

  ! 构造新的A矩阵和X向量
  call construct_matrix(x_new, y_new, A, X, n, p)

  ! 计算新的拟合参数
  call least_squares_fit(A, X, b_new, n, p)

  ! 输出新的拟合参数
  print *, 'New Fitted parameters with perturbation:'
  print *, b_new

contains
  ! 构造矩阵子程序
  subroutine construct_matrix(x_vals, y_vals, A, X, n, p)
    real(8), intent(in) :: x_vals(n), y_vals(n)
    real(8), intent(out) :: A(n,p), X(n)
    integer, intent(in) :: n, p
    integer :: i

    do i = 1, n
      A(i,1) = 1.0d0
      A(i,2) = x_vals(i)
      A(i,3) = y_vals(i)
      A(i,4) = x_vals(i) * y_vals(i)
      A(i,5) = y_vals(i)**2
      X(i) = x_vals(i)**2
    end do
  end subroutine construct_matrix

end program orbit_fitting_with_perturbation

! 最小二乘拟合子程序
subroutine least_squares_fit(A, X, b, n, p)
  real(8), intent(in) :: A(n,p), X(n)
  real(8), intent(out) :: b(p)
  integer, intent(in) :: n, p
  real(8) :: AtA(p,p), AtX(p)
  integer :: i, j, k

  ! 初始化AtA和AtX
  AtA = 0.0d0
  AtX = 0.0d0

  ! 计算AtA矩阵和AtX向量
  do i = 1, p
    do j = 1, p
      do k = 1, n
        AtA(i,j) = AtA(i,j) + A(k,i) * A(k,j)
      end do
    end do
    do k = 1, n
      AtX(i) = AtX(i) + A(k,i) * X(k)
    end do
  end do

  ! 通过高斯消元法求解AtA * b = AtX
  call gauss_solve(AtA, AtX, b, p)
end subroutine least_squares_fit

! 高斯消元法
subroutine gauss_solve(A, B, X, n)
  real(8), intent(inout) :: A(n,n), B(n)
  real(8), intent(out) :: X(n)
  integer, intent(in) :: n
  real(8) :: factor
  integer :: i, j, k
  
  ! 前向消元
  do i = 1, n-1
    do j = i+1, n
      factor = A(j,i) / A(i,i)
      A(j,:) = A(j,:) - factor * A(i,:)
      B(j) = B(j) - factor * B(i)
    end do
  end do
  
  ! 回代求解
  X(n) = B(n) / A(n,n)
  do i = n-1, 1, -1
    X(i) = (B(i) - sum(A(i,i+1:n) * X(i+1:n))) / A(i,i)
  end do
end subroutine gauss_solve
