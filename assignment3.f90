program planet_orbit_fit
  implicit none
  integer :: i, j, n
  real(8), dimension(10) :: x, y, y_fitted
  real(8), dimension(5) :: b
  real(8), dimension(10, 5) :: A
  real(8), dimension(10) :: rhs
  real(8), dimension(5, 5) :: ATA
  real(8), dimension(5) :: ATb
  real(8) :: y_mean, ss_tot, ss_res, R2
  integer :: unit

  ! 打开文件用于写入结果
  open(unit=10, file="fitted_results.txt")
  open(unit=11, file="observed_data.txt")

  ! 输入观测数据
  x = (/1.02, 0.95, 0.87, 0.77, 0.67, 0.56, 0.44, 0.30, 0.16, 0.01/)
  y = (/0.39, 0.32, 0.27, 0.22, 0.18, 0.15, 0.13, 0.12, 0.13, 0.15/)

  ! 构建矩阵A和右端项向量rhs
  n = 10
  do i = 1, n
    A(i, 1) = 1.0
    A(i, 2) = x(i)
    A(i, 3) = y(i)
    A(i, 4) = x(i) * y(i)
    A(i, 5) = y(i) * y(i)
    rhs(i) = x(i) * x(i)
  end do

  ! 计算 ATA 和 ATb
  ATA = matmul(transpose(A), A)
  ATb = matmul(transpose(A), rhs)

  ! 使用 Gauss 消去法求解 ATA * b = ATb
  call gauss_solve(ATA, ATb, 5)

  ! 结果保存在 ATb 中，此时 ATb 就是拟合参数 b
  b = ATb

  ! 计算拟合值
  y_fitted = matmul(A, b)

  ! 计算观测值的平均值
  y_mean = sum(rhs) / n

  ! 计算总平方和 (SS_tot) 和残差平方和 (SS_res)
  ss_tot = sum((rhs - y_mean)**2)
  ss_res = sum((rhs - y_fitted)**2)

  ! 计算 R^2 相关系数
  R2 = 1.0 - ss_res / ss_tot

  ! 打印拟合参数和相关系数
  print *, "Fitted parameters: ", b
  print *, "R-squared: ", R2

  ! 写入拟合参数到文件
  write(10,*) "Fitted parameters:", b

  ! 写入观测数据到文件
  do i = 1, n
    write(11,*) x(i), y(i)
  end do

  ! 关闭文件
  close(10)
  close(11)

contains

  subroutine gauss_solve(A, b, n)
    implicit none
    integer, intent(in) :: n
    real(8), intent(inout) :: A(n,n)
    real(8), intent(inout) :: b(n)
    integer :: i, j, k
    real(8) :: factor

    ! 前向消去
    do k = 1, n-1
      do i = k+1, n
        factor = A(i,k) / A(k,k)
        A(i,k:n) = A(i,k:n) - factor * A(k,k:n)
        b(i) = b(i) - factor * b(k)
      end do
    end do

    ! 回代
    do i = n, 1, -1
      b(i) = (b(i) - sum(A(i,i+1:n) * b(i+1:n))) / A(i,i)
    end do

  end subroutine gauss_solve

end program planet_orbit_fit
