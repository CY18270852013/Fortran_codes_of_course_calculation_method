program orbit_fitting
  implicit none
  integer, parameter :: n = 10, p = 5
  real(8) :: A(n,p), X(n), b(p), AtA(p,p), AtX(p)
  real(8) :: x_vals(n), y_vals(n)  ! 更改变量名避免冲突
  integer :: i, j, k
  
  ! 观测值
  data x_vals /1.02, 0.95, 0.87, 0.77, 0.67, 0.56, 0.44, 0.30, 0.16, 0.01/
  data y_vals /0.39, 0.32, 0.27, 0.22, 0.18, 0.15, 0.13, 0.12, 0.13, 0.15/

  ! 构造A矩阵和X向量
  do i = 1, n
    A(i,1) = 1.0d0
    A(i,2) = x_vals(i)
    A(i,3) = y_vals(i)
    A(i,4) = x_vals(i)*y_vals(i)
    A(i,5) = y_vals(i)**2
    X(i) = x_vals(i)**2
  end do

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

  ! 输出拟合参数
  print *, 'Fitted parameters:'
  print *, b

contains
  ! 高斯消元法
  subroutine gauss_solve(A, B, X, n)
    real(8), intent(inout) :: A(n,n), B(n)
    real(8), intent(out) :: X(n)
    integer, intent(in) :: n  ! 明确声明 n 为整型
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

end program orbit_fitting
