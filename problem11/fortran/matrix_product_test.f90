program test_matrix_product
  implicit none
  integer, parameter :: n = 4, adj = 4
  integer :: i, j, k, max_prod, prod
  integer :: matrix(n, n)
  integer, dimension(4,4) :: test_data = reshape([ &
    1, 2, 3, 4, &
    5, 6, 7, 8, &
    9, 10, 11, 12, &
    13, 14, 15, 16 ], [4,4])
  max_prod = 0
  ! Copy test_data to matrix
  matrix = test_data
  ! Test right
  do i = 1, n
    do j = 1, n - adj + 1
      prod = 1
      do k = 0, adj - 1
        prod = prod * matrix(i, j + k)
      end do
      if (prod > max_prod) max_prod = prod
    end do
  end do
  ! Test down
  do i = 1, n - adj + 1
    do j = 1, n
      prod = 1
      do k = 0, adj - 1
        prod = prod * matrix(i + k, j)
      end do
      if (prod > max_prod) max_prod = prod
    end do
  end do
  ! Test diag right-down
  do i = 1, n - adj + 1
    do j = 1, n - adj + 1
      prod = 1
      do k = 0, adj - 1
        prod = prod * matrix(i + k, j + k)
      end do
      if (prod > max_prod) max_prod = prod
    end do
  end do
  ! Test diag left-down
  do i = 1, n - adj + 1
    do j = adj, n
      prod = 1
      do k = 0, adj - 1
        prod = prod * matrix(i + k, j - k)
      end do
      if (prod > max_prod) max_prod = prod
    end do
  end do
  if (max_prod == 43680) then
    print *, 'PASS: Greatest product =', max_prod
  else
    print *, 'FAIL: Expected 43680 but got', max_prod
  end if
end program test_matrix_product
