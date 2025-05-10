program matrix_product
  implicit none
  integer, parameter :: n = 20, adj = 4
  integer :: i, j, k, max_prod, prod
  integer :: row, col, drow, dcol
  integer :: matrix(n, n)
  integer :: coords(adj, 2)
  integer :: trow, tcol
  character(len=100) :: line
  character(len=20) :: filename
  open(unit=10, file='../matrix.txt', status='old')
  do i = 1, n
    read(10, '(A)') line
    read(line, *) (matrix(i, j), j = 1, n)
  end do
  close(10)
  ! Find the greatest product and store winning coordinates
  max_prod = 0
  do i = 1, n
    do j = 1, n
      ! Right
      if (j + adj - 1 <= n) then
        prod = 1
        do k = 0, adj - 1
          prod = prod * matrix(i, j + k)
        end do
        if (prod > max_prod) then
          max_prod = prod
          do k = 1, adj
            coords(k,1) = i
            coords(k,2) = j + k - 1
          end do
        end if
      end if
      ! Down
      if (i + adj - 1 <= n) then
        prod = 1
        do k = 0, adj - 1
          prod = prod * matrix(i + k, j)
        end do
        if (prod > max_prod) then
          max_prod = prod
          do k = 1, adj
            coords(k,1) = i + k - 1
            coords(k,2) = j
          end do
        end if
      end if
      ! Diagonal right-down
      if (i + adj - 1 <= n .and. j + adj - 1 <= n) then
        prod = 1
        do k = 0, adj - 1
          prod = prod * matrix(i + k, j + k)
        end do
        if (prod > max_prod) then
          max_prod = prod
          do k = 1, adj
            coords(k,1) = i + k - 1
            coords(k,2) = j + k - 1
          end do
        end if
      end if
      ! Diagonal left-down
      if (i + adj - 1 <= n .and. j - adj + 1 >= 1) then
        prod = 1
        do k = 0, adj - 1
          prod = prod * matrix(i + k, j - k)
        end do
        if (prod > max_prod) then
          max_prod = prod
          do k = 1, adj
            coords(k,1) = i + k - 1
            coords(k,2) = j - k + 1
          end do
        end if
      end if
    end do
  end do

  ! Print the matrix with highlighting (spacing and color exactly as Scala)
  do i = 1, n
    do j = 1, n
      trow = 0
      do k = 1, adj
        if (coords(k,1) == i .and. coords(k,2) == j) trow = 1
      end do
      if (trow == 1) then
        write(*,'(A,I2.2,A,A)', advance='no') char(27)//'[31m', matrix(i,j), char(27)//'[0m', ' '
      else
        write(*,'(I2.2,A)', advance='no') matrix(i, j), ' '
      end if
    end do
    print *
  end do
  print *
  print '(A,I0)', 'Greatest product of four adjacent numbers: ', max_prod
end program matrix_product
