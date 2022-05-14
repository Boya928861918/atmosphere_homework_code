Program main
    implicit none 
    real(kind = 8), allocatable :: h(:,:), x0(:), b(:), x(:)
    real(kind = 8) :: num, max
    integer(kind = 8) :: n, i, j, k, m
    integer :: t(4)

    ! t = (/6, 8, 10, 15/)
    ! do n = 1, 4
        m = 3
        allocate(h(m,m))
        allocate(x0(m))
        allocate(b(m))
        allocate(x(m))
        x0 = (/11.0, 12.0, 13.0/)

        ! 创建希尔伯特矩阵
        h = reshape([10, -1, -1, -1, 10, -1, -2, -2, 5], [3, 3])
        
        b = (/72, 83, 42/)

        x = 0
        k = 0
        do while(abs(maxval(x) - 13) > 1E-6)
            do i = 1, m 
                num = 0
                do j = 1, m 
                    if (i .ne. j) then 
                        num = num + h(i, j) * x(j)
                    end if
                end do 
                x(i) = (b(i) - num ) / h(i, i)
            end do 

            k = k+1

        end do 

        print*, k
        print*, x

        deallocate(h)
        deallocate(x0)
        deallocate(b)
        deallocate(x)
    ! end do 
end 
