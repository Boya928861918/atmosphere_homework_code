Program main
    implicit none 
    real(kind = 8), allocatable :: h(:,:), x0(:), b(:), x(:)
    real(kind = 8) :: num, max
    integer(kind = 8) :: n, i, j, k, m
    integer :: t(4)

    t = (/6, 8, 10, 15/)
    do n = 1, 4
        m = t(n)
        allocate(h(m,m))
        allocate(x0(m))
        allocate(b(m))
        allocate(x(m))
        x0 = 1

        ! 创建希尔伯特矩阵
        do i = 1, m
            do j = 1, m
                num = (i + j - 1)
                h(i, j) = 1 / num
            end do 
        end do 
        
        ! 设定Ax=b中的矩阵b
        do i = 1, m 
            b(i) = sum(h(i,:))
        end do 

        ! 迭代过程
        x = 0
        k = 0
        do while(maxval(abs(x-x0)) > 1E-4)
            do i = 1, m 
                num = 0
                do j = 1, m 
                    if (i .ne. j) then 
                        num = num + h(i, j) * x(j)
                    end if
                end do 
                x(i) = (b(i) - num ) / h(i, i)
            end do 

            ! 计数
            k = k+1

        end do 

        print*, "迭代次数：", k
        print*, "迭代精度：", abs(maxval(x) - 1)
        print*, "迭代结果：", x

        deallocate(h)
        deallocate(x0)
        deallocate(b)
        deallocate(x)
    end do 
end 
