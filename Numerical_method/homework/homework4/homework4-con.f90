! 求阶乘程序
function fac(num)
    implicit none
    integer(kind = 8) :: num, sums
    integer(kind = 8) :: fac

    sums = 1
    do while(num - 0 > 1E-6)
        sums = sums*num
        num = num-1
    end do 

    fac = sums
    return
    
end function

Program main
    implicit none
    real(kind = 8), allocatable :: h(:,:), h_(:,:), temp(:)
    real(kind = 8) :: a, a_, num
    integer(kind = 8), external :: fac
    integer(kind = 8) :: n, i, j, k, m


    do m = 1, 10
        allocate(h(m,m))
        allocate(h_(m,m))
        allocate(temp(m))

        ! 创建希尔伯特矩阵
        do i = 1, m
            do j = 1, m
                num = (i + j - 1)
                h(i, j) = 1 / num
            end do 
        end do 

        ! 计算条件数A1
        do i = 1, m
            temp(i) = sum(abs(h(:, i)))
        end do 

        a = maxval(temp)

        ! 创建希尔伯特矩阵的逆矩阵
        do i = 1, m 
            do j = 1, m 
                h_(i, j) = (-1)**(i+j) * (i+j-1) * fac(m+i-1)/(fac(m-j)*fac(i+j-1))&
                * fac(m+j-1)/(fac(m-i)*fac(i+j-1)) * (fac(i+j-2)/(fac(i-1)*fac(j-1)))**2
            end do 
        end do 

        ! 计算条件数A_1
        do i = 1, m
            temp(i) = sum(abs(h_(:, i)))
        end do 

        a_ = maxval(temp)

        print*, a*a_

        deallocate(h)
        deallocate(h_)
        deallocate(temp)
    end do 
end