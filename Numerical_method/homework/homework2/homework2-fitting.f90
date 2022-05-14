Program main
    implicit none
    integer, parameter :: m = 19
    real :: x(0:m)
    real :: y(0:m)
    real :: b(5), xi(5, 5), xy(5), c(5, 6)
    real :: num, s, x0, y0, Q
    integer :: n, i, j, k

    x = (/0.99, 0.95, 0.87, 0.77, 0.67, 0.56, 0.44, 0.30, 0.16, 0.01, -0.2, -0.11, 0.05, 0.16, 0.29, 0.42, 0.59, 0.73, 0.85, 0.93/)
    y = (/0.39, 0.32, 0.27, 0.22, 0.18, 0.15, 0.13, 0.12, 0.13, 0.15, 0.22, 0.32, 0.36, 0.40, 0.41, 0.42, 0.43, 0.42, 0.41, 0.40/)

    xi = reshape([20.00, sum(x), sum(y), sum(x*y), sum(y**2), sum(x), sum(x**2), sum(x*y), sum(x**2*y), sum(x*y**2), sum(y),&
    sum(x*y), sum(y**2), sum(x*y**2), sum(y**3), sum(x*y), sum(x**2*y), sum(x*y**2), sum(x**2*y**2), sum(x*y**3), sum(y**2),&
    sum(x*y**2), sum(y**3), sum(x*y**3), sum(y**4)], [5, 5])


    xy = [sum(x**2), sum(x**3), sum(x**2*y), sum(x**3*y), sum(x**2*y**2)]

    ! print*, xi

    ! 高斯消元法求矩阵的逆
    c = 0
    ! print*, c

    do i = 1, 5
        do j = 1, 5
            c(i, j) = xi(i, j)
        end do
        c(i, 6) = xy(i)
    end do

    ! print*, c

    do k = 1, 4
        s = c(k, k)
        do j = 1, 6
            c(k, j) = c(k, j)/s
        end do
        do i = k+1, 5
            num = c(i, k)
            if (num .ne. 0.00000000) then
                ! print*, num
                do j = 1, 6
                    c(i, j) = c(i, j)-c(k, j)*num
                end do
            end if
        end do
    end do

    ! print*, c

    do k = 5, 2, -1
        s = c(k, k)
        do j = 1, 6
            c(k, j) = c(k, j)/s
        end do
        do i = k-1, 1, -1  
            num = c(i, k)
            if (num .ne. 0.00000000) then
                do j = 1, 6
                    c(i, j) = c(i, j)-c(k, j)*num
                end do
            end if
        end do
    end do

    do i = 1, 5
        b(i) = c(i, 6) 
    end do 

    print*, b

    Q = 0

    do i = 0, m
        x0 = x(i)
        y0 = y(i)
        Q = Q + (b(1)+b(2)*x0+b(3)*y0+b(4)*x0*y0+b(5)*y0**2-x0**2)**2
    end do

    print*, Q

END