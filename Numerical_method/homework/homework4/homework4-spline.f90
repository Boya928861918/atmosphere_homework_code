! LU分解求矩阵的逆
subroutine LU(xy, xi, c)
    implicit none 
    integer, parameter :: m = 9, n = 5
    real :: b(0:n-1), xi(0:n-1, 0:n-1), xy(0:n-1), c(0:n-1), l(0:n-1, 0:n-1), u(0:n-1, 0:n-1)
    integer :: i, j, k, r

    l = 1
    u = 0

    ! 初始化矩阵
    do i = 0, n-1
        do j = 0, n-1
            if (j>i) then 
                l(i, j) = 0
            end if 
            if (j<i) then 
                u(i, j) = 0
            end if 
        end do 
    end do 

    ! 杜立特尔分解
    do k = 0, n-1
        do j = k, n-1
            u(k, j) = xi(k, j)
            do i = 0, k-1
                u(k, j) = u(k, j) - l(k, i)*u(i, j)
            end do 
        end do 
        do i = k+1, n-1
            l(i, k) = xi(i, k)
            do j = 0, k-1
                l(i, k) = l(i, k) - l(i, j)*u(j, k)
            end do 
            l(i, k) = l(i, k)/u(k, k)
        end do 
    end do 

    print*, l 
    print*, u

    ! 解Ly=b
    do i = 0, n-1
        b(i) = xy(i)
        do j = 0, i-1
            b(i) = b(i) - l(i, j)*b(j)
        end do 
    end do 

    ! 解Ux=y
    do i = n-1, 0, -1
        c(i) = b(i)
        do j = i+1, n-1
            c(i) = c(i) - u(i, j)*c(j)
        end do 
        c(i) = c(i) / u(i, i)
    end do 
    
end subroutine LU

Program main
    implicit none
    integer, parameter :: m = 9, n = 5
    real :: x(0:m), dx(0:m)
    real :: y(0:m), dy(0:m)
    real :: b(0:n-1), xi(0:n-1, 0:n-1), xy(0:n-1), c(0:n-1), c1(0:n-1), l(0:n-1, 0:n-1), u(0:n-1, 0:n-1)
    real :: num, s, x0, y0, Q
    integer :: i, j, k, r

    x = (/1.02, 0.95, 0.87, 0.77, 0.67, 0.56, 0.44, 0.30, 0.16, 0.01/)
    y = (/0.39, 0.32, 0.27, 0.22, 0.18, 0.15, 0.13, 0.12, 0.13, 0.15/)
    dx = (/-0.0029, 0.0007, -0.0082, -0.0038, -0.0041, 0.0026, -0.0001, -0.0058, -0.0005, -0.0034/)
    dy = (/-0.0033, 0.0043, 0.0006, 0.0020, 0.0044, 0.0009, 0.0028, 0.0034, 0.0059, 0.0024/)

    xi = reshape([10.00, sum(x), sum(y), sum(x*y), sum(y**2), sum(x), sum(x**2), sum(x*y), sum(x**2*y), sum(x*y**2), sum(y),&
    sum(x*y), sum(y**2), sum(x*y**2), sum(y**3), sum(x*y), sum(x**2*y), sum(x*y**2), sum(x**2*y**2), sum(x*y**3), sum(y**2),&
    sum(x*y**2), sum(y**3), sum(x*y**3), sum(y**4)], [5, 5])


    xy = [sum(x**2), sum(x**3), sum(x**2*y), sum(x**3*y), sum(x**2*y**2)]

    ! LU分解求矩阵的逆
    c = 0
    call LU(xy, xi, c)
    print*, c

    Q = 0

    do i = 0, m
        x0 = x(i)
        y0 = y(i)
        Q = Q + (c(0)+c(1)*x0+c(2)*y0+c(3)*x0*y0+c(4)*y0**2-x0**2)**2
    end do

    print*, "误差平方和：", Q

    ! 加上偏差重新计算一遍
    c1 = 0
    x = x + dx 
    y = y + dy
    xi = reshape([10.00, sum(x), sum(y), sum(x*y), sum(y**2), sum(x), sum(x**2), sum(x*y), sum(x**2*y), sum(x*y**2), sum(y),&
    sum(x*y), sum(y**2), sum(x*y**2), sum(y**3), sum(x*y), sum(x**2*y), sum(x*y**2), sum(x**2*y**2), sum(x*y**3), sum(y**2),&
    sum(x*y**2), sum(y**3), sum(x*y**3), sum(y**4)], [5, 5])

    xy = [sum(x**2), sum(x**3), sum(x**2*y), sum(x**3*y), sum(x**2*y**2)]
    call LU(xy, xi, c1)
    print*, c1

    Q = 0

    do i = 0, m
        x0 = x(i)
        y0 = y(i)
        Q = Q + (c1(0)+c1(1)*x0+c1(2)*y0+c1(3)*x0*y0+c1(4)*y0**2-x0**2)**2
    end do

    print*, "误差平方和：", Q

END