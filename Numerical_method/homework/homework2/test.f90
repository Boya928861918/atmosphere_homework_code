Program main
    implicit none

    integer, parameter :: n = 4
    real :: x(0:n)
    real :: y(0:n)
    real :: u(n), r(n), h(n), d(n)
    real :: a(n,n), c(n, n+1), m(n)
    real :: num, s, counts
    integer :: i, j, k

    x = (/0.00, 1.57, 3.14, 4.71, 6.28/)
    y = (/0.00, 1.00, 0.00, -1.00, 0.00/)

    ! 样条插值法
    do i = 1, n
        h(i) = x(i)-x(i-1)
    end do

    do i = 1, n-1
        u(i) = h(i)/(h(i)+h(i+1))
        r(i) = h(i+1)/(h(i)+h(i+1))
    end do
    ! 设置边界条件
    u(n) = -h(n)/(h(n)+h(1))
    r(n) = h(1)/(h(1)+h(n))
    
    ! d(1) = 6*((y(2)-y(1))/h(2)-(y(1)-y(2))/h(1))/(h(2)+h(1))
    do i = 1, n-1
        d(i) = 6*((y(i+1)-y(i))/h(i+1)-(y(i)-y(i-1)/h(i)))/(h(i)+h(i+1))
    end do
    ! 设置边界条件
    d(n) = 6* ( (y(1)-y(0))/h(1)  - (y(n)-y(n-1))/h(n) ) / (h(n)+h(1))

    ! print*, d(n)

    do i = 1, n
        do j = 1, n
            a(i ,j) = 0
        end do 
    end do

    ! 求系数矩阵
    a(1, 1) = 2
    a(1, 2) = r(1)
    a(1, n) = u(1)
    do i = 2, n-1
        a(i, i-1) = u(i+1)
        a(i, i) = 2
        a(i, i+1) = r(i+1)
    end do
    a(n, 1) = r(n)
    a(n, n-1) = u(n)
    a(n, n) = 2
    
    ! print*, a

    ! 高斯消元法求矩阵的逆
    do i = 1, n
        do j = 1, n+1
            c(i, j) = 0
        end do
    end do 
    ! print*, c

    do i = 1, n
        do j = 1, n
            c(i, j) = a(i, j)
        end do
        c(i, n+1) = d(i)
    end do

    print*, c

    do k = 1, n-1
        s = c(k, k)
        do j = 1, n+1
            c(k, j) = c(k, j)/s
        end do
        do i = k+1, n
            num = c(i, k)
            if (abs(num) > 1E-6) then
                ! print*, num
                do j = 1, n+1
                    c(i, j) = c(i, j)-c(k, j)*num
                end do
            end if
        end do
    end do

    ! print*, c

    do k = n, 2, -1
        s = c(k, k)
        do j = 1, n+1
            c(k, j) = c(k, j)/s
        end do
        do i = k-1, 1, -1  
            num = c(i, k)
            if (abs(num)>1E-6) then
                do j = 1, n+1
                    c(i, j) = c(i, j)-c(k, j)*num
                end do
            end if
        end do
    end do

    print*, c

    ! do i = 1, 19
    !     do j = 1, 19
    !         t(i, j) = c(i, j+19)
    !     end do
    ! end do

    ! print*, t
    
    ! do i = 1, 19
    !     counts = 0
    !     do j = 1, 19
    !         counts = counts+t(i, j)*d(j)
    !     end do 
    !     m(i) = counts
    ! end do

    ! print*, m

    do i = 1, n
        m(i) = c(i, n+1)
    end do

    print*, m
END


