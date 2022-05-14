Program main
    implicit none
    integer, parameter :: n=19
    real :: h(0:n), u(n), r(n), m(0:n), d(0:n)
    real :: x(0:n), y(0:n)
    real :: c(0:n, 0:n+1), a(0:n, 0:n)
    real :: s, num
    integer :: i, j, k

    do i = 0, n
        x(i) = i+1
    end do

! 原本是x
    y = (/0.99, 0.95, 0.87, 0.77, 0.67, 0.56, 0.44, 0.30, 0.16, 0.01, -0.2, -0.11, 0.05, 0.16, 0.29, 0.42, 0.59, 0.73, 0.85, 0.93/)
 
    ! y = (/0.39, 0.32, 0.27, 0.22, 0.18, 0.15, 0.13, 0.12, 0.13, 0.15, 0.22, 0.32, 0.36, 0.40, 0.41, 0.42, 0.43, 0.42, 0.41, 0.40/)


    do i = 1, n 
        h(i) = x(i) - x(i-1)
    end do
    h(0) = x(0) - x(19)

    do i = 1, n-1
        u(i) = h(i) / (h(i)+h(i+1))
        r(i) = 1 - u(i)
    end do 
    
    u(n) = h(n) / (h(1)+h(n))
    r(n) = h(1) / (h(1)+h(n))

    do i = 1, n-1
        d(i) = 6 * ((y(i+1)-y(i))/h(i+1) - (y(i)-y(i-1)) /h(i) )/(h(i)+h(i+1))
    end do 
    d(n) = 6 * ( (y(1)-y(n))/h(1) - (y(n)-y(n-1))/h(n) )/(h(1)+h(n))
    d(0) = 0

    a = 0

    a(0, 0) = 1
    ! a(1, 2) = r(1)
    a(0, n) = -1
    do i = 1, n-1
        a(i, i-1) = u(i)
        a(i, i) = 2
        a(i, i+1) = r(i)
    end do 
    a(n, 1) = r(n)
    a(n, n-1) = u(n)
    a(n, n) = 2

    c = 0

    do i = 0, n 
        do j = 0, n
            c(i, j) = a(i, j)
        end do
        c(i, n+1) = d(i)
    end do

    print*, c

! 高斯消元法求矩阵的逆

    do k = 0, n-1
        s = c(k, k)
        if (abs(s) > 1E-6) then
            do j = 1, n+1
                c(k, j) = c(k, j)/s
            end do
            do i = k+1, n
                num = c(i, k)
                if (abs(num) > 1E-6) then
                    print*, num
                    do j = 0, n+1
                        c(i, j) = c(i, j)-c(k, j)*num
                    end do
                end if
            end do
        end if 
    end do

    ! print*, c

    do k = n, 1, -1
        s = c(k, k)
        do j = 0, n+1
            c(k, j) = c(k, j)/s
        end do
        do i = k-1, 0, -1  
            num = c(i, k)
            if (abs(num)>1E-6) then
                do j = 0, n+1
                    c(i, j) = c(i, j)-c(k, j)*num
                end do
            end if
        end do
    end do

    ! print*, c

    do i = 0, n
        m(i) = c(i, n+1)
    end do

    print*, m






end