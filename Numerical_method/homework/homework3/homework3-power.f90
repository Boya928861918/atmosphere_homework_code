! 幂法实现程序
subroutine multpuly(a, v, n, v0)
    implicit none 
    integer :: i, j, k, m, n
    real :: a(n, n), v(n, 1), v0(n ,1)
    real :: num, max

    v0 = v
    do i = 1, n 
        num = 0
        
        do j = 1, n
            num = num + a(i, j)*v0(j, 1)
        end do 
        v(i, 1) = num

    end do 

end subroutine


Program main 
    implicit none
    integer, parameter :: n = 5
    real, external :: inner, mods
    real :: a(n, n), v(n, 1), r(n, 1), v0(n ,1), r0(n, 1), a0(n, 1), a1(n, 1), a2(n, 1)
    integer :: i, j, k, m, g
    
    a = reshape([11, -3, -8, 1, 8, -6, 5, 12, 6, -18, 4, -2, -3, -2, 8, -10, 4, 12, 3, -14, -4, 1, 4, -1, -1], [n, n])
    a0 = reshape([1, 2, 1, 2, 1], [n, 1])
    a1 = reshape([1.0, -1.0, 1.0, -1.0, 1.2], [n, 1])
    a2 = reshape([0.65, 0.56, 0.16, -0.34, -0.49], [n, 1])

! 以（1）要求的初始向量计算
    v = a0
    v0 = v
    g = 0
    k = 1

    do i = 1, 10000
        call multpuly(a, v, n, v0)
        r = v / v0
        g = g + 1
        if (abs(r(k, 1) - 5.00000) < 1E-5) then 
            exit
        end if
    end do

    r = v / v0
    print*, r(k, 1)
    print*, 'speed 1:', g

! 以（2）要求的初始向量计算
    v = a1
    v0 = v
    g = 0

    do i = 1, 10000
        call multpuly(a, v, n, v0)
        r = v / v0
        g = g + 1
        if (abs(r(k, 1) - 5.00000) < 1E-5) then 
            exit
        end if
    end do

    r = v / v0
    print*, r(k, 1)
    print*, 'speed 2:', g

! 以（3）要求的初始向量计算
    v = a2
    v0 = v
    g = 0
    
    do i = 1, 10000
        call multpuly(a, v, n, v0)
        r = v / v0
        g = g + 1
        if (abs(r(k, 1) - 5.00000) < 1E-5) then 
            exit
        end if
    end do

    r = v / v0
    print*, r(k, 1)
    print*, 'speed 3:', g

end 
