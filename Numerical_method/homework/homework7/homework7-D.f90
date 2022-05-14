program main
    implicit none 
    real(kind = 8) :: u(17, 17), v(17, 17), ux(17, 17), vy(17, 17), D(17, 17)
    real(kind = 8) :: S(0:18, 0:18), S0(0:18, 0:18), R(0:18, 0:18), y(0:18, 0:18)
    real(kind = 8) :: max_num
    integer :: i, j, k, m, n

    open(unit=4, file='u.txt', recl = 6)
    open(unit=11, file='v.txt', recl = 6)
    do j = 1, 17
        do i = 1, 17
            read(4, *) u(i ,j)
            read(11, *) v(i ,j)
        end do
    end do
    close(4)
    close(11)
    ! 中心差分
    do i = 2, 16
        do j = 2, 16
            ux(i, j) = (u(i, j-1) + u(i, j+1)) / (2*0.25)
            vy(i, j) = (v(i-1, j) + v(i+1, j)) / (2*0.25)
        end do 
    end do 

    do j = 1, 16
        ! 向前差分
        ux(1, j) = (u(1, j) + u(1, j+1)) / (0.25)
        ! 向后差分
        ux(17, j+1) = (u(17, j) + u(17, j+1)) / (0.25)
        ! 向前差分
        vy(1, j) = (v(1, j) + v(2, j)) / (0.25)
        ! 向后差分
        vy(17, j+1) = (v(17, j+1) + v(16, j+1)) / (0.25)
    end do 
    do i = 1, 16
        ! 向前差分
        ux(i+1, 1) = (u(i+1, 1) + u(i+1, 2)) / (0.25)
        ! 向后差分
        ux(i, 17) = (u(i, 17) + u(i, 16)) / (0.25)
        vy(i+1, 1) = (v(i, 1) + v(i+1, 1)) / (0.25)
        ! 向前差分
        vy(i, 17) = (v(i, 17) + v(i+1, 17)) / (0.25)
    end do
    D = ux + vy
    
    S = 0
    S0 = 10
    k = 1
    max_num = 1
    do while (max_num > 1E-7)
        S0 = S
        do i = 1, 17
            do j = 1, 17
                R(i, j) = (S(i+1, j) + S(i, j+1) + S(i-1, j) + S(i, j-1) - 4 * S(i, j))/(0.25**2) + D(i, j)
                S(i, j) = S(i, j) + 0.4 * (0.25**2) * R(i, j)
            end do 
        end do 
        k = k+1
        y = S-S0
        max_num = abs(y(1, 1))
        do m = 1, 17
            do n = 1, 17
                if (max_num < abs(y(m, n))) then
                    max_num = abs(y(m, n))
                end if 
            end do 
        end do 
    end do

    ! 中心差分
    do i = 2, 16
        do j = 2, 16
            u(i, j) = -(S(i, j-1) + S(i, j+1)) / (2*0.25)
            v(i, j) = -(S(i-1, j) + S(i+1, j)) / (2*0.25)
        end do 
    end do 

    do j = 1, 16
        u(1, j) = -(S(1, j) + S(1, j+1)) / (0.25)
        u(17, j+1) = -(S(17, j) + S(17, j+1)) / (0.25)
        v(1, j) = -(S(1, j) + S(2, j)) / (0.25)
        v(17, j+1) = -(S(17, j+1) + S(16, j+1)) / (0.25)
    end do 
    do i = 1, 16
        u(i+1, 1) = -(S(i+1, 1) + S(i+1, 2)) / (0.25)
        u(i, 17) = -(S(i, 17) + S(i, 16)) / (0.25)
        v(i+1, 1) = -(S(i, 1) + S(i+1, 1)) / (0.25)
        v(i, 17) = -(S(i, 17) + S(i+1, 17)) / (0.25)
    end do

    do i = 1, 17
        print*, S(i, :)
    end do
    print*, '----------------------------------------------------------------------------------------'
    do i = 1, 17
        print*, u(i, :)
    end do
    print*, '----------------------------------------------------------------------------------------'
    do i = 1, 17
        print*, v(i, :)
    end do
end