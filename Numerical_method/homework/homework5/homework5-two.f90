Program main
    implicit none
    real(kind = 4), parameter :: a = 1928332946
    real(kind = 8), parameter :: a0 = 1928332946
    integer :: i, j, k 
    real(kind = 4) :: l, r, x
    real(kind = 8) :: l0, r0, x0

    l = 1.0
    r = 1000000
    k = 0
    x = (l+r) / 2
    do while(abs(x - sqrt(a)) > 1E-6)
        x = (l+r) / 2
        if ((x**2 - a) < 0) then
            l = x 
        else
            r = x
        end if 
        k = k+1
    end do 

    print*, "收敛速度：", k
    print*, "收敛结果：", x
    print*, "内部函数结果：", sqrt(a)

    l0 = 1.0
    r0 = 1000000
    k = 0
    x0 = (l0+r0) / 2
    do while(abs(x0 - sqrt(a0)) > 1E-6)
        x0 = (l0+r0) / 2
        if ((x0**2 - a0) < 0) then
            l0 = x0
        else
            r0 = x0
        end if 
        k = k+1
    end do 

    print*, "收敛速度：", k
    print*, "收敛结果：", x0
    print*, "内部函数结果：", sqrt(a0)

end