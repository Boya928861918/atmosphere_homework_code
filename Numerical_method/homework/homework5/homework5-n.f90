Program main
    implicit none
    real(kind = 4), parameter :: a = 1928332946
    real(kind = 8), parameter :: a0 = 1928332946
    integer :: i, j, k 
    real(kind = 4) :: x, f 
    real(kind = 8) :: x0, f0

    x = 500000.0
    k = 0
    do while(abs(x - sqrt(a)) > 1E-6)
        f = x**2 - a
        x = x - f/(2*x)
        k = k+1
    end do 
    print*, "收敛速度：", k
    print*, "收敛结果：", x
    print*, "内部函数结果：", sqrt(a)


    x0 = 500000.0
    k = 0
    do while(abs(x0 - sqrt(a0)) > 1E-6)
        f0 = x0**2 - a0
        x0 = x0 - f0/(2*x0)
        k = k+1
    end do 
    print*, "收敛速度：", k
    print*, "收敛结果：", x0
    print*, "内部函数结果：", sqrt(a0)

end