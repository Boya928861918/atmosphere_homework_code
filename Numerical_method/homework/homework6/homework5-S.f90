Program main
    implicit none
    integer :: i, j, k, n
    real :: x, x0, Si, temp, n0, a, b, f

    temp = 1
    Si = 0
    n = 4
    k = 0
    a = 0
    b = 1
    do while(abs(temp-Si) > 1E-6)
        temp = Si
        n0 = n
        x0 = (b-a)/(n0)
        x = 0
        f = 0
        do x = a, a+(n-1)*x0, x0 
            f = f + 1/sqrt(1+x**3) + 4/sqrt(1+((x+x+x0)/2)**3) + 1/sqrt(1+(x+x0)**3)
        end do 
        Si = (b-a)*f/(6*n0)
        n = n + 1
        k = k + 1
    end do 
    print*, '收敛结果：', Si
    print*, '收敛次数：',k
end