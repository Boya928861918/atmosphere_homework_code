Program main
    implicit none
    integer :: i, j, k, n
    real(kind = 8) :: t, t0, Si, n0, a, b, f
    real(kind = 8) :: temp(8), x0(8)

    temp = (/1, 1, 2, 6, 24, 120, 720, 5040/)
    x0 = (/1, 2, 3, 4, 5, 6, 7, 8/)
    
    k = 0
    a = 0
    b = 1E4
    n = 1E4
    do i = 1, 8
        Si = 0
        do while(abs(temp(i)-Si) > 1E-6)
            n0 = n
            t0 = (b-a)/(n0)
            t = 0
            f = 0
            do t = a, a+(n-1)*t0, t0 
                f = f + t**(x0(i)-1)*exp(-t) + 4*((t+t+t0)/2)**(x0(i)-1)*exp(-((t+t+t0)/2)) + (t+t0)**(x0(i)-1)*exp(-(t+t0))
            end do 
            Si = (b-a)*f/(6*n0)
            n = n + 1
            k = k + 1
        end do 
        print*, '收敛结果：', Si
        print*, '收敛次数：', k
    end do
end