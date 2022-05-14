Program main
    implicit none
    integer,parameter :: n = 5
    real :: array(n, n), c(n, n), x(n), o(n)
    real :: s, num
    integer :: i, j, k, m
    

    array = reshape([11, -3, -8, 1, 8, -6, 5, 12, 6, -18, 4, -2, -3, -2, 8, -10, 4, 12, 3, -14, -4, 1, 4, -1, -1], [n, n])
    o = (/8.0000, -4.32000017, -1.3333397, 1.0000, 0.0000/)
    c = array

    do i = 1, n
        num = 0
        do j = 1, n 
            num = num + array(i, j) * o(j)
        end do 
        x(i) = num
    end do 
        

    print *, x
    
end