Program main
    implicit none
    integer,parameter :: n = 5
    real :: array(n, n), c(n, n), x(n, n)
    real :: s, num
    integer :: i, j, k, m
    

    array = reshape([6, -3, -8, 1, 8, -6, 0, 12, 6, -18, 4, -2, -8, -2, 8, -10, 4, 12, -2, -14, -4, 1, 4, -1, -6], [n, n])
    c = 0
    c = array

    ! print *, c
    do k = 1, n-1
        s = c(k, k)
        if (abs(s) > 1E-6) then
            do j = 1, n+1
                c(k, j) = c(k, j)/s
            end do
            do i = k+1, n
                num = c(i, k)
                if (abs(num) > 1E-6) then
                    do j = 1, n+1
                        c(i, j) = c(i, j)-c(k, j)*num
                    end do
                end if
            end do
        end if 
        ! do i = k, 
    end do

    do k = n, 1, -1
        s = c(k, k)
        if (abs(s) > 1E-6) then
            do j = 1, n+1
                c(k, j) = c(k, j)/s
                ! print*, c(k, j)
            end do
            do i = k-1, 1, -1  
                num = c(i, k)
                ! print*, num
                if (abs(num)>1E-6) then
                    do j = 1, n+1
                        c(i, j) = c(i, j)-c(k, j)*num
                    end do
                end if
            end do
        end if
    end do

    print*, c
end