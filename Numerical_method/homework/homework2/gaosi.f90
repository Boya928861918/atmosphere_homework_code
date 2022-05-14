Program main
    implicit none
    real :: c(3,4)
    real :: s, num
    integer :: i, j, k

    ! 高斯消元法求矩阵的逆
    c = reshape([2, 3, 4, 1, 1, 3, 3, 2, 8, -5, -1, -14],[3,4])

    do k = 1, 2
        s = c(k, k)
        do j = 1, 4
            c(k, j) = c(k, j)/s
        end do
        do i = k+1, 3
            num = c(i, k)
            if (num .ne. 0.00000000) then
                ! print*, num
                do j = 1, 4
                    c(i, j) = c(i, j)-c(k, j)*num
                end do
            end if
        end do
    end do

    ! print*, c

    do k = 3, 2, -1
        s = c(k, k)
        do j = 1, 4
            c(k, j) = c(k, j)/s
        end do
        do i = k-1, 1, -1  
            num = c(i, k)
            if (num .ne. 0.00000000) then
                do j = 1, 4
                    c(i, j) = c(i, j)-c(k, j)*num
                end do
            end if
        end do
    end do

    print*, c
end