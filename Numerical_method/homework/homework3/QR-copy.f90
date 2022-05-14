! 求内积程序
function inner(a, r, n)
    implicit none
    integer :: n, i
    real :: a(n), r(n)
    real :: inner, num
    num = 0

    do i = 1, n
        num = num + a(i)*r(i)
    end do
    inner = num
    return
end function

! 求模程序
function mods(a, n)
    implicit none
    real :: a(n)
    real :: mods, num
    integer :: i, n
    num = 0
    do i = 1, n 
        num = num + a(i) ** 2
    end do 
    mods = sqrt(num)
    return
end function

! QR法实现程序
subroutine QR(a, n)
    implicit none
    real, external :: inner, mods
    integer :: n, i, j, k
    real :: a(n, n), b(n, n), r(n, n), c(n ,n), inner_num(n)

    ! 初始化矩阵第一列
    b(1:n, 1) = a(1:n, 1)
    r(1:n, 1) = b(1:n, 1)/mods(b(1:n, 1), n)
    do i = 2, n
        inner_num = 0
        do j = 2, i 
            inner_num = inner_num + inner(a(1:n ,i), r(1:n, j-1), n) * r(1:n , j-1)
        end do 
        b(1:n, i) = a(1:n, i) - inner_num
        r(1:n, i) = b(1:n, i)/mods(b(1:n, i), n)
    end do

    ! 将算出的beta和r填入矩阵R中
    do i = 1, n 
        c(i, i) = mods(b(1:n ,i), n)
        if (i < n) then
            do j = i+1, n 
                c(i, j) = inner(a(1:n, j), r(1:n, i), n)
            end do 
        end if 
    end do

    a = matmul(c, r)
end subroutine

! 解方程程序
subroutine solve(array, n, m, x)
    real :: array(n, n), c(n, n), x(n, n)
    real :: s, num, max, temp
    integer :: i, j, k, n, m, count, max_count

    c = 0
    c = array

    ! print *, c
    do k = 1, n
        count = k
        max_count = k
        max = c(k, k)
        do i = k, n
            if (max < c(i, k)) then
                max = c(i, k)
                max_count = count
            end if 
            count = count + 1
        end do 

        s = max
        ! print*, s
        if (abs(s) > 1E-9) then
            do i = k, n
                num = c(i, k)/c(max_count, k)
                if (i .ne. max_count) then
                    do j = 1, n
                        c(i, j) = c(i, j) - c(max_count, j) * num
                    end do
                end if
            end do
        end if 
        do j = 1, n
            temp = c(max_count, j)
            c(max_count, j) = c(k, j)
            c(k, j) = temp
        end do 
        ! print*, c
    end do

    ! do k = n, 2, -1
    !     s = c(k, k)
    !     if (abs(s) > 1E-6) then
    !         do i = k-1, 1, -1  
    !             num = c(i, k) / c(k, k)
    !             ! print*, num
    !             if (abs(c(i, k))>1E-6) then
    !                 do j = 1, n
    !                     c(i, j) = c(i, j) - c(k, j)*num
    !                 end do
    !             end if
    !         end do
    !     end if
    ! end do

    x(1:n, m) = c(1:n, 5)
    print*, c

end subroutine 


Program main
    implicit none
    integer, parameter :: n = 5
    real, external :: inner, mods
    real :: a(n, n), b(n), E(n, n), x(n, n), array(n, n), a_(n, n)
    integer :: i, j, k, m

    a = reshape([11, -3, -8, 1, 8, -6, 5, 12, 6, -18, 4, -2, -3, -2, 8, -10, 4, 12, 3, -14, -4, 1, 4, -1, -1], [n, n])
    ! a_ = a
    ! E = 0

    ! do i = 1, n 
    !     E(i, i) = 1
    ! end do 

    do k = 1, 1000
        call QR(a, n)
    end do

    ! x = 0
    ! print*, a
    ! do i = 1, n
    !     m = i
    !     array = a_ - a(m, m)*E
    !     print*, a(m, m)
    !     call solve(array, n, m, x)
    ! end do 

    
    do i = 1, n
        print *, a(i, i)
        ! print *, x(1:n, i)
    end do

end