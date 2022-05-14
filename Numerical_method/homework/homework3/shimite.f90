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
    real :: a(n), b(n), r(n), c(n), inner_num(n)

    ! 初始化矩阵第一列
    b(1:n, 1) = a(1:n, 1)
    r(1:n, 1) = b(1:n, 1)/mods(b(1:n, 1), n)
    do i = 2, n
        inner_num = 0
        inner_num = inner_num + inner(a(1:n ,i), r(1:n, j-1), n) * r(1:n , j-1)
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

Program main
    implicit none
    integer, parameter :: n = 5
    real, external :: inner, mods
    real :: a(n), b(n), E(n), x(n), array(n), a_(n)
    integer :: i, j, k, m

    a = (/0.16222142, 0.187666, 0.0543719, -0.39799566, 0.04201456/)

    call QR(a, n)

    print *, a(i, i)

end