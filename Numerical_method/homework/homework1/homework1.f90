! 第一种方法：双精度顺序相加
function way_one(randGroup, n)
    implicit none
    integer :: n, counter_1
    REAL(kind=8) :: randGroup(n)
    REAL(kind=8) :: way_one
    REAL(kind=8) :: num_1
    num_1 = 0
    do  counter_1 = 1, n
        num_1 = num_1 + randGroup(counter_1)
    end do
    way_one = num_1
    return
end

! 第二种方法：单精度顺序相加
function way_two(single_randGroup, n)
    implicit none
    integer :: n, counter_2
    REAL(kind=4) :: single_randGroup(n)
    REAL(kind=4) :: way_two
    REAL(kind=4) :: num_2
    num_2 = 0
    do  counter_2 = 1, n
        num_2 = num_2 + single_randGroup(counter_2)
    end do
    way_two = num_2
    return
end

! 第三种方法：按照题目给出的算法顺序相加
function way_three(single_randGroup, n)
    implicit none
    integer :: n, counter_3
    REAL(kind=4) :: single_randGroup(n)
    REAL(kind=4) :: way_three
    REAL(kind=4) :: s, c, t, y
    s = single_randGroup(1)
    c = 0
    do counter_3 = 2, n
        y = single_randGroup(counter_3)-c
        t = s+y
        c = (t-s)-y
        s = t
    end do
    way_three = s
    return
end

! 第四种方法：从大到小排列后相加
function way_four(single_randGroup, n)
    implicit none
    integer :: n, i, j, max, counter_4
    REAL :: single_randGroup(n), randGroup_(n)
    REAL(kind=4) ::way_four, temp, num_4, k

    randGroup_ = single_randGroup
    ! 此处采用选择排序法
    do i = 1, n-1
        ! 暂定i是最大值
        max = i
        do j = i+1, n
            ! 通过逐个比较找出实际的最大值
            if (randGroup_(max)<randGroup_(j)) then
                max = j
            end if
        end do
        ! 交换max和i的位置
        temp = randGroup_(max)
        randGroup_(max) = randGroup_(i)
        randGroup_(i) = temp
    end do
    num_4 = 0
    ! 顺序相加
    do  counter_4 = 1, n
        num_4 = num_4 + randGroup_(counter_4)
    end do
    way_four = num_4
    return
end

! 第五种方法：从小到大排列后相加
function way_five(single_randGroup, n)
    implicit none
    integer :: n, i, j, min, counter_5
    REAL :: single_randGroup(n), f_randGroup_(n)
    REAL(kind=4) ::way_five, temp, num_5

    f_randGroup_ = single_randGroup
    ! 此处采用选择排序法
    do i = 1, n-1
        ! 暂定i是最大值
        min = i
        do j = i+1, n
            ! 通过逐个比较找出实际的最大值
            if (f_randGroup_(min)>f_randGroup_(j)) then
                min = j
            end if
        end do
        ! 交换min和i的位置
        temp = f_randGroup_(min)
        f_randGroup_(min) = f_randGroup_(i)
        f_randGroup_(i) = temp
    end do
    num_5 = 0
    ! 顺序相加
    do  counter_5 = 1, n
        num_5 = num_5 + f_randGroup_(counter_5)
    end do
    way_five = num_5
    return
end

Program main
    implicit none

    integer :: n, counter
    REAL(kind=4) :: way_two_num, way_three_num, way_four_num, way_five_num
    REAL(kind=8) :: randNum, way_one_num

    REAL(kind=4), external :: way_two, way_three, way_four, way_five
    REAL(kind=8), external :: way_one

    REAL(kind=8), allocatable :: randGroup(:)
    REAL(kind=4), allocatable :: single_randGroup(:)

    ! 设定产生n个数
    write(*, *) "Please input the numbers you want to count:"
    read(5, *) n

    ! 设定可变数组大小
    allocate(randGroup(n))
    allocate(single_randGroup(n))

    ! 为数组填入双精度随机数
    do counter = 1, n, 1
        call random_number(randNum)
        randGroup(counter) = randNum
        single_randGroup(counter) = real(randNum, kind=4)
    end do

    ! 采用第一种方法计算值
    way_one_num = way_one(randGroup, n)
    write(*, *) "way one's answer is :",way_one_num

    call cpu_time(real_time)
    ! 采用第二种方法计算值
    way_two_num = way_two(single_randGroup, n)
    write(*, *) "way two's answer is :",way_two_num

    ! 采用第三种方法计算值
    way_three_num = way_three(single_randGroup, n)
    write(*, *) "way three's answer is :",way_three_num

    ! 采用第四种方法计算值
    way_four_num = way_four(single_randGroup, n)
    write(*, *) "way four's answer is :",way_four_num

    ! 采用第五种方法计算值
    way_five_num = way_five(single_randGroup, n)
    write(*, *) "way five's answer is :",way_five_num

    deallocate(randGroup)
END
