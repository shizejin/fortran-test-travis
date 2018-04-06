program test_utility

    use mod_utility

    implicit none

    integer(8) :: n, i
    real(8), allocatable :: c(:), h(:), particip(:), badheal(:)
    real(8), allocatable :: utils(:)
    real(8) :: tol

    p_leispref = 3399.0_8
    p_fixcost = 240.0_8
    p_leisprefbad = 202.0_8
    p_gamh = 0.615_8
    p_gamc = 7.69_8
    p_conspref = 100000000.0_8
    p_onemgamc = 1.0_8 / (1.0_8 - p_gamc)

    tol = 1e-6_8

    ! read the data
    ! the first line indicates the number of obs
    ! and whether the utility is separable
    read(*, *) n, nonsep
    allocate(c(n))
    allocate(h(n))
    allocate(particip(n))
    allocate(badheal(n))
    allocate(utils(n))

    do i = 1, n
        read(*, *) c(i), h(i), particip(i), badheal(i)
    end do

    call U(n, c, h, particip, badheal, tol, utils)

    print*, utils

end program

