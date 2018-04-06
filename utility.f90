module mod_utility
  
  implicit none
    real(8) :: p_leispref, p_fixcost, p_leisprefbad, p_gamh, p_gamc, p_conspref, p_onemgamc
    ! for separable utility function
    integer(2) :: nonsep

  contains

    subroutine U(n, c, h, particip, badheal, tol, utils)

      implicit none
            integer(8), intent(in) :: n
            real(8), intent(in) :: c(n)
            real(8), intent(in) :: h(n)
            real(8), intent(in) :: particip(n)
            real(8), intent(in) :: badheal(n)
            real(8), intent(in) :: tol
            real(8), intent(out) :: utils(n)

            real(8) :: leisure(n)
            real(8) :: within(n)

            if (nonsep == 1) then
                leisure = p_leispref - h - ((p_fixcost*particip) + (p_leisprefbad*badheal))
                within = (c**p_gamh) * (leisure**(1-p_gamh))

                if (abs(p_gamc - 1.0_8) < tol) then
                    utils = p_conspref * log(within)
                else
                    utils = p_conspref * (within**(1-p_gamc)) * p_onemgamc
                end if
            else
                within = c**(1-p_gamc)*p_onemgamc
                leisure = 5280-h-((p_fixcost*particip) + (p_leisprefbad*badheal))
                leisure = leisure**(1-p_gamh)*p_onemgamc
                utils = p_conspref*(within + (p_leispref*leisure))
            end if

    end subroutine U
end module mod_utility
