!> 
module EllipticIntegrals
    use PrecisionMod, only: MK
    implicit none

    ! For Elliptic pi
    real(MK), dimension(10),parameter :: T=(/.9931285991850949D0,.9639719272779138D0,&
              .9122344282513259D0,.8391169718222188D0,&
              .7463319064601508D0,.6360536807265150D0,&
              .5108670019508271D0,.3737060887154195D0,&
              .2277858511416451D0,.7652652113349734D-1/)
    real(MK), dimension(10),parameter :: W=(/.1761400713915212D-1,.4060142980038694D-1,&
              .6267204833410907D-1,.8327674157670475D-1,&
              .1019301198172404D0,.1181945319615184D0,&
              .1316886384491766D0,.1420961093183820D0,&
              .1491729864726037D0,.1527533871307258D0/)
contains


    !> Complete elliptic integrals of first and second kind
    ! Just like matlab function ellipke
    subroutine elliptic_ke(m,K,E)
        use PrecisionMod, only: precision_equal
        !  m=k^2          K              E
        ! ---------------------------------
        !  .00      1.570796      1.570796
        !  .50      1.854074      1.350643
        ! 1.00       ì            1.000000
        real(MK), intent(in) :: m  !< m=k^2 
        real(MK), intent(out) :: K, E
        ! Variables
        real(MK) :: pk ,ak, bk, ae, be
        pk=1.0d0-m

        !if(m>0.99999_MK) then
        !    ! Using asymptotic expression (cant, phi is not the one from m)
        !    K=log(4./cos(phi))
        !    E=1.+0.5*(K-1.0/1.2) * cos(phi)**2
        !endif
        if (precision_equal(m,1._MK)) then
            k=1.0d+300
            e=1.0d0
        else
            !
            ak=(((.01451196212d0*pk+.03742563713d0)*pk +.03590092383d0)*pk+.09666344259d0)*pk+ 1.38629436112d0
            bk=(((.00441787012d0*pk+.03328355346d0)*pk +.06880248576d0)*pk+.12498593597d0)*pk+.5d0
            k=ak-bk*log(pk)

            ae=(((.01736506451d0*pk+.04757383546d0)*pk+.0626060122d0)*pk+.44325141463d0)*pk+1.0d0
            be=(((.00526449639d0*pk+.04069697526d0)*pk+.09200180037d0)*pk+.2499836831d0)*pk
            e=ae-be*log(pk)
        endif
    end subroutine 

    !> Compute the elliptic integral of the third kind using Gauss-Legendre quadrature
    ! Just like matlab function ellipticPi(n,phi,m) BUT PHI IN DEGREES
    ! Use phi=90 for complete elliptic integral Pi
    subroutine elliptic_pi(n,phi,m,EL3)
        use PrecisionMod, only: precision_equal
        real(MK),intent(in) :: n !< parameter [0 1]
        real(MK),intent(in) :: phi !< argument in degrees
        real(MK),intent(in) :: m !< modulus [0 1] 
        real(MK),intent(out) :: EL3   !< Elliptic integral value Pi
        integer :: i
        real(MK) :: c0,c1,c2,t1,t2,f1,f2
        real(MK) :: k,c
        logical :: lb1,lb2
        k   =sqrt(m)
        c=n

        lb1= precision_equal(k,1.0_MK)  .and. abs(phi-90.0).le.1.0d-8
        lb2= precision_equal(c,1.0_MK)  .and. abs(phi-90.0).le.1.0d-8
        if (lb1.or.lb2) then
            el3=1.0d+300
            return
        endif
        c1=0.87266462599716d-2*phi
        c2=c1
        el3=0.0d0
        do i=1,10
           c0=c2*T(i)
           t1=c1+c0
           t2=c1-c0
           f1=1.0d0/((1.0d0-c*dsin(t1)*dsin(t1))*dsqrt(1.0d0-k*k*dsin(t1)*dsin(t1)))
           f2=1.0d0/((1.0d0-c*dsin(t2)*dsin(t2))*dsqrt(1.0d0-k*k*dsin(t2)*dsin(t2)))
           el3=el3+W(i)*(f1+f2)
        enddo
        el3=c1*el3
    end subroutine

    !subroutine elliptic_ke_c(m,k,e)                           !COMPAQ-COMPILER
    subroutine elliptic_ke_c(m,k,e) bind(C,name='elliptic_ke')!OTHER-COMPILER
        use PrecisionMod, only: C_DOUBLE
        real(C_DOUBLE),intent(in)   :: m !< 
        real(C_DOUBLE), intent(out) :: k !< 
        real(C_DOUBLE), intent(out) :: e !< 
        real(MK)    :: k_MK      !< 
        real(MK)    :: e_MK      !< 
        call elliptic_ke(m,k_MK,e_MK)
        k=real(k_MK,C_DOUBLE)
        e=real(e_MK,C_DOUBLE)
    end subroutine 

    !subroutine elliptic_pi_c(n,phi,m,pi)                           !COMPAQ-COMPILER
    subroutine elliptic_pi_c(n,phi,m,pi) bind(C,name='elliptic_pi')!OTHER-COMPILER
        use PrecisionMod, only: C_DOUBLE
        real(C_DOUBLE),intent(in)    :: n     !< 
        real(C_DOUBLE),intent(in)    :: phi     !< 
        real(C_DOUBLE),intent(in)    :: m     !< 
        real(C_DOUBLE), intent(out)    :: pi      !< 
        real(MK)    :: pi_MK      !< 
        call elliptic_pi(n,phi,m,pi_MK)
        pi=real(pi_MK,C_DOUBLE)
    end subroutine 

end module EllipticIntegrals
