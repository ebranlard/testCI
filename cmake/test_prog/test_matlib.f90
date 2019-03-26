program test_matlib
    use MatLib, only: elliptic_ke, ris_nan, MK, pi
    implicit none
    real(MK) :: m,k,e
    ! Elliptci_ke was put as a dll export, so linking with a shared library will work
    call elliptic_ke(0.0_MK,k,e)

    ! ris_nan was not exported, so linking will fail on windows
    if (ris_nan(k)) then
        write(*,'(A)')'[FAIL] Something wrong with elliptic_ke'
    endif


    if ( abs(k-pi/2)<1e-6 ) then
        write(*,'(A)')'[ OK ] Matlib elliptic ke'
    else
        write(*,*) 'Value of pi',pi
        write(*,*) 'Elliptic KE:',k,e
        write(*,'(A)')'[FAIL] Matlib elliptic ke'
    endif

end program
