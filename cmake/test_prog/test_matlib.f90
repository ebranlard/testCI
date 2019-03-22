program test_matlib
    use MatLib
    implicit none
    real(MK) :: m,k,e
    call elliptic_ke(0.0_MK,k,e)

    if ( abs(k-pi/2)<1e-6 ) then
        write(*,'(A)')'[ OK ] Matlib elliptic ke'
    else
        write(*,*) 'Value of pi',pi
        write(*,*) 'Elliptic KE:',k,e
        write(*,'(A)')'[FAIL] Matlib elliptic ke'
    endif

end program
