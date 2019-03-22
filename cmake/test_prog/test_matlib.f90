program test_matlib
    use MatLib
    implicit none
    real(MK) :: m,k,e
    write(*,*) 'Value of pi',pi
    call elliptic_ke(0.0_MK,k,e)
    write(*,*) 'Elliptic KE:',k,e
end program
