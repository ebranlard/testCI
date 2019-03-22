!> 
module BadgerWrapC
    implicit none

contains
    !> Returns 1 if everything ok
    integer(C_INT) function purelib_init(input_file_c) BIND(C,name='purelib_init')
        !
        character(kind=C_CHAR,len=1),dimension(*),intent(in)  ::  input_file_c
        !call cstring2fortran(svar_c, svar)
        !character(len=255)  :: input_file
        write(*,*)'Input file:',input_file_c
    end function

    !> Printing version
    subroutine purelib_version() BIND(C,name='purelib_version')
        write(*,*) 'Version: 0.0'
    end subroutine

end module

