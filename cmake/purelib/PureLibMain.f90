!> 
module PureLibMain
    use iso_c_binding
    implicit none

    character(len=35), parameter :: VERSION = 'v1.1-12.g226d26f'

contains
    !> Get Double
    real(C_DOUBLE) function purelib_getdb() BIND(C,name='purelib_getdb')
        !DEC$ IF .NOT. DEFINED(__LINUX__)
        !DEC$ ATTRIBUTES DLLEXPORT :: purelib_getdb
        !GCC$ ATTRIBUTES DLLEXPORT :: purelib_getdb
        !DEC$ ENDIF
        purelib_getdb=12.12_C_DOUBLE
    end function

    !> Get int
    integer(C_INT) function purelib_getint() BIND(C,name='purelib_getint')
        !DEC$ IF .NOT. DEFINED(__LINUX__)
        !DEC$ ATTRIBUTES DLLEXPORT :: purelib_getint
        !GCC$ ATTRIBUTES DLLEXPORT :: purelib_getint
        !DEC$ ENDIF
        purelib_getint=12
    end function

    !> Getting version
    subroutine purelib_get_version(s_c) BIND(C,name='purelib_get_version')
        !DEC$ IF .NOT. DEFINED(__LINUX__)
        !DEC$ ATTRIBUTES DLLEXPORT :: purelib_get_version
        !GCC$ ATTRIBUTES DLLEXPORT :: purelib_get_version
        !DEC$ ENDIF
        character(kind=C_CHAR,len=1),dimension(*), intent(inout) :: s_c
        call fortranstring2c(VERSION,s_c)
    end subroutine

    !> Printing version
    subroutine purelib_print_version() BIND(C,name='purelib_print_version')
        !DEC$ IF .NOT. DEFINED(__LINUX__)
        !DEC$ ATTRIBUTES DLLEXPORT :: purelib_print_version
        !GCC$ ATTRIBUTES DLLEXPORT :: purelib_print_version
        !DEC$ ENDIF
        write(*,*) 'Version: ',VERSION
    end subroutine

    !> Init
    integer(C_INT) function purelib_init(input_file_c) BIND(C,name='purelib_init')
        !DEC$ IF .NOT. DEFINED(__LINUX__)
        !DEC$ ATTRIBUTES DLLEXPORT :: purelib_init
        !GCC$ ATTRIBUTES DLLEXPORT :: purelib_init
        !DEC$ ENDIF
        character(kind=C_CHAR,len=1),dimension(*), intent(in)  ::  input_file_c
        character(len=255)  :: input_file
        call cstring2fortran(input_file_c,input_file)
        write(*,*)'Init input file:',trim(input_file)
        purelib_init=1
    end function

    !> Add function
    real(C_DOUBLE) function purelib_add(x,y) BIND(C,name='purelib_add')
        !DEC$ IF .NOT. DEFINED(__LINUX__)
        !DEC$ ATTRIBUTES DLLEXPORT :: purelib_add
        !GCC$ ATTRIBUTES DLLEXPORT :: purelib_add
        !DEC$ ENDIF
        real(C_DOUBLE), intent(in) :: x,y
        purelib_add=x+y
    end function

    !> Add subroutine
    subroutine purelib_addvec(x,y,add,n) BIND(C,name='purelib_addvec')
        !DEC$ IF .NOT. DEFINED(__LINUX__)
        !DEC$ ATTRIBUTES DLLEXPORT :: purelib_addvec
        !GCC$ ATTRIBUTES DLLEXPORT :: purelib_addvec
        !DEC$ ENDIF
        integer(C_INT), intent(in) :: n
        real(C_DOUBLE), dimension(n), intent(in) :: x,y
        real(C_DOUBLE), dimension(n), intent(out) :: add
        add=x+y
    end subroutine


    subroutine cstring2fortran(s_c,s)
        character(kind=C_CHAR,len=1),dimension(*),intent(in) :: s_c
        character(*), intent(inout):: s
        integer :: i
        loop_string: do i=1,len(s)
            if ( s_c(i) == CHAR(0) ) then
                exit loop_string
            else
                s(i:i) = s_c(i)
            end if
        end do loop_string

        if(i==1) then
            s=''
        else
            s = s(1:(i-1))
            s = trim(s)
        endif
    end subroutine
    
    subroutine fortranstring2c(s_f,s_c,n)
        character(len=*),intent(in):: s_f
        character(kind=C_CHAR,len=1),dimension(*),intent(inout) :: s_c
        integer, intent(out), optional :: n  
        integer :: i
        loop_string: do i=1,len(s_f)
            if ( s_f(i:i) == CHAR(0) ) then
                exit loop_string
            else
                s_c(i) = s_f(i:i)
            end if
        end do loop_string
        if(present(n))then
            n=i-1
        endif
    end subroutine

end module

