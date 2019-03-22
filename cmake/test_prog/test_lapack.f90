program test_lapack
    use LinearSolve
    use PrecisionMod
    implicit none
    real(kind=8), dimension(:),allocatable :: x,b,b2,b3
    real(kind=8), dimension(:,:),allocatable :: a,a2,a3
    real(kind=8),dimension(:),allocatable :: err, err2,err3
    integer :: i, info, lda, ldb, nrhs, n
    integer, dimension(:), allocatable :: ipiv
    logical :: bSingular
    real(8)  :: max_error_lapack
    real(8)  :: max_error_manu
    real(8)  :: max_error_omnivor
    logical :: bTest=.true.

    interface
        subroutine dgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
            integer, intent(in)                     :: n,nrhs
            integer, intent(in)                     :: lda
            integer, intent(in)                     :: ldb
            real(8), dimension(lda, n), intent(inout) :: a
            integer,dimension(n),intent(out)        :: ipiv
            real(8), dimension(ldb,nrhs), intent(inout)  :: b
            integer, intent(out)                    :: info
        end subroutine 
    end interface



    n=300;
    if (.not. bTest) then
        !     call omp_set_num_threads(4)
        !print*,'procs',omp_get_num_procs(),'max_threads', omp_get_max_threads()
        !     call mkl_set_num_threads(4)
    endif
    
    nrhs = 1 ! number of right hand sides in b
    lda = n  ! leading dimension of a
    ldb = n  ! leading dimension of b

    allocate(a(lda,n))
    allocate(ipiv(n))

    allocate(b(n)) !allocate(b_proper(ldb,nrhs))
    allocate(err(n))
    allocate(x(n))
    allocate(a2(n,n))
    allocate(a3(n,n))
    allocate(b2(n))
    allocate(b3(n))
    allocate(err2(n))
    allocate(err3(n))

    call random_number(a)
    a2=a
    a3=a
    call random_number(x)
    b = matmul(a,x) ! compute RHS
    b2=b
    b3=b



    call dgesv(n, nrhs, a, lda, ipiv, b, ldb, info)


    call factor(a2,ipiv,bSingular)
    call solve(a2,b2,ipiv,bSingular)
    !     call solve_lapackr8(a2,b2, ipiv)
    !     call solve_lapackd(a2,b2, ipiv)

    call factor(a3)
    call solve(a3,b3)


    ! Note: the solution is returned in b and a has been changed.
    ! compare computed solution to original x:
    do i=1,n
        err(i) = abs(x(i)-b(i))/abs(x(i))
        err2(i) = abs(x(i)-b2(i))/abs(x(i))
        err3(i) = abs(x(i)-b3(i))/abs(x(i))
    enddo
    ! 
    max_error_lapack  = sum(err)/n
    max_error_manu    = sum(err2)/n
    max_error_omnivor = sum(err3)/n
    if (.not.bTest) then
        !     print '(A,3d16.6)', 'Error:',sum(err)/n
        print*,'Errors:'
        print '(3d16.6)', n
    endif

    if(max_error_lapack>1e-4) then
        write(*,'(A,EN13.3E2)'),'[FAIL] Matlib matrix solver - LAPACK solver error:',max_error_lapack
        STOP 1
    elseif(max_error_manu>1e-4) then
        write(*,'(A,EN13.3E2)'),'[FAIL] Matlib matrix solver - Builtin solver error:',max_error_manu
        STOP 1
    elseif(max_error_omnivor>1e-4) then
        write(*,'(A,EN13.3E2)'),'[FAIL] Matlib matrix solver - Omnivor LAPACK solver error:',max_error_omnivor
        STOP 1
    else
        write(*,'(A,EN13.3E2)'),'[ OK ] Matlib matrix solver - solution accuracy:',max_error_omnivor
    endif


end program
