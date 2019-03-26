program test_lapack
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

    write(*,'(A)')'[ OK ] Test Lapack min'



end program
