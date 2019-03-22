module LinearSolve
    use PrecisionMod, only: MK, CDK, CIK

    implicit none

    interface factor; module procedure & 
            factor_manu,&
            factor_lapackd,&
            factor_lapacks;
    end interface
    interface solve; module procedure & 
            solve_manu,&
            solve_lapackr4,&
            solve_lapackr8;
    end interface

    interface 
        subroutine sgetrs(trans,n,nrhs,a,lda,ipiv,b,ldb,info)
            character(len=1), intent(in) :: trans
            integer, intent(in)                     :: n,nrhs
            integer, intent(in)                     :: lda
            integer, intent(in)                     :: ldb
            real(4), dimension(lda, n), intent(in) :: a
            integer,dimension(n),intent(out)        :: ipiv
            real(4), dimension(ldb,nrhs), intent(inout)  :: b
            integer, intent(out)                    :: info
        end subroutine

        subroutine dgetrs(trans,n,nrhs,a,lda,ipiv,b,ldb,info)
            character(len=1), intent(in) :: trans
            integer, intent(in)                     :: n,nrhs
            integer, intent(in)                     :: lda
            integer, intent(in)                     :: ldb
            real(8), dimension(lda, n), intent(in) :: a
            integer,dimension(n),intent(out)        :: ipiv
            real(8), dimension(ldb,nrhs), intent(inout)  :: b
            integer, intent(out)                    :: info
        end subroutine

        subroutine sgetrf(m,n,a,lda,ipiv,info)
            integer, intent(in)                     :: n,m
            integer, intent(in)                     :: lda
            real(4), dimension(lda, n), intent(inout) :: a
            integer,dimension(n),intent(out)        :: ipiv
            integer, intent(out)                    :: info
        end subroutine 

        subroutine dgetrf(m,n,a,lda,ipiv,info)
            integer, intent(in)                     :: n,m
            integer, intent(in)                     :: lda
            real(8), dimension(lda, n), intent(inout) :: a
            integer,dimension(n),intent(out)        :: ipiv
            integer, intent(out)                    :: info
        end subroutine

    end interface 



    private
    public :: factor, solve, invert_by_pivot
    public :: AxV



contains
    !> This subroutine factors a general matrix [a].
    subroutine factor_manu(a)
        integer :: i, j,  neqn
        real(MK), dimension(:, :), intent(inout) :: a

        neqn = size(a, 1)

        do j = 2, neqn
            do i = 1, j-1
                a(j, i) = a(j, i) - dot_product(a(j, 1:i-1), a(1:i-1, i))
                a(i, j) = a(i, j) - dot_product(a(i, 1:i-1), a(1:i-1, j))
                a(j, i) = a(j, i)/a(i, i)
            end do
            a(j, j) = a(j, j) - dot_product(a(j, 1:j-1), a(1:j-1, j))
        end do

    end subroutine

    !> This subroutine solves [a]{x} = {b} using the previously factored
    !> coefficient matrix [a] from subroutine 'factor'.
    !> The subroutine returns the solution {x} by overwriting b.
    subroutine solve_manu(a, b)
        integer :: i, neqn
        real(MK), dimension(:, :), intent(in) :: a
        real(MK), dimension(:), intent(inout) :: b

        neqn = size(a, 1)

        ! Forward substitution
        do i = 2, neqn
            b(i) = b(i) - dot_product(a(i, 1:i-1), b(1:i-1))
        end do

        ! Backward substitution
        b(neqn) = b(neqn)/a(neqn, neqn)
        do i = neqn-1, 1, -1
            b(i) = (b(i) - dot_product(a(i, i+1:neqn), b(i+1:neqn)))/a(i, i)
        end do
    end subroutine


    subroutine invert_by_pivot(m,Mat)
        integer(CIK), intent(in) :: m
        real(CDK), dimension(:,:), intent(inout) :: Mat
        ! Variables
        real(CDK), dimension(:),allocatable :: pivot
        real(CDK) :: a,b
        integer :: i,j,k
        allocate(pivot(1:m))

        do i=1,m
            a=Mat(i,i)
            Mat(i,i)=1.0
            do j=1,m
                pivot(j) = Mat(j,i)/a;
                Mat(j,i) = pivot(j)
            enddo
            do j=1,m
                if(i/=j) then
                    b   = Mat(i,j);
                    Mat(i,j) = 0.0_MK
                    do k=1,m
                        Mat(k,j) = Mat(k,j)-b*pivot(k)
                    enddo
                endif
            enddo
        enddo
    end subroutine

    subroutine AxV(m,Mat,RHS,Res)
        !
        integer(CIK), intent(in) :: m
        real(CDK), dimension(:,:), intent(in) :: Mat
        real(CDK), dimension(:), intent(in) :: RHS
        real(CDK), dimension(:), intent(out) :: Res
        !
        integer :: i,j

        do i=1,m
            Res(i)=0._CDK
            do j=1,m
                Res(i)=Res(i)+Mat(i,j)*RHS(j)
            enddo
        enddo

    end subroutine



    !> LU factorization of a matrix [a] using LAPACK.
    ! DGETRF(l)		LAPACK routine (version	1.1)		    DGETRF(l)
    ! 
    ! NAME
    !   DGETRF - compute an LU factorization of a general M-by-N matrix A using
    !   partial pivoting with	row interchanges
    ! SYNOPSIS
    !   SUBROUTINE DGETRF( M,	N, A, LDA, IPIV, INFO )
    !       INTEGER	     INFO, LDA,	M, N
    !       INTEGER	     IPIV( * )
    !       DOUBLE	     PRECISION A( LDA, * )
    ! PURPOSE
    !   DGETRF computes an LU	factorization of a general M-by-N matrix A using par-
    !   tial pivoting	with row interchanges.
    !   The factorization has	the form
    !      A = P * L * U
    !   where	P is a permutation matrix, L is	lower triangular with unit diagonal
    !   elements (lower trapezoidal if m > n), and U is upper	triangular (upper
    !   trapezoidal if m < n).
    !   This is the right-looking Level 3 BLAS version of the	algorithm.
    ! ARGUMENTS
    !   M	  (input) INTEGER
    ! 	  The number of	rows of	the matrix A.  M >= 0.
    !   N	  (input) INTEGER
    ! 	  The number of	columns	of the matrix A.  N >= 0.
    !   A	  (input/output) DOUBLE	PRECISION array, dimension (LDA,N)
    ! 	  On entry, the	M-by-N matrix to be factored.  On exit,	the factors L
    ! 	  and U	from the factorization A = P*L*U; the unit diagonal elements
    ! 	  of L are not stored.
    !   LDA	  (input) INTEGER
    ! 	  The leading dimension	of the array A.	 LDA >=	max(1,M).
    !   IPIV	  (output) INTEGER array, dimension (min(M,N))
    ! 	  The pivot indices; for 1 <= i	<= min(M,N), row i of the matrix was
    ! 	  interchanged with row	IPIV(i).
    !   INFO	  (output) INTEGER
    ! 	  = 0:	successful exit
    ! 	  < 0:	if INFO	= -i, the i-th argument	had an illegal value
    ! 	  > 0:	if INFO	= i, U(i,i) is exactly zero. The factorization has
    ! 	  been completed, but the factor U is exactly singular,	and division
    ! 	  by zero will occur if	it is used to solve a system of	equations.
    subroutine factor_lapackd(a,ipiv,bSingular)
        double precision, dimension(:, :), intent(inout) :: a
        integer,dimension(:),intent(inout)               :: ipiv
        logical,intent(out)               :: bSingular
        integer	:: info,lda,m,n
        n = size(a, 1); m=n; lda=n;
        ! Factorization using lapack 
        call dgetrf(m,n,a,lda,ipiv,info )

        bSingular=.false.
        if (info .lt. 0) then
            write (*,*) 'illegal dgetrf argument value', -info
        else if (info .gt. 0) then
            write (*,*) 'matrix was singular'
            bSingular=.true.
            call factor_manu(a)
        end if
    end subroutine
    subroutine factor_lapacks(a,ipiv,bSingular)
        real, dimension(:, :), intent(inout) :: a
        integer,dimension(:),intent(inout)               :: ipiv
        logical,intent(out)               :: bSingular
        integer	:: info,lda,m,n
        n = size(a, 1); m=n; lda=n;
        ! Factorization using lapack 
        call sgetrf(m,n,a,lda,ipiv,info )

        bSingular=.false.
        if (info .lt. 0) then
            write (*,*) 'illegal sgetrf argument value', -info
        else if (info .gt. 0) then
            write (*,*) 'matrix was singular'
            bSingular=.true.
            print*,'TODO factor manu single precision'
            !call factor_manu(a)
        end if
    end subroutine

    !> 
    ! DGETRS(l)		LAPACK routine (version	1.1)		    DGETRS(l)
    ! NAME
    !   DGETRS - solve a system of linear equations  A * X = B or A' * X = B with a
    !   general N-by-N matrix	A using	the LU factorization computed by DGETRF
    ! SYNOPSIS
    !   SUBROUTINE DGETRS( TRANS, N, NRHS, A,	LDA, IPIV, B, LDB, INFO	)
    !       CHARACTER	     TRANS
    !       INTEGER	     INFO, LDA,	LDB, N,	NRHS
    !       INTEGER	     IPIV( * )
    !       DOUBLE	     PRECISION A( LDA, * ), B( LDB, * )
    ! PURPOSE
    !   DGETRS solves	a system of linear equations
    !      A * X = B	or  A' * X = B with a general N-by-N matrix A using the	LU
    !   factorization	computed by DGETRF.
    ! ARGUMENTS
    !   TRANS	  (input) CHARACTER*1
    ! 	  Specifies the	form of	the system of equations:
    ! 	  = 'N':  A * X	= B  (No transpose)
    ! 	  = 'T':  A'* X	= B  (Transpose)
    ! 	  = 'C':  A'* X	= B  (Conjugate	transpose = Transpose)
    !   N	  (input) INTEGER
    ! 	  The order of the matrix A.  N	>= 0.
    !   NRHS	  (input) INTEGER
    ! 	  The number of	right hand sides, i.e.,	the number of columns of the
    ! 	  matrix B.  NRHS >= 0.
    !   A	  (input) DOUBLE PRECISION array, dimension (LDA,N)
    ! 	  The factors L	and U from the factorization A = P*L*U as computed by
    ! 	  DGETRF.
    !   LDA	  (input) INTEGER
    ! 	  The leading dimension	of the array A.	 LDA >=	max(1,N).
    !   IPIV	  (input) INTEGER array, dimension (N)
    ! 	  The pivot indices from DGETRF; for 1<=i<=N, row i of the matrix was
    ! 	  interchanged with row	IPIV(i).
    !   B	  (input/output) DOUBLE	PRECISION array, dimension (LDB,NRHS)
    ! 	  On entry, the	right hand side	matrix B.  On exit, the	solution
    ! 	  matrix X.
    !   LDB	  (input) INTEGER
    ! 	  The leading dimension	of the array B.	 LDB >=	max(1,N).
    !   INFO	  (output) INTEGER
    ! 	  = 0:	successful exit
    ! 	  < 0:	if INFO	= -i, the i-th argument	had an illegal value
    subroutine solve_lapackr4(a, b,ipiv,bSingular)
        real(4), dimension(:, :), intent(in) :: a
        real(4), dimension(:), intent(inout) :: b
        integer,dimension(:),intent(inout)    :: ipiv
        logical, intent(in) :: bSingular
        character:: trans='N'
        integer	:: info,lda,ldb,m,n,nrhs
        n = size(a, 1); m=n; lda=n; ldb=n;nrhs=1;
        ! Factorization using lapack 
        if(.not.bSingular) then
            call sgetrs( trans, n, nrhs, a,	lda, ipiv, b, ldb, info	)
            if (info .lt. 0) then
                write (*,*) 'illegal sgetrs argument value', -info
            endif
        else
            print*,'TODO, solver manu single precision'
            !call solve_manu(a,b)
        endif
    end subroutine
    subroutine solve_lapackr8(a, b,ipiv,bSingular)
        real(8), dimension(:, :), intent(in) :: a
        real(8), dimension(:), intent(inout) :: b
        integer,dimension(:),intent(inout)    :: ipiv
        logical, intent(in) :: bSingular
        character:: trans='N'
        integer	:: info,lda,ldb,m,n,nrhs
        n = size(a, 1); m=n; lda=n; ldb=n;nrhs=1;
        if(.not.bSingular) then
            ! Factorization using lapack 
            call dgetrs( trans, n, nrhs, a,	lda, ipiv, b, ldb, info	)
            if (info .lt. 0) then
                write (*,*) 'illegal sgetrs argument value', -info
            endif
        else
            call solve_manu(a,b)
        endif
    end subroutine






end module LinearSolve
