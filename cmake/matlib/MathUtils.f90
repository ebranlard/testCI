!> 
module MathUtils
    use PrecisionMod, only: MK
    implicit none
    interface sort_in_place
        module procedure sort_in_placer, sort_in_placei, sort_in_placeii, sort_in_placeri
    end interface
    interface diff; 
        module procedure ddiff
    end interface
    interface loop; 
        module procedure loop1, loopn
    end interface
contains

    ! --------------------------------------------------------------------------------
    ! ---  
    ! --------------------------------------------------------------------------------
    !> Divides two float and returns the proper integer n if l=n.h, otherwise returns floor(l/h)
    integer function floor_divide_exact(l,h) result(n)
        use PrecisionMod, only: precision_equal
        ! arguments
        real(MK), intent(in) :: l
        real(MK), intent(in) :: h
        ! 
        real(MK) :: nh
        n=floor(l/h)  
        if(n<0) then
            print*,'Error: MathUtils: Floor divide not thought for negative values'
        endif
        ! if l=nh it might be that floor returns n-1
        nh=real(n+1,MK)*h
        if(precision_equal(nh,l)) then
            n=n+1
        endif
    end function
    !> Divides two float and always returns an integer lower even if the l = n.h we will return n-1
    integer function floor_divide_lower(l,h) result(n)
        use PrecisionMod, only: precision_equal
        ! arguments
        real(MK), intent(in) :: l
        real(MK), intent(in) :: h
        ! 
        real(MK) :: nh
        n=floor(l/h)  
        if(n<0) then
            print*,'Error: MathUtils: Floor divide not thought for negative values'
        endif
        ! if l=nh we return n-1
        nh=real(n,MK)*h
        if(precision_equal(nh,l)) then
            n=n-1
        endif
    end function





    ! --------------------------------------------------------------------------------
    ! --- At least one true
    ! --------------------------------------------------------------------------------
    logical function some_true(vb)
        logical, dimension(:),intent(in) :: vb
        integer :: i
        some_true=.false.
        do i = 1,size(vb)
            if (vb(i)) then
                some_true=.true.
                exit
            endif
        end do
    end function 

    ! --------------------------------------------------------------------------------
    ! ---  diff
    ! --------------------------------------------------------------------------------
    !> simple diff
    function ddiff(d)
        real(MK),dimension(:),intent(in) ::d
        real(MK),dimension(size(d)-1) ::ddiff
        integer::i
        do i=1,size(d)-1
            ddiff(i)=d(i+1)-d(i)
        enddo
    end function
    ! --------------------------------------------------------------------------------
    ! --- Linspace and 1:3
    ! --------------------------------------------------------------------------------
    function linspace_p(s,e,n)
        integer,intent(in):: n
        real(MK),intent(in):: s,e
        real(MK),dimension(:),pointer :: linspace_p
        integer:: i
        allocate(linspace_p(1:n))
        if(n==1) then
            linspace_p(1)=s
        else
            do i=0,n-1
                linspace_p(i+1)=real(i,MK)
            enddo
            linspace_p=(linspace_p/(n-1))*(e-s)+s
        endif
    end function
    function linspace(s,e,n)
        integer,intent(in):: n
        real(MK),intent(in):: s,e
        real(MK),dimension(n) :: linspace
        integer:: i
        if(n==1) then
            linspace(1)=s
        else
            do i=0,n-1
                linspace(i+1)=(real(i,MK)/(n-1))*(e-s)+s
            enddo
        endif
    end function
   
    !> Equivalent to mtlab linspace but in subroutine way
    subroutine linvect(vect,s,e,n)
        integer,intent(in):: n !< size of vector
        real(MK),dimension(n) :: vect !< input vector, should be allocated
        real(MK),intent(in):: s,e !< start and end
!         allocate(linspace(1:n))
        ! Local variables
        integer :: i
        real(MK) :: factor !< compute the factor only once
        if(n==1) then
            vect(1)=s
        else
            factor=(e-s)/real((n-1),MK)
            do i=0,n-1
                vect(i+1)=real(i,MK)*factor+s
            enddo
        endif
    end subroutine

!      function seq(istart,iend)
!          integer,intent(in):: istart,iend
!          !         integer,dimension(iend-istart+1) :: seq
!          integer,dimension(:),allocatable :: seq
!          integer:: i
!          integer:: n
!          n=max(istart-iend+1,0)
!          allocate(seq(1:n))
!          seq=[ ((i),i=istart,iend) ]
!      end function
    subroutine seq(istart,iend,vin)
        integer,intent(in):: istart,iend
        !         integer,dimension(iend-istart+1) :: seq
!          integer,dimension(:),allocatable,intent(inout) :: vin
        integer,dimension(:) :: vin
        integer::n
        integer:: i
        n=iend-istart+1
!          if(associated(vin)) then
            if (size(vin)/=n) then
                print*,'Error wrong size for input vector in seq'
            else
!                  allocate(vin(1:iend-istart+1))
!              endif
!          else
!              allocate(vin(1:iend-istart+1))
!          endif
        vin=(/ ((i),i=istart,iend) /)
    endif
    end subroutine


    ! --------------------------------------------------------------------------------
    ! --- Loop
    ! --------------------------------------------------------------------------------
    !> loop ensures that v stays within 1:n 
    function loop1(v, n)
        ! Arguments declarations 
        integer, intent(in) :: n      !<  
        integer, intent(in) :: v !< 
        integer :: loop1 !< 
        loop1=modulo(v-1,n)+1
    end function
    function loopn(v, n)
        ! Arguments declarations 
        integer, intent(in) :: n      !<  
        integer, dimension(:), intent(in) :: v !< 
        integer, dimension(size(v,1)) :: loopn !< 
        loopn=modulo(v-1,n)+1
    end function

    ! --------------------------------------------------------------------------------
    ! --- Cross 
    ! --------------------------------------------------------------------------------
    !> Cross product of two vectors ! See crossprod in UTILS...
    function cross(a, b)
        real(MK), dimension(3) :: cross
        real(MK), dimension(3), intent(in) :: a, b
        cross(1) = a(2) * b(3) - a(3) * b(2)
        cross(2) = a(3) * b(1) - a(1) * b(3)
        cross(3) = a(1) * b(2) - a(2) * b(1)
    end function cross


    ! ------------------------------------------------------------------------------
    ! --- Sort
    ! ------------------------------------------------------------------------------
    !> 
    pure subroutine sort_in_placer(a)
        real(MK), intent(inout), dimension(:) :: a
        real(MK) :: temp
        integer :: i, j

        do i = 2, size(a)
            j = i - 1
            temp = a(i)
            do while (j>=1 .and. a(j)>temp)
                a(j+1) = a(j)
                j = j - 1
            end do
            a(j+1) = temp
        end do
    end subroutine sort_in_placer

    pure subroutine sort_in_placei(a)
        integer, intent(inout), dimension(:) :: a
        integer :: temp
        integer :: i, j

        do i = 2, size(a)
            j = i - 1
            temp = a(i)
            do while (j>=1 .and. a(j)>temp)
                a(j+1) = a(j)
                j = j - 1
                if (j<1) then
                    exit
                endif
            end do
            a(j+1) = temp
        end do
    end subroutine sort_in_placei


    !> sort a and performs the same manipulations to a2
    ! In Matlab [a,I]=sort(a); a2=a2(I);
    pure subroutine sort_in_placeii(a,a2)
        integer, intent(inout), dimension(:) :: a
        integer, intent(inout), dimension(:) :: a2
        integer :: temp
        integer :: temp2
        integer :: i, j

        do i = 2, size(a)
            j = i - 1
            temp = a(i)
            temp2 = a2(i)
            do while (j>=1 .and. a(j)>temp)
                a(j+1) = a(j)
                a2(j+1) = a2(j)
                j = j - 1
                if (j<1) then
                    exit
                endif
            end do
            a(j+1) = temp
            a2(j+1) = temp2
        end do
    end subroutine sort_in_placeii

    !> sort a and performs the same manipulations to a2
    ! In Matlab [a,I]=sort(a); a2=a2(I);
    pure subroutine sort_in_placeri(a,a2)
        real(MK), intent(inout), dimension(:) :: a
        integer, intent(inout), dimension(:) :: a2
        real(MK) :: temp
        integer :: temp2
        integer :: i, j

        do i = 2, size(a)
            j = i - 1
            temp = a(i)
            temp2 = a2(i)
            do while (j>=1 .and. a(j)>temp)
                a(j+1) = a(j)
                a2(j+1) = a2(j)
                j = j - 1
                if (j<1) then
                    exit
                endif
            end do
            a(j+1) = temp
            a2(j+1) = temp2
        end do
    end subroutine sort_in_placeri

    ! --------------------------------------------------------------------------------
    ! --- Find, find closest  
    ! --------------------------------------------------------------------------------
    !> Returns the index of the element in vector v which is the closest (and lower) to x
    ! Assumes that v is sorted in ascending order
    subroutine which_value_lowest(x,v,i_floor,x_floor)
        use MathConstants, only: NaN
        real(MK) :: x !< value we look for
        real(MK),dimension(:) :: v
        integer  :: i_floor !< closest lowest index (=-1 if does not exists)
        real(MK) :: x_floor !< closest lowest value
        integer :: i,n

        n=size(v)
        if(v(1)>x) then
            x_floor=NaN
            i_floor=-1
            return
        endif
        
        do i=2,n
            if(v(i)>x) then
                x_floor=v(i-1)
                i_floor=i-1
                return
            endif
        enddo

        i_floor=n
        x_floor=v(n)
        return

    end subroutine


    !> Same as above I believe, allows equlity and perform birnary search
    integer function fbinary_search(x, x0) result(i_inf)
        ! Arguments declarations 
        real(MK), dimension(:),intent(in) :: x  !<  
        real(MK), intent(in) :: x0  !<  
        ! Variable declarations 
        integer :: i_sup  !<  
        integer :: mid  !<  
        ! x a sorted vector (typically a grid vector) 
        ! Performs binary search and return the largest index such that x(i) <= x0 
        i_inf=1
        i_sup=size(x)

        ! Safety test 
        if (x0<x(1)) then 
            i_inf=-1
            return
        end if 
        if (x0>=x(i_sup)) then 
            i_inf=i_sup
            return
        end if 


        ! We loop until we narrow down to one index 
        do while (i_inf+1<i_sup) 
            mid=(int((i_inf+i_sup)/2))
            if (x(mid)<=x0) then 
                i_inf=mid
            else
                i_sup=mid
            end if 
        end do 
    end function fbinary_search 




    ! ------------------------------------------------------------------------------
    ! --- 
    ! ------------------------------------------------------------------------------

    function count_substring(s1, s2) result(c)
        character(*), intent(in) :: s1, s2
        integer :: c, p, posn

        c = 0
        if(len(s2) == 0) return
        p = 1
        do 
            posn = index(s1(p:), s2)
            if(posn == 0) return
            c = c + 1
            p = p + posn + len(s2)
        end do
    end function

    ! ------------------------------------------------------------------------------
    ! --- unique (see matlab functions)
    ! ------------------------------------------------------------------------------




end module MathUtils
