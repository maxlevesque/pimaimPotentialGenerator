program pimaimPotentialGenerator

    use iso_fortran_env, only: dp => real64
    use potentialParameters, only: initPotentialParameters => init
    use fumitosi, only: FT
    use polarizabilities, only: polarizability
    use dipoles, only: dip
    use periodic_table, only: initPeriodicTable => init_periodic_table
    implicit none

    integer :: nSpecies
    integer, allocatable, dimension(:) :: listOfSpecies
    logical, allocatable, dimension(:) :: isPolarizable
    integer :: i
    
    call initPeriodicTable
    call initPotentialParameters
    call getNecessaryInformations
    call checkAllNeededPotentialsArePresent
    call printPotentialDotInpt
    
    contains
    
        subroutine checkAllNeededPotentialsArePresent
            use periodic_table, only: ptable
            integer :: i, j, li, lj
            do i= 1, nSpecies
                if( .not. isPolarizable(i) ) cycle
                li = listOfSpecies(i)
                do j= 1, nSpecies
                    if( j==i ) cycle
                    lj = listOfSpecies(j)
                    if (FT (li,lj)%alpha == 0._dp) then
                        print*,"STOP. Fumi Tosi potential is lacking for ", ptable(li)%symbol(1:2), ptable(lj)%symbol(1:2)
                        stop
                    end if
                    if (dip(li,lj)%order == 0) then
                        print*,"STOP. dipolar dumping potential info is lacking for ", ptable(li)%symbol(1:2),ptable(lj)%symbol(1:2)
                        stop
                    end if
                end do
            end do
        end subroutine
   
        subroutine getNecessaryInformations
            print*,"How many species ?"
            read(*,*) nSpecies
            allocate(listOfSpecies(nSpecies), source=0)
            allocate(isPolarizable(nSpecies))
            do i=1, nSpecies
                777 print*,"Enter atomic nb of species",i
                read(*,*) listOfSpecies(i)
                if( i>= 2) then
                    if( i<=0 ) then
                        print*,"Invalid value. It should be strictly positive."
                        goto 777
                    end if
                end if
                print*,"Is species ",i," polarizable? (T or F)"
                read(*,*) isPolarizable(i)
            end do
        end subroutine

        subroutine printPotentialDotInpt
            use periodic_table, only: ptable
            integer :: i, j, li, lj
            open(10,file="potential.inpt")
            write(10,'(a)')'FT          i.e. Fumi Tosi'
            do i= 1, nSpecies
                li = listOfSpecies(i)
                do j= i, nSpecies
                    lj = listOfSpecies(j)
                    write(10,*) FT(li,lj)%alpha, ptable(li)%symbol(1:2), ptable(lj)%symbol(1:2), " alpha. src: ",&
                                                    trim(adjustl(FT(li,lj)%source))
                    write(10,*) FT(li,lj)%B,  ptable(li)%symbol(1:2), ptable(lj)%symbol(1:2), " B"
                    write(10,*) FT(li,lj)%C6, ptable(li)%symbol(1:2), ptable(lj)%symbol(1:2), " C6"
                    write(10,*) FT(li,lj)%C8, ptable(li)%symbol(1:2), ptable(lj)%symbol(1:2), " C8"
                    write(10,*) FT(li,lj)%F6, ptable(li)%symbol(1:2), ptable(lj)%symbol(1:2), " F6"
                    write(10,*) FT(li,lj)%F8, ptable(li)%symbol(1:2), ptable(lj)%symbol(1:2), " F8"
                    write(10,*)
                end do
            end do
            do i= 1, nSpecies
                if( .not. isPolarizable(i) ) cycle
                li = listOfSpecies(i)
                write(10,*)
                write(10,*) polarizability(li),' polarizability of ',ptable(li)%symbol
                do j= 1, nSpecies
                    if( j==i ) cycle
                    lj = listOfSpecies(j)
                    write(10,*) dip(li,lj)%dampingb,' damping (b) of    ', ptable(li)%symbol(1:2), ptable(lj)%symbol(1:2)
                    write(10,*) dip(li,lj)%order,'                       damping order of ', &
                                                                                ptable(li)%symbol(1:2), ptable(lj)%symbol(1:2)
                    write(10,*) dip(li,lj)%dampingc,' damping (c) of    ', ptable(li)%symbol(1:2), ptable(lj)%symbol(1:2)
                end do
            end do
        end subroutine
    
end program
