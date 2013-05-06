program pimaimPotentialGenerator

    use pimaimPotentialParameters, only: FT, dip, polarizability, initPimaimPotentialParameters=>init
    implicit none

    integer :: nSpecies
    integer, allocatable, dimension(:) :: listOfSpecies
    logical, allocatable, dimension(:) :: isPolarizable
    integer :: i
    
    call initPimaimPotentialParameters
    call getNecessaryInformations
    call checkAllNeededPotentialsArePresent
    call printPotentialDotInpt
    
    contains
    
        subroutine checkAllNeededPotentialsArePresent
            integer :: i, j, li, lj
            do i= 1, nSpecies
                if( .not. isPolarizable(i) ) cycle
                li = listOfSpecies(i)
                do j= 1, nSpecies
                    if( j==i ) cycle
                    lj = listOfSpecies(j)
                    if (dip(li,lj)%order == 0) then
                        print*,"STOP. Potential info is lacking for ",li,lj
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
                    if( any(listOfSpecies(1:i-1)==listOfSpecies(i)) ) then
                        print*,"Species already entered. Enter another one."
                        goto 777
                    else if( i<=0 ) then
                        print*,"Invalid value. It should be strictly positive."
                        goto 777
                    end if
                end if
                print*,"Is species ",i," polarizable? (T or F)"
                read(*,*) isPolarizable(i)
            end do
        end subroutine

        subroutine printPotentialDotInpt
            integer :: i, j, li, lj
            open(10,file="potential.inpt")
            write(10,'(a)')'FT          i.e. Fumi Tosi'
            do i= 1, nSpecies
                li = listOfSpecies(i)
                do j= i, nSpecies
                    lj = listOfSpecies(j)
                    write(10,*) FT(li,lj)%alpha, li, lj
                    write(10,*) FT(li,lj)%B
                    write(10,*) FT(li,lj)%C6
                    write(10,*) FT(li,lj)%C8
                    write(10,*) FT(li,lj)%F6
                    write(10,*) FT(li,lj)%F8
                    write(10,*)
                end do
            end do
            do i= 1, nSpecies
                if( .not. isPolarizable(i) ) cycle
                li = listOfSpecies(i)
                write(10,*)
                write(10,*) polarizability(li)
                do j= 1, nSpecies
                    if( j==i ) cycle
                    lj = listOfSpecies(j)
                    write(10,*) dip(li,lj)%dampingb
                    write(10,*) dip(li,lj)%order
                    write(10,*) dip(li,lj)%dampingc
                end do
            end do
        end subroutine
    
end program
