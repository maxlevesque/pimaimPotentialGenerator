module potentialParameters

    use fumitosi, only: initFumiTosi => init
    use polarizabilities, only: initPolarizabilities => init
    use dipoles, only: initDipoles => init
    implicit none
    
    
    contains
    
        subroutine init
            call initPolarizabilities
            call initFumiTosi
            call initDipoles
        end subroutine init
       
end module
