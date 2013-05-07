module polarizabilities

    use ISO_FORTRAN_ENV, only: dp => real64, i2b => int8

    implicit none
    
    integer(i2b), parameter, private :: nElement = 118
    real(dp), dimension (nElement) :: polarizability
    
    contains

        subroutine init
            polarizability (:) = 0.d0

            polarizability(  3 ) = 0.0d0            !   Li  src: Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)
            polarizability(  8 ) = 10.74d0          !   O 
            polarizability(  9 ) = 7.885d0          !   F   src: actual work on transmutations
            polarizability( 11 ) = 1.028d0          !   Na  src: Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)
            polarizability( 19 ) = 5.0d0            !   K   src: Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)
            polarizability( 20 ) = 3.1d0            !   Ca  src: Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)
            polarizability( 22 ) = 2.9067812052d0   !   Ti  
            polarizability( 37 ) = 8.4d0            !   Rb  src: Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)
            polarizability( 38 ) = 5.1d0            !   Sr  src: Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)
            polarizability( 39 ) = 3.8d0            !   Y   src: Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)
            polarizability( 40 ) = 2.926d0          !   Zr  src: Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)
            polarizability( 55 ) = 14.8d0           !   Cs  src: Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)
            polarizability( 57 ) = 7.5d0            !   La  src: Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)
            polarizability( 90 ) = 7.696d0          !   Th
        end subroutine
        
end module
