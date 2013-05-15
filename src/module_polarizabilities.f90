module polarizabilities

    use ISO_FORTRAN_ENV, only: dp => real64, i2b => int8

    implicit none
    
    integer(i2b), parameter, private :: nElement = 118
    real(dp), dimension (nElement) :: polarizability
    
    contains

        subroutine init
            polarizability (:) = 0._dp

            polarizability(  3 ) = 0.0_dp            !   Li  src: Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)
            polarizability(  4 ) = 0.0_dp            !   Be  src: Heaton et al., J. Phys. Chem. B 110, 11459 (2006)
            polarizability(  8 ) = 10.74_dp          !   O 
            polarizability(  9 ) = 7.885_dp          !   F   src: actual work on transmutations. 7.9 in Benes et al., J. Chem. Phys. 130, 134716 (2009)
            polarizability( 11 ) = 1.028_dp          !   Na  src: Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)
            polarizability( 19 ) = 5.0_dp            !   K   src: Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)
            polarizability( 20 ) = 3.1_dp            !   Ca  src: Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)
            polarizability( 22 ) = 2.9067812052_dp   !   Ti  
            polarizability( 37 ) = 8.4_dp            !   Rb  src: Benes et al., J. Chem. Phys. 130, 134716 (2009)
            polarizability( 38 ) = 5.1_dp            !   Sr  src: Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)
            polarizability( 39 ) = 3.8_dp            !   Y   src: Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)
            polarizability( 40 ) = 2.926_dp          !   Zr  src: Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)
            polarizability( 55 ) = 14.8_dp           !   Cs  src: Benes et al., J. Chem. Phys. 130, 134716 (2009)
            polarizability( 57 ) = 7.5_dp            !   La  src: Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)
            polarizability( 90 ) = 7.696_dp          !   Th  src: 
        end subroutine
        
end module
