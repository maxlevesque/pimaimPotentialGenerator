module fumitosi

    use iso_fortran_env, only: dp => real64, i2b => int8 
    use atomicNumbersAssociatedToNames
    implicit none
    
    integer(i2b), parameter :: nelement = 118
    type FumiTosiPotential
        real(dp) :: alpha, B, C6, C8, F6, F8
        character(150) :: source
    end type
    type (FumiTosiPotential), dimension(nelement,nelement) :: FT ! Fumi Tozzi
    
    contains
    
        subroutine init
            call zeroify

            FT(Be,Be)%source = "Heaton et al., J. Phys. Chem. B 110, 11459 (2006)"
            FT(Be,Be)%alpha  = 3.944_dp
            FT(Be,Be)%B      = 106.16_dp
            FT(Be,Be)%C6     = 0.001_dp 
            FT(Be,Be)%C8     = 0.001_dp  
            FT(Be,Be)%F6     = 1.0_dp
            FT(Be,Be)%F8     = 1.0_dp

            FT(Be,F)%source = "Heaton et al., J. Phys. Chem. B 110, 11459 (2006)"
            FT(Be,F)%alpha  = 2.254_dp
            FT(Be,F)%B      = 41.72_dp
            FT(Be,F)%C6     = 0.001_dp 
            FT(Be,F)%C8     = 0.001_dp  
            FT(Be,F)%F6     = 1.0_dp
            FT(Be,F)%F8     = 1.0_dp

            FT(Be,Na)%source = "Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)"
            FT(Be,Na)%alpha  = 5.0_dp
            FT(Be,Na)%B      = 1.0_dp
            FT(Be,Na)%C6     = 0.001_dp ! cf mail Mathieu 
            FT(Be,Na)%C8     = 0.001_dp ! cf mail Mathieu  
            FT(Be,Na)%F6     = 1.9_dp
            FT(Be,Na)%F8     = 1.9_dp

            FT(Be,Y)%source = "Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)"
            FT(Be,Y)%alpha  = 5.0_dp
            FT(Be,Y)%B      = 1.0_dp
            FT(Be,Y)%C6     = 0.001_dp ! cf mail Mathieu
            FT(Be,Y)%C8     = 0.001_dp ! cf mail Mathieu
            FT(Be,Y)%F6     = 1.9_dp
            FT(Be,Y)%F8     = 1.9_dp

            FT(Be,La)%source = "Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)"
            FT(Be,La)%alpha  = 5.0_dp
            FT(Be,La)%B      = 1.0_dp
            FT(Be,La)%C6     = 0.001_dp ! cf mail Mathieu
            FT(Be,La)%C8     = 0.001_dp ! cf mail Mathieu
            FT(Be,La)%F6     = 1.9_dp
            FT(Be,La)%F8     = 1.9_dp

            FT(F,F)%source = "Salanne et al., Phys. Chem. Chem. Phys., 2009, 11, 11501-11506"
            FT(F,F)%alpha  = 2.444_dp
            FT(F,F)%B      = 282.3_dp
            FT(F,F)%C6     = 15._dp  
            FT(F,F)%C8     = 150._dp 
            FT(F,F)%F6     = 1.9_dp 
            FT(F,F)%F8     = 1.9_dp  

            FT(F,Zr)%source = "actual work on transmutations"
            FT(F,Zr)%alpha  = 1.791_dp   
            FT(F,Zr)%B      = 72.16_dp   
            FT(F,Zr)%C6     = 33.5000_dp 
            FT(F,Zr)%C8     = 335.000_dp 
            FT(F,Zr)%F6     = 1.900_dp   
            FT(F,Zr)%F8     = 1.900_dp   
            
            FT(F,Th)%source = "Dewan et al., Journal of Nuclear Materials 434 (2013) 322-327"
            FT(F,Th)%alpha = 1.634_dp  
            FT(F,Th)%B     = 70.148_dp 
            FT(F,Th)%C6    = 38.7_dp   
            FT(F,Th)%C8    = 387._dp   
            FT(F,Th)%F6    = 1.900_dp  
            FT(F,Th)%F8    = 1.900_dp  
            
            FT(F,Li)%source = "unknown, used in Levesque et al., J. Chem. Phys. XXXXXXX TODO"
            FT(F,Li)%alpha = 1.974_dp 
            FT(F,Li)%B     = 52.83_dp 
            FT(F,Li)%C6    = 13.25_dp 
            FT(F,Li)%C8    = 88.15_dp 
            FT(F,Li)%F6    = 1.900_dp 
            FT(F,Li)%F8    = 1.900_dp 
!~             FT(F,Li)%source = "Salanne et al., Phys. Chem. Chem. Phys., 2009, 11, 11501-11506"
!~             FT(F,Li)%alpha = 1.974_dp
!~             FT(F,Li)%B     = 18.8_dp 
!~             FT(F,Li)%C6    = 1.2_dp  
!~             FT(F,Li)%C8    = 12.2_dp 
!~             FT(F,Li)%F6    = 1.900_dp
!~             FT(F,Li)%F8    = 1.900_dp
            
            FT(F,K)%source = "Salanne et al., Phys. Chem. Chem. Phys., 2009, 11, 11501-11506"
            FT(F,K)%alpha = 2.04_dp  
            FT(F,K)%B     = 138.8_dp 
            FT(F,K)%C6    = 3.9_dp   
            FT(F,K)%C8    = 38.7_dp  
            FT(F,K)%F6    = 1.900_dp 
            FT(F,K)%F8    = 1.900_dp 
            
            FT(F,Y)%source = ".NOT. Rollet et al., Journal of Fluorine Chemistry 134 (2012) 44-48"
            FT(F,Y)%alpha = 1.832_dp
            FT(F,Y)%B     = 87.40_dp
            FT(F,Y)%C6    = 13.7_dp 
            FT(F,Y)%C8    = 0.001_dp
            FT(F,Y)%F6    = 1.900_dp
            FT(F,Y)%F8    = 1.900_dp
            
            FT(F,La)%source = "unknown"
            FT(F,La)%alpha = 1.867_dp
            FT(F,La)%B     = 161.6_dp
            FT(F,La)%C6    = 13.7_dp ! copied from (F,Y)
            FT(F,La)%C8    = 0.001_dp ! copied from (F,Y)
            FT(F,La)%F6    = 1.900_dp
            FT(F,La)%F8    = 1.900_dp
            
            FT(F,Na)%source = "actual work on transmutations"
            FT(F,Na)%alpha = 1.974_dp 
            FT(F,Na)%B     = 52.83_dp 
            FT(F,Na)%C6    = 13.25_dp 
            FT(F,Na)%C8    = 88.15_dp 
            FT(F,Na)%F6    = 1.900_dp 
            FT(F,Na)%F8    = 1.900_dp 
            
            FT(Zr,Zr)%source = "actual work on transmutations"
            FT(Zr,Zr)%alpha = 5.00_dp 
            FT(Zr,Zr)%B     = 1.00_dp 
            FT(Zr,Zr)%C6    = 75.00_dp
            FT(Zr,Zr)%C8    = 750.00_dp
            FT(Zr,Zr)%F6    = 1.900_dp 
            FT(Zr,Zr)%F8    = 1.900_dp 
            
            FT(Zr,Na)%source = "actual work on transmutations Salanne et al., J. Fluo. Chem. 130, 38-44 (2009)"
            FT(Zr,Na)%alpha = 5.00_dp 
            FT(Zr,Na)%B     = 1.00_dp 
            FT(Zr,Na)%C6    = 29.60_dp
            FT(Zr,Na)%C8    = 197.100_dp
            FT(Zr,Na)%F6    = 1.900_dp
            FT(Zr,Na)%F8    = 1.900_dp
            
            FT(Na,Na)%source = "actual work on transmutations"
            FT(Na,Na)%alpha = 5.00_dp
            FT(Na,Na)%B     = 1.00_dp
            FT(Na,Na)%C6    = 11.700_dp
            FT(Na,Na)%C8    = 51.80_dp
            FT(Na,Na)%F6    = 1.900_dp
            FT(Na,Na)%F8    = 1.900_dp
            
            FT(Na,K)%source = "Salanne et al., J. Fluo. Chem. 130, 38-44 (2009) (error in paper, corrected here)"
            FT(Na,K)%alpha = 5.0_dp 
            FT(Na,K)%B     = 1.0_dp 
            FT(Na,K)%C6    = 3.4_dp 
            FT(Na,K)%C8    = 22.8_dp
            FT(Na,K)%F6    = 1.9_dp 
            FT(Na,K)%F8    = 1.9_dp 
            
            FT(Th,Th)%source = "Dewan et al., Journal of Nuclear Materials 434 (2013) 322-327"
            FT(Th,Th)%alpha = 5.0_dp    
            FT(Th,Th)%B     = 1.0_dp    
            FT(Th,Th)%C6    = 100.000_dp
            FT(Th,Th)%C8    = 1000.00_dp
            FT(Th,Th)%F6    = 1.900_dp  
            FT(Th,Th)%F8    = 1.900_dp  
            
            FT(Th,Li)%source = "Dewan et al., Journal of Nuclear Materials 434 (2013) 322-327"
            FT(Th,Li)%alpha = 5.0_dp   
            FT(Th,Li)%B     = 1.0_dp   
            FT(Th,Li)%C6    = 3.16_dp  
            FT(Th,Li)%C8    = 31.6_dp  
            FT(Th,Li)%F6    = 1.900_dp 
            FT(Th,Li)%F8    = 1.900_dp 
            
            FT(Th,Y)%source = "unknown"
            FT(Th,Y)%alpha = 5.0_dp   
            FT(Th,Y)%B     = 1.0_dp
            FT(Th,Y)%C6    = 3.160_dp
            FT(Th,Y)%C8    = 31.6_dp
            FT(Th,Y)%F6    = 1.900_dp
            FT(Th,Y)%F8    = 1.900_dp
            
            FT(Li,Li)%source = "Salanne et al., Phys. Chem. Chem. Phys., 2009, 11, 11501-11506"
            FT(Li,Li)%alpha = 5.0_dp 
            FT(Li,Li)%B     = 1.0_dp 
            FT(Li,Li)%C6    = 0.10_dp
            FT(Li,Li)%C8    = 1.00_dp
            FT(Li,Li)%F6    = 1.90_dp
            FT(Li,Li)%F8    = 1.90_dp
            
            FT(Li,K)%source = "Salanne et al., Phys. Chem. Chem. Phys., 2009, 11, 11501-11506"
            FT(Li,K)%alpha = 5.0_dp 
            FT(Li,K)%B     = 1.0_dp 
            FT(Li,K)%C6    = 0.30_dp
            FT(Li,K)%C8    = 3.20_dp
            FT(Li,K)%F6    = 1.90_dp
            FT(Li,K)%F8    = 1.90_dp
            
            FT(Li,Na)%source = "Salanne et al., J. Fluo. Chem. 130, 38-44 (2009) (error in paper, corrected here)"
            FT(Li,Na)%alpha = 5.0_dp 
            FT(Li,Na)%B     = 1.0_dp 
            FT(Li,Na)%C6    = 1.1_dp 
            FT(Li,Na)%C8    = 7.00_dp
            FT(Li,Na)%F6    = 1.90_dp
            FT(Li,Na)%F8    = 1.90_dp
            
            FT(Li,Zr)%source = "Salanne et al., J. Fluo. Chem. 130, 38-44 (2009) (error in paper, corrected here)"
            FT(Li,Zr)%alpha = 5.0_dp  
            FT(Li,Zr)%B     = 1.0_dp  
            FT(Li,Zr)%C6    = 2.7_dp  
            FT(Li,Zr)%C8    = 27.4_dp 
            FT(Li,Zr)%F6    = 1.90_dp 
            FT(Li,Zr)%F8    = 1.90_dp 
            
            FT(Li,Y)%source = ".NOT. Rollet et al., Journal of Fluorine Chemistry 134 (2012) 44-48"
            FT(Li,Y)%alpha = 5.0_dp  
            FT(Li,Y)%B     = 1.0_dp  
            FT(Li,Y)%C6    = 0.01_dp 
            FT(Li,Y)%C8    = 0.1_dp  
            FT(Li,Y)%F6    = 1.900_dp
            FT(Li,Y)%F8    = 1.900_dp
            
            FT(Y,Zr)%source = "Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)"
            FT(Y,Zr)%alpha  = 5.0_dp
            FT(Y,Zr)%B      = 1.0_dp
            FT(Y,Zr)%C6    = 0.01_dp 
            FT(Y,Zr)%C8    = 0.1_dp  
            FT(Y,Zr)%F6    = 1.900_dp
            FT(Y,Zr)%F8    = 1.900_dp
            
            FT(Y,Na)%source = "Salanne et al., Theor. Chem. Acc. 131, 1143 (2012)"
            FT(Y,Na)%alpha  = 5.0_dp
            FT(Y,Na)%B      = 1.0_dp
            FT(Y,Na)%C6    = 0.01_dp 
            FT(Y,Na)%C8    = 0.1_dp  
            FT(Y,Na)%F6    = 1.900_dp
            FT(Y,Na)%F8    = 1.900_dp
            
            FT(K,K)%source = "Salanne et al., Phys. Chem. Chem. Phys., 2009, 11, 11501-11506"
            FT(K,K)%alpha = 5.0_dp  
            FT(K,K)%B     = 1.0_dp  
            FT(K,K)%C6    = 1.0_dp  
            FT(K,K)%C8    = 10.0_dp 
            FT(K,K)%F6    = 1.9_dp  
            FT(K,K)%F8    = 1.9_dp  
            
            FT(K,Zr)%source = "Salanne et al., J. Fluo. Chem. 130, 38-44 (2009) (error in paper, corrected here)"
            FT(K,Zr)%alpha = 5.0_dp
            FT(K,Zr)%B     = 1.0_dp
            FT(K,Zr)%C6    = 8.7_dp
            FT(K,Zr)%C8    = 86.6_dp
            FT(K,Zr)%F6    = 1.9_dp
            FT(K,Zr)%F8    = 1.9_dp
            
            FT(Y,Y)%source = ".NOT. Rollet et al., Journal of Fluorine Chemistry 134 (2012) 44-48"
            FT(Y,Y)%alpha = 5.0_dp   
            FT(Y,Y)%B     = 1.0_dp   
            FT(Y,Y)%C6    = 12.5040_dp  
            FT(Y,Y)%C8    = 0.001_dp    
            FT(Y,Y)%F6    = 1.900_dp 
            FT(Y,Y)%F8    = 1.900_dp 
            
            FT(La,Th)%source = "copied from Th,Y"
            FT(La,Th)%alpha = 5.0_dp  
            FT(La,Th)%B     = 1.0_dp  
            FT(La,Th)%C6    = 3.160_dp 
            FT(La,Th)%C8    = 31.6_dp 
            FT(La,Th)%F6    = 1.900_dp 
            FT(La,Th)%F8    = 1.900_dp 
            
            FT(La,Li)%source = "copied from Li,Y"
            FT(La,Li)%alpha = 5.0_dp 
            FT(La,Li)%B     = 1.0_dp 
            FT(La,Li)%C6    = 1.0000d-2
            FT(La,Li)%C8    = 1.0000d-1
            FT(La,Li)%F6    = 1.900_dp
            FT(La,Li)%F8    = 1.900_dp
            
            FT(O,O)%source = "Dario Corradini, 30/04/2013"
            FT(O,O)%alpha  = 2.406_dp
            FT(O,O)%B      = 290.4_dp
            FT(O,O)%C6     = 22.0000712_dp
            FT(O,O)%C8     = 425.999477_dp
            FT(O,O)%F6     = 1.400001255_dp
            FT(O,O)%F8     = 1.400001255_dp
            
            FT(O,F)%source = "Dario Corradini, 30/04/2013"
            FT(O,F)%alpha  = 2.495_dp
            FT(O,F)%B      = 278.4_dp
            FT(O,F)%C6     = 18.1659568_dp
            FT(O,F)%C8     = 252.7839644_dp
            FT(O,F)%F6     = 1.650000345_dp
            FT(O,F)%F8     = 1.650000345_dp
            
            FT(O,Ti)%source = "Dario Corradini, 30/04/2013"
            FT(O,Ti)%alpha  = 1.5157263637_dp
            FT(O,Ti)%B      = 43.000376669_dp
            FT(O,Ti)%C6     = 0.001_dp
            FT(O,Ti)%C8     = 0.001_dp
            FT(O,Ti)%F6     = 1.400001255_dp
            FT(O,Ti)%F8     = 1.400001255_dp
            
            FT(F,Ti)%source = "Dario Corradini, 30/04/2013"
            FT(F,Ti)%alpha  = 1.6567595403_dp
            FT(F,Ti)%B      = 28.312934368_dp
            FT(F,Ti)%C6     = 0.001_dp
            FT(F,Ti)%C8     = 0.001_dp
            FT(F,Ti)%F6     = 1.400001255_dp
            FT(F,Ti)%F8     = 1.400001255_dp

            FT(Ti,Ti)%source = "Dario Corradini, 30/04/2013"
            FT(Ti,Ti)%alpha  = 5.0_dp
            FT(Ti,Ti)%B      = 1.0_dp
            FT(Ti,Ti)%C6     = 0.001_dp 
            FT(Ti,Ti)%C8     = 0.001_dp 
            FT(Ti,Ti)%F6     = 1.400001255_dp
            FT(Ti,Ti)%F8     = 1.400001255_dp

            call symetrizeInteractions

        end subroutine init
        
        subroutine zeroify
            FT%source = ""
            FT%alpha = 0._dp
            FT%B = 0.0_dp
            FT%C6 = 0.0_dp
            FT%C8 = 0.0_dp
            FT%F6 = 0.0_dp
            FT%F8 = 0.0_dp
        end subroutine zeroify

        subroutine symetrizeInteractions
            integer :: i, j
            do i=1, nelement
                do j= i, nelement
                    call reverse(FT%alpha,i,j)
                    call reverse(FT%B,i,j)
                    call reverse(FT%C6,i,j)
                    call reverse(FT%C8,i,j)
                    call reverse(FT%F6,i,j)
                    call reverse(FT%F8,i,j)
                end do
            end do
        end subroutine
        
        subroutine reverse( array, i, j)
            use periodic_table, only: ptable
            integer, intent(in) :: i, j
            real(dp), dimension(nelement,nelement), intent(inout) :: array
            if (i == j) return
            if( array(i,j)==0.  .and. array(j,i)==0.  ) then
                !do nothing
            else if ( array(i,j)==0._dp  .and. array(j,i)/=0._dp  ) then
                array(i,j) = array(j,i)
            else if ( array(i,j)/=0._dp  .and. array(j,i)==0._dp  ) then
                array(j,i) = array(i,j)
            else if ( array(i,j)/=0._dp  .and. array(j,i)/=0._dp  ) then
                !do nothing
                print*,'STOP. Non-symetric (thus incorrect) Fumi Tosi potential (Fij/=Fji) found for '&
                                                            , ptable(i)%symbol(1:2), ptable(j)%symbol(1:2)
                stop
            end if
        end subroutine

end module
