module dipoles

    use iso_fortran_env, only: dp => real64, i2b => int8
    use atomicNumbersAssociatedToNames
    implicit none

    type dipolar
        real(dp) :: dampingb, dampingc !1st line, 3rd line
        integer(i2b) :: order  !2nd line
        character(150) :: source
    end type
    integer(i2b), parameter :: nelement = 118
    type(dipolar), dimension(nelement,nelement) :: dip

    contains
    
        subroutine init
            ! dip(A,B) means effect of B on A.
            
            dip(F,F)%source = ""
            dip(F,F)%dampingb    = 10.0_dp
            dip(F,F)%order     = 4_i2b
            dip(F,F)%dampingc     = 0.0_dp

            dip(F,Be)%source = "Heaton et al., J. Phys. Chem. B 110, 11460 (2006)"
            dip(F,Be)%dampingb = 1.78_dp
            dip(F,Be)%order    = 4_i2b
            dip(F,Be)%dampingc = 0.99_dp
            dip(Be,F)%source = "Heaton et al., J. Phys. Chem. B 110, 11460 (2006)"
            dip(Be,F)%dampingb = 0.0_dp ! Be is not polarizable
            dip(Be,F)%order    = 0_i2b
            dip(Be,F)%dampingc = 0.0_dp

            dip(F,Rb)%source = "Benes et al., J. Chem. Phys. 130 (2009) 134716"
            dip(F,Rb)%dampingb = 1.822_dp
            dip(F,Rb)%order = 4_i2b
            dip(F,Rb)%dampingc = 3.463_dp
            dip(Rb,F)%source = "Benes et al., J. Chem. Phys. 130 (2009) 134716"
            dip(Rb,F)%dampingb = 1.822_dp
            dip(Rb,F)%order = 4_i2b
            dip(Rb,F)%dampingc = -0.44_dp

            dip(F,Cs)%source = "Benes et al., J. Chem. Phys. 130 (2009) 134716"
            dip(F,Cs)%dampingb = 1.930_dp
            dip(F,Cs)%order = 4_i2b
            dip(F,Cs)%dampingc = 3.391_dp
            dip(Cs,F)%source = "Benes et al., J. Chem. Phys. 130 (2009) 134716"
            dip(Cs,F)%dampingb = 1.930_dp
            dip(Cs,F)%order = 4_i2b
            dip(Cs,F)%dampingc = 0.485_dp

            dip(Rb,Rb)%source = "Benes et al., J. Chem. Phys. 130 (2009) 134716"
            dip(Rb,Rb)%dampingb = 1.0_dp
            dip(Rb,Rb)%order = 4_i2b
            dip(Rb,Rb)%dampingc = 0.0_dp

            dip(Rb,Cs)%source = "Benes et al., J. Chem. Phys. 130 (2009) 134716"
            dip(Rb,Cs)%dampingb = 1.0_dp
            dip(Rb,Cs)%order = 4_i2b
            dip(Rb,Cs)%dampingc = 0.0_dp
            dip(Cs,Rb)%source = "Benes et al., J. Chem. Phys. 130 (2009) 134716"
            dip(Cs,Rb)%dampingb = 1.0_dp
            dip(Cs,Rb)%order = 4_i2b
            dip(Cs,Rb)%dampingc = 0.0_dp

            dip(Cs,Cs)%source = "Benes et al., J. Chem. Phys. 130 (2009) 134716"
            dip(Cs,Cs)%dampingb = 1.0_dp
            dip(Cs,Cs)%order = 4_i2b
            dip(Cs,Cs)%dampingc = 0.0_dp

            dip(F,Ca)%source = "Salanne et al., Theor Chem Acc (2012) 131:1143"
            dip(F,Ca)%dampingb = 1.732_dp
            dip(F,Ca)%order = 4_i2b
            dip(F,Ca)%dampingc = 1.5_dp
            dip(Ca,F)%source = "Salanne et al., Theor Chem Acc (2012) 131:1143"
            dip(Ca,F)%dampingb = 1.732_dp
            dip(Ca,F)%order = 4_i2b
            dip(Ca,F)%dampingc = -0.31_dp

            dip(F,Sr)%source = "Salanne et al., Theor Chem Acc (2012) 131:1143"
            dip(F,Sr)%dampingb = 1.554_dp
            dip(F,Sr)%order = 4_i2b
            dip(F,Sr)%dampingc = 1.331_dp
            dip(Sr,F)%source = "Salanne et al., Theor Chem Acc (2012) 131:1143"
            dip(Sr,F)%dampingb = 1.554_dp
            dip(Sr,F)%order = 4_i2b
            dip(Sr,F)%dampingc = -0.33_dp

            dip(F,Zr)%source = "Salanne et al., Theor Chem Acc (2012) 131:1143"
            dip(F,Zr)%dampingb = 1.882_dp      
            dip(F,Zr)%order  = 4_i2b         
            dip(F,Zr)%dampingc  = 1.886_dp    
            dip(Zr,F)%source = "Salanne et al., Theor Chem Acc (2012) 131:1143"
            dip(Zr,F)%dampingb = 1.882_dp      
            dip(Zr,F)%order  = 4_i2b
            dip(Zr,F)%dampingc  = -1.0_dp

            dip(F,Th)%source = "Dewan et al., Journal of Nuclear Materials 434 (2013) 322-327"
            dip(F,Th)%dampingb = 1.938_dp 
            dip(F,Th)%order  = 4_i2b
            dip(F,Th)%dampingc  = 2.791_dp 
            dip(Th,F)%dampingb = 1.938_dp 
            dip(Th,F)%order  = 4_i2b
            dip(Th,F)%dampingc  = -0.60906_dp 

            dip(F,Li)%source = "Salanne et al., Phys. Chem. Chem. Phys., 2009, 11, 11501-11506"
            dip(F,Li)%dampingb = 1.834_dp 
            dip(F,Li)%order  = 4_i2b
            dip(F,Li)%dampingc  = 1.335_dp 

            dip(F,K)%source = "Salanne et al., Phys. Chem. Chem. Phys., 2009, 11, 11501-11506"
            dip(F,K)%dampingb = 1.745_dp 
            dip(F,K)%order  = 4_i2b
            dip(F,K)%dampingc  = 2.500_dp
            dip(K,F)%source = "Salanne et al., Phys. Chem. Chem. Phys., 2009, 11, 11501-11506"
            dip(K,F)%dampingb = 1.745_dp 
            dip(K,F)%order  = 4_i2b
            dip(K,F)%dampingc  = -0.31_dp 

            dip(F,Y)%source = "Rollet et al., Journal of Fluorine Chemistry 134 (2012) 44-48"
            dip(F,Y)%dampingb = 1.847_dp  
            dip(F,Y)%order  = 4_i2b
            dip(F,Y)%dampingc  = 1.966_dp  
            dip(Y,F)%dampingb = 1.847_dp  
            dip(Y,F)%order  = 4_i2b 
            dip(Y,F)%dampingc  = -0.890_dp 

            dip(F,La)%source = "unknown"
            dip(F,La)%dampingb = 1.614_dp 
            dip(F,La)%order  = 4_i2b
            dip(F,La)%dampingc  = 1.348_dp 
            dip(La,F)%dampingb = 1.614_dp 
            dip(La,F)%order  = 4_i2b
            dip(La,F)%dampingc  = -0.47_dp 

            dip(F,Na)%source = "actual work on transmutations"
            dip(F,Na)%dampingb = 1.831_dp    
            dip(F,Na)%order  = 4_i2b   
            dip(F,Na)%dampingc  = 2.5_dp      
            dip(Na,F)%dampingb = 1.831_dp 
            dip(Na,F)%order  = 4_i2b
            dip(Na,F)%dampingc  = 0.022_dp 

            dip(Zr,Na)%source = "actual work on transmutations .NOT. Salanne et al., J. Fluo. Chem. 130, 38-44 (2009)"
            dip(Zr,Na)%dampingb = 10.0_dp
            dip(Zr,Na)%order  = 4_i2b
            dip(Zr,Na)%dampingc  = 0.0001_dp 
            dip(Na,Zr)%dampingb = 10.0_dp
            dip(Na,Zr)%order  = 4_i2b
            dip(Na,Zr)%dampingc  = 0.0001_dp 

            dip(Th,Li)%source = "Dewan et al., Journal of Nuclear Materials 434 (2013) 322-327"
            dip(Th,Li)%dampingb = 10.00_dp 
            dip(Th,Li)%order  = 4_i2b
            dip(Th,Li)%dampingc  = 0.001_dp 

            dip(Th,Y)%source = "unknown"
            dip(Th,Y)%dampingb = 10.00_dp 
            dip(Th,Y)%order  = 4_i2b
            dip(Th,Y)%dampingc  = 0.001_dp 
            dip(Y,Th)%source = "unknown"
            dip(Y,Th)%dampingb = 10.00_dp 
            dip(Y,Th)%order  = 4_i2b
            dip(Y,Th)%dampingc  = 0.001_dp 

            dip(Y,Li)%source = ".NOT. Rollet et al., Journal of Fluorine Chemistry 134 (2012) 44-48"
            dip(Y,Li)%dampingb = 10.00_dp 
            dip(Y,Li)%order  = 4_i2b
            dip(Y,Li)%dampingc  = 0.001_dp 

            dip(Y,Zr)%source = "TODO ASK CONFIRMATION FOR MATHIEU"
            dip(Y,Zr)%dampingb = 10.00_dp 
            dip(Y,Zr)%order  = 4_i2b
            dip(Y,Zr)%dampingc  = 0.001_dp 
            dip(Zr,Y)%source = "TODO ASK CONFIRMATION FOR MATHIEU"
            dip(Zr,Y)%dampingb = 10.00_dp 
            dip(Zr,Y)%order  = 4_i2b
            dip(Zr,Y)%dampingc  = 0.001_dp 

            dip(Y,Na)%source = "TODO ASK CONFIRMATION FOR MATHIEU"
            dip(Y,Na)%dampingb = 10.00_dp 
            dip(Y,Na)%order  = 4_i2b
            dip(Y,Na)%dampingc  = 0.001_dp 
            dip(Na,Y)%source = "TODO ASK CONFIRMATION FOR MATHIEU"
            dip(Na,Y)%dampingb = 10.00_dp 
            dip(Na,Y)%order  = 4_i2b
            dip(Na,Y)%dampingc  = 0.001_dp 

            dip(Na,Be)%source = "suggested by Mathieu in mail of April 2, 2013"
            dip(Na,Be)%dampingb = 10.0_dp
            dip(Na,Be)%order = 4_i2b
            dip(Na,Be)%dampingc = 0.001_dp

            dip(Cs,Be)%source = "Hypothesis by Levesque based on the effect of Be on Na. May 10 2013"
            dip(Cs,Be)%dampingb = 10.0_dp
            dip(Cs,Be)%order = 4_i2b
            dip(Cs,Be)%dampingc = 0.001_dp

            dip(La,Th)%source = "copied from Th,Y"
            dip(La,Th)%dampingb = 10.00_dp 
            dip(La,Th)%order  = 4_i2b
            dip(La,Th)%dampingc  = 0.001_dp 

            dip(La,Be)%source = "copied from Th,Y"
            dip(La,Be)%dampingb = 10.00_dp 
            dip(La,Be)%order  = 4_i2b
            dip(La,Be)%dampingc  = 0.001_dp 

            dip(Y,Be)%source = "copied from Th,Y"
            dip(Y,Be)%dampingb = 10.00_dp 
            dip(Y,Be)%order  = 4_i2b
            dip(Y,Be)%dampingc  = 0.001_dp 

            dip(Th,La)%source = "unknown"
            dip(Th,La)%dampingb = 10.00_dp 
            dip(Th,La)%order  = 4_i2b
            dip(Th,La)%dampingc  = 0.001_dp 

            dip(La,Li)%source = "copied from Li,Y"
            dip(La,Li)%dampingb = 10.00_dp 
            dip(La,Li)%order  = 4_i2b
            dip(La,Li)%dampingc  = 0.001_dp 

            dip(Y,La)%source = "unknown"
            dip(Y,La)%dampingb = 10.00_dp 
            dip(Y,La)%order  = 4_i2b
            dip(Y,La)%dampingc  = 0.001_dp 
            dip(La,Y)%source = "unknown"
            dip(La,Y)%dampingb = 10.00_dp 
            dip(La,Y)%order  = 4_i2b
            dip(La,Y)%dampingc  = 0.001_dp 

            dip(O,O)%source = "Dario Corradini, 30/04/2013 TO BE CHECKED"
            dip(O,O)%dampingb    = 2.513_dp
            dip(O,O)%order     = 4_i2b
            dip(O,O)%dampingc     = 2.227_dp

            dip(O,F)%source= "Dario Corradini, 30/04/2013 TO BE CHECKED"
            dip(O,F)%dampingb    = 2.298_dp
            dip(O,F)%order     = 4_i2b
            dip(O,F)%dampingc     = 0.0_dp
            dip(F,O)%source= "Dario Corradini, 30/04/2013 TO BE CHECKED"
            dip(F,O)%dampingb    = 2.298_dp
            dip(F,O)%order     = 4_i2b
            dip(F,O)%dampingc     = 0.0_dp

            dip(O,Ti)%source= "Dario Corradini, 30/04/2013 TO BE CHECKED"
            dip(O,Ti)%dampingb    = 2.0644340419_dp
            dip(O,Ti)%order     = 4_i2b
            dip(O,Ti)%dampingc     = 2.1332671421_dp
            dip(Ti,O)%source= "Dario Corradini, 30/04/2013 TO BE CHECKED"
            dip(Ti,O)%dampingb    = 2.0644340419_dp
            dip(Ti,O)%order     = 4_i2b
            dip(Ti,O)%dampingc     = -1.9033036302_dp

            dip(F,Ti)%source = "Dario Corradini, 30/04/2013 TO BE CHECKED"
            dip(F,Ti)%dampingb     = 2.2060686135_dp
            dip(F,Ti)%order      = 4_i2b
            dip(F,Ti)%dampingc      = 2.9067812052_dp
            dip(Ti,F)%source = "Dario Corradini, 30/04/2013 TO BE CHECKED"
            dip(Ti,F)%dampingb     = 2.2060686135_dp
            dip(Ti,F)%order      = 4_i2b
            dip(Ti,F)%dampingc      = -2.6605685674_dp

            dip(Ti,Ti)%source = "Dario Corradini, 30/04/2013 TO BE CHECKED"
            dip(Ti,Ti)%dampingb     = 10.0_dp
            dip(Ti,Ti)%order      = 4_i2b
            dip(Ti,Ti)%dampingc      = 0.0_dp

        end subroutine

end module
