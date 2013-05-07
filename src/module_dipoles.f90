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
            dip(F,F)%dampingb    = 10.0d0
            dip(F,F)%order     = 4
            dip(F,F)%dampingc     = 0.0d0
            dip(F,Zr)%source = "actual work on transmutations"
            dip(F,Zr)%dampingb = 1.882d0      
            dip(F,Zr)%order  = 4         
            dip(F,Zr)%dampingc  = 1.886d0      
            dip(Zr,F)%dampingb = 1.882d0      
            dip(Zr,F)%order  = 4         
            dip(Zr,F)%dampingc  = -1.0d0    
            dip(F,Th)%source = "Dewan et al., Journal of Nuclear Materials 434 (2013) 322-327"
            dip(F,Th)%dampingb = 1.938d0 
            dip(F,Th)%order  = 4
            dip(F,Th)%dampingc  = 2.791d0 
            dip(Th,F)%dampingb = 1.938d0 
            dip(Th,F)%order  = 4
            dip(Th,F)%dampingc  = -0.60906d0 
            dip(F,Li)%source = "Salanne et al., Phys. Chem. Chem. Phys., 2009, 11, 11501-11506"
            dip(F,Li)%dampingb = 1.834d0 
            dip(F,Li)%order  = 4    
            dip(F,Li)%dampingc  = 1.335d0 
            dip(F,K)%source = "Salanne et al., Phys. Chem. Chem. Phys., 2009, 11, 11501-11506"
            dip(F,K)%dampingb = 1.745d0 
            dip(F,K)%order  = 4    
            dip(F,K)%dampingc  = 2.500d0 
            dip(K,F)%dampingb = 1.745d0 
            dip(K,F)%order  = 4    
            dip(K,F)%dampingc  = -0.31d0 
            dip(F,Y)%source = "Rollet et al., Journal of Fluorine Chemistry 134 (2012) 44-48"
            dip(F,Y)%dampingb = 1.847d0  
            dip(F,Y)%order  = 4     
            dip(F,Y)%dampingc  = 1.966d0  
            dip(Y,F)%dampingb = 1.847d0  
            dip(Y,F)%order  = 4     
            dip(Y,F)%dampingc  = -0.890d0 
            dip(F,La)%source = "unknown"
            dip(F,La)%dampingb = 1.614d0 
            dip(F,La)%order  = 4
            dip(F,La)%dampingc  = 1.348d0 
            dip(La,F)%dampingb = 1.614d0 
            dip(La,F)%order  = 4
            dip(La,F)%dampingc  = -0.47d0 
            dip(F,Na)%source = "actual work on transmutations"
            dip(F,Na)%dampingb = 1.831d0    
            dip(F,Na)%order  = 4       
            dip(F,Na)%dampingc  = 2.5d0      
            dip(Na,F)%dampingb = 1.831d0 
            dip(Na,F)%order  = 4
            dip(Na,F)%dampingc  = 0.022d0 
            dip(Zr,Na)%source = "actual work on transmutations .NOT. Salanne et al., J. Fluo. Chem. 130, 38-44 (2009)"
            dip(Zr,Na)%dampingb = 10.0d0
            dip(Zr,Na)%order  = 4
            dip(Zr,Na)%dampingc  = 0.0001d0 
            dip(Na,Zr)%dampingb = 10.0d0
            dip(Na,Zr)%order  = 4
            dip(Na,Zr)%dampingc  = 0.0001d0 
            dip(Th,Li)%source = "Dewan et al., Journal of Nuclear Materials 434 (2013) 322-327"
            dip(Th,Li)%dampingb = 10.00d0 
            dip(Th,Li)%order  = 4
            dip(Th,Li)%dampingc  = 0.001d0 
            dip(Th,Y)%source = "unknown"
            dip(Th,Y)%dampingb = 10.00d0 
            dip(Th,Y)%order  = 4
            dip(Th,Y)%dampingc  = 0.001d0 
            dip(Y,Th)%dampingb = 10.00d0 
            dip(Y,Th)%order  = 4
            dip(Y,Th)%dampingc  = 0.001d0 
            dip(Y,Li)%source = ".NOT. Rollet et al., Journal of Fluorine Chemistry 134 (2012) 44-48"
            dip(Y,Li)%dampingb = 10.00d0 
            dip(Y,Li)%order  = 4
            dip(Y,Li)%dampingc  = 0.001d0 
            dip(Y,Zr)%source = "TODO ASK CONFIRMATION FOR MATHIEU"
            dip(Y,Zr)%dampingb = 10.00d0 
            dip(Y,Zr)%order  = 4
            dip(Y,Zr)%dampingc  = 0.001d0 
            dip(Zr,Y)%source = "TODO ASK CONFIRMATION FOR MATHIEU"
            dip(Zr,Y)%dampingb = 10.00d0 
            dip(Zr,Y)%order  = 4
            dip(Zr,Y)%dampingc  = 0.001d0 
            dip(Y,Na)%source = "TODO ASK CONFIRMATION FOR MATHIEU"
            dip(Y,Na)%dampingb = 10.00d0 
            dip(Y,Na)%order  = 4
            dip(Y,Na)%dampingc  = 0.001d0 
            dip(Na,Y)%source = "TODO ASK CONFIRMATION FOR MATHIEU"
            dip(Na,Y)%dampingb = 10.00d0 
            dip(Na,Y)%order  = 4
            dip(Na,Y)%dampingc  = 0.001d0 
            dip(La,Th)%source = "copied from Th,Y"
            dip(La,Th)%dampingb = 10.00d0 
            dip(La,Th)%order  = 4
            dip(La,Th)%dampingc  = 0.001d0 
            dip(Th,La)%dampingb = 10.00d0 
            dip(Th,La)%order  = 4
            dip(Th,La)%dampingc  = 0.001d0 
            dip(La,Li)%source = "copied from Li,Y"
            dip(La,Li)%dampingb = 10.00d0 
            dip(La,Li)%order  = 4
            dip(La,Li)%dampingc  = 0.001d0 
            dip(Y,La)%source = "unknown"
            dip(Y,La)%dampingb = 10.00d0 
            dip(Y,La)%order  = 4
            dip(Y,La)%dampingc  = 0.001d0 
            dip(La,Y)%dampingb = 10.00d0 
            dip(La,Y)%order  = 4
            dip(La,Y)%dampingc  = 0.001d0 
            dip(O,O)%source = "Dario Corradini, 30/04/2013"
            dip(O,O)%dampingb    = 2.513d0
            dip(O,O)%order     = 4
            dip(O,O)%dampingc     = 2.227d0
            dip(O,F)%source= "Dario Corradini, 30/04/2013"
            dip(O,F)%dampingb    = 2.298d0
            dip(O,F)%order     = 4
            dip(O,F)%dampingc     = 0.0d0
            dip(F,O)%source= "Dario Corradini, 30/04/2013"
            dip(F,O)%dampingb    = 2.298d0
            dip(F,O)%order     = 4
            dip(F,O)%dampingc     = 0.0d0
            dip(O,Ti)%source= "Dario Corradini, 30/04/2013"
            dip(O,Ti)%dampingb    = 2.0644340419d0
            dip(O,Ti)%order     = 4
            dip(O,Ti)%dampingc     = 2.1332671421d0
            dip(Ti,O)%source= "Dario Corradini, 30/04/2013"
            dip(Ti,O)%dampingb    = 2.0644340419d0
            dip(Ti,O)%order     = 4
            dip(Ti,O)%dampingc     = -1.9033036302d0
            dip(F,Ti)%source = "Dario Corradini, 30/04/2013"
            dip(F,Ti)%dampingb     = 2.2060686135d0
            dip(F,Ti)%order      = 4
            dip(F,Ti)%dampingc      = 2.9067812052d0
            dip(Ti,F)%source = "Dario Corradini, 30/04/2013"
            dip(Ti,F)%dampingb     = 2.2060686135d0
            dip(Ti,F)%order      = 4
            dip(Ti,F)%dampingc      = -2.6605685674d0
            dip(Ti,Ti)%source = "Dario Corradini, 30/04/2013"
            dip(Ti,Ti)%dampingb     = 10.0d0
            dip(Ti,Ti)%order      = 4
            dip(Ti,Ti)%dampingc      = 0.0d0

        end subroutine

end module
