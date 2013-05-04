module pimaimPotentialParameters

    use ISO_FORTRAN_ENV
    use atomicNumbersAssociatedToNames
    implicit none
    
    type pot
        real(REAL64) :: alpha, B, C6, C8, F6, F8
        character(150) :: source
    end type
    type dipolar
        real(REAL64) :: dampingb, dampingc !1st line, 3rd line
        integer(INT8) :: order  !2nd line
        character(150) :: source
    end type
    type(pot), dimension(118,118) :: FT ! Fumi Tozzi
    type(dipolar), dimension(118,118) :: dip
    real(REAL64), dimension(118) :: polarizability
    
    contains
    
        subroutine init
            call zeroify
            
            FT(F,F)%source = "Salanne et al., Phys. Chem. Chem. Phys., 2009, 11, 11501-11506"
            FT(F,F)%alpha  = 2.444d0
            FT(F,F)%B      = 282.3d0
            FT(F,F)%C6     = 15.d0  
            FT(F,F)%C8     = 150.d0 
            FT(F,F)%F6     = 1.9d0 
            FT(F,F)%F8     = 1.9d0  
            polarizability(F) = 7.885 ! actual work on transmutations
            dip(F,F)%dampingb    = 10.0d0
            dip(F,F)%order     = 4
            dip(F,F)%dampingc     = 0.0d0

            FT(F,Zr)%source = "actual work on transmutations"
            FT(F,Zr)%alpha  = 1.791d0   
            FT(F,Zr)%B      = 72.16d0   
            FT(F,Zr)%C6     = 33.5000d0 
            FT(F,Zr)%C8     = 335.000d0 
            FT(F,Zr)%F6     = 1.900d0   
            FT(F,Zr)%F8     = 1.900d0   
            dip(F,Zr)%source = "actual work on transmutations"
            dip(F,Zr)%dampingb = 1.882d0      
            dip(F,Zr)%order  = 4         
            dip(F,Zr)%dampingc  = 1.886d0      
            dip(Zr,F)%dampingb = 1.882d0      
            dip(Zr,F)%order  = 4         
            dip(Zr,F)%dampingc  = -1.0d0    
            
            FT(F,Th)%source = "Dewan et al., Journal of Nuclear Materials 434 (2013) 322-327"
            FT(F,Th)%alpha = 1.634d0  
            FT(F,Th)%B     = 70.148d0 
            FT(F,Th)%C6    = 38.7d0   
            FT(F,Th)%C8    = 387.d0   
            FT(F,Th)%F6    = 1.900d0  
            FT(F,Th)%F8    = 1.900d0  
            dip(F,Th)%source = "Dewan et al., Journal of Nuclear Materials 434 (2013) 322-327"
            dip(F,Th)%dampingb = 1.938d0 
            dip(F,Th)%order  = 4
            dip(F,Th)%dampingc  = 2.791d0 
            dip(Th,F)%dampingb = 1.938d0 
            dip(Th,F)%order  = 4
            dip(Th,F)%dampingc  = -0.60906d0 
            
            FT(F,Li)%source = "unknown, used in Levesque et al., J. Chem. Phys. XXXXXXX TODO"
            FT(F,Li)%alpha = 1.974d0 
            FT(F,Li)%B     = 52.83d0 
            FT(F,Li)%C6    = 13.25d0 
            FT(F,Li)%C8    = 88.15d0 
            FT(F,Li)%F6    = 1.900d0 
            FT(F,Li)%F8    = 1.900d0 
            FT(F,Li)%source = "Salanne et al., Phys. Chem. Chem. Phys., 2009, 11, 11501-11506"
            FT(F,Li)%alpha = 1.974d0
            FT(F,Li)%B     = 18.8d0 
            FT(F,Li)%C6    = 1.2d0  
            FT(F,Li)%C8    = 12.2d0 
            FT(F,Li)%F6    = 1.900d0
            FT(F,Li)%F8    = 1.900d0
            dip(F,Li)%source = "Salanne et al., Phys. Chem. Chem. Phys., 2009, 11, 11501-11506"
            dip(F,Li)%dampingb = 1.834d0 
            dip(F,Li)%order  = 4    
            dip(F,Li)%dampingc  = 1.335d0 
            
            FT(F,K)%source = "Salanne et al., Phys. Chem. Chem. Phys., 2009, 11, 11501-11506"
            FT(F,K)%alpha = 2.04d0  
            FT(F,K)%B     = 138.8d0 
            FT(F,K)%C6    = 3.9d0   
            FT(F,K)%C8    = 38.7d0  
            FT(F,K)%F6    = 1.900d0 
            FT(F,K)%F8    = 1.900d0 
            dip(F,K)%source = "Salanne et al., Phys. Chem. Chem. Phys., 2009, 11, 11501-11506"
            dip(F,K)%dampingb = 1.745d0 
            dip(F,K)%order  = 4    
            dip(F,K)%dampingc  = 2.500d0 
            dip(K,F)%dampingb = 1.745d0 
            dip(K,F)%order  = 4    
            dip(K,F)%dampingc  = -0.31d0 
            
            FT(F,Y)%source = ".NOT. Rollet et al., Journal of Fluorine Chemistry 134 (2012) 44-48"
            FT(F,Y)%alpha = 1.832d0
            FT(F,Y)%B     = 87.40d0
            FT(F,Y)%C6    = 13.7d0 
            FT(F,Y)%C8    = 0.001d0
            FT(F,Y)%F6    = 1.900d0
            FT(F,Y)%F8    = 1.900d0
            dip(F,Y)%source = "Rollet et al., Journal of Fluorine Chemistry 134 (2012) 44-48"
            dip(F,Y)%dampingb = 1.847d0  
            dip(F,Y)%order  = 4     
            dip(F,Y)%dampingc  = 1.966d0  
            dip(Y,F)%dampingb = 1.847d0  
            dip(Y,F)%order  = 4     
            dip(Y,F)%dampingc  = -0.890d0 
            
            FT(F,La)%source = "unknown"
            FT(F,La)%alpha = 1.867d0
            FT(F,La)%B     = 161.6d0
            FT(F,La)%C6    = 13.7d0 ! copied from (F,Y)
            FT(F,La)%C8    = 0.001d0 ! copied from (F,Y)
            FT(F,La)%F6    = 1.900d0
            FT(F,La)%F8    = 1.900d0
            dip(F,La)%source = "unknown"
            dip(F,La)%dampingb = 1.614d0 
            dip(F,La)%order  = 4
            dip(F,La)%dampingc  = 1.348d0 
            dip(La,F)%dampingb = 1.614d0 
            dip(La,F)%order  = 4
            dip(La,F)%dampingc  = -0.47d0 
            
            FT(F,Na)%source = "actual work on transmutations"
            FT(F,Na)%alpha = 1.974d0 
            FT(F,Na)%B     = 52.83d0 
            FT(F,Na)%C6    = 13.25d0 
            FT(F,Na)%C8    = 88.15d0 
            FT(F,Na)%F6    = 1.900d0 
            FT(F,Na)%F8    = 1.900d0 
            dip(F,Na)%source = "actual work on transmutations"
            dip(F,Na)%dampingb = 1.831d0    
            dip(F,Na)%order  = 4       
            dip(F,Na)%dampingc  = 2.5d0      
            dip(Na,F)%dampingb = 1.831d0 
            dip(Na,F)%order  = 4
            dip(Na,F)%dampingc  = 0.022d0 
            
            FT(Zr,Zr)%source = "actual work on transmutations"
            FT(Zr,Zr)%alpha = 5.00d0 
            FT(Zr,Zr)%B     = 1.00d0 
            FT(Zr,Zr)%C6    = 75.00d0
            FT(Zr,Zr)%C8    = 750.00d0
            FT(Zr,Zr)%F6    = 1.900d0 
            FT(Zr,Zr)%F8    = 1.900d0 
            polarizability(Zr) = 2.926d0 
            
            FT(Zr,Na)%source = "actual work on transmutations Salanne et al., J. Fluo. Chem. 130, 38-44 (2009)"
            FT(Zr,Na)%alpha = 5.00d0 
            FT(Zr,Na)%B     = 1.00d0 
            FT(Zr,Na)%C6    = 29.60d0
            FT(Zr,Na)%C8    = 197.100d0
            FT(Zr,Na)%F6    = 1.900d0
            FT(Zr,Na)%F8    = 1.900d0
            dip(Zr,Na)%source = "actual work on transmutations .NOT. Salanne et al., J. Fluo. Chem. 130, 38-44 (2009)"
            dip(Zr,Na)%dampingb = 10.0d0
            dip(Zr,Na)%order  = 4
            dip(Zr,Na)%dampingc  = 0.0001d0 
            dip(Na,Zr)%dampingb = 10.0d0
            dip(Na,Zr)%order  = 4
            dip(Na,Zr)%dampingc  = 0.0001d0 
            
            FT(Na,Na)%source = "actual work on transmutations"
            FT(Na,Na)%alpha = 5.00d0
            FT(Na,Na)%B     = 1.00d0
            FT(Na,Na)%C6    = 11.700d0
            FT(Na,Na)%C8    = 51.80d0
            FT(Na,Na)%F6    = 1.900d0
            FT(Na,Na)%F8    = 1.900d0
            polarizability(Na) = 1.028d0 
            
            FT(Na,K)%source = "Salanne et al., J. Fluo. Chem. 130, 38-44 (2009) (error in paper, corrected here)"
            FT(Na,K)%alpha = 5.0d0 
            FT(Na,K)%B     = 1.0d0 
            FT(Na,K)%C6    = 3.4d0 
            FT(Na,K)%C8    = 22.8d0
            FT(Na,K)%F6    = 1.9d0 
            FT(Na,K)%F8    = 1.9d0 
            
            FT(Th,Th)%source = "Dewan et al., Journal of Nuclear Materials 434 (2013) 322-327"
            FT(Th,Th)%alpha = 5.0d0    
            FT(Th,Th)%B     = 1.0d0    
            FT(Th,Th)%C6    = 100.000d0
            FT(Th,Th)%C8    = 1000.00d0
            FT(Th,Th)%F6    = 1.900d0  
            FT(Th,Th)%F8    = 1.900d0  
            polarizability(Th) = 7.696d0
            
            FT(Th,Li)%source = "Dewan et al., Journal of Nuclear Materials 434 (2013) 322-327"
            FT(Th,Li)%alpha = 5.0d0   
            FT(Th,Li)%B     = 1.0d0   
            FT(Th,Li)%C6    = 3.16d0  
            FT(Th,Li)%C8    = 31.6d0  
            FT(Th,Li)%F6    = 1.900d0 
            FT(Th,Li)%F8    = 1.900d0 
            dip(Th,Li)%source = "Dewan et al., Journal of Nuclear Materials 434 (2013) 322-327"
            dip(Th,Li)%dampingb = 10.00d0 
            dip(Th,Li)%order  = 4
            dip(Th,Li)%dampingc  = 0.001d0 
            
            FT(Th,Y)%source = "unknown"
            FT(Th,Y)%alpha = 5.0d0   
            FT(Th,Y)%B     = 1.0d0
            FT(Th,Y)%C6    = 3.160d0
            FT(Th,Y)%C8    = 31.6d0
            FT(Th,Y)%F6    = 1.900d0
            FT(Th,Y)%F8    = 1.900d0
            dip(Th,Y)%source = "unknown"
            dip(Th,Y)%dampingb = 10.00d0 
            dip(Th,Y)%order  = 4
            dip(Th,Y)%dampingc  = 0.001d0 
            dip(Y,Th)%dampingb = 10.00d0 
            dip(Y,Th)%order  = 4
            dip(Y,Th)%dampingc  = 0.001d0 
            
            FT(Li,Li)%source = "Salanne et al., Phys. Chem. Chem. Phys., 2009, 11, 11501-11506"
            FT(Li,Li)%alpha = 5.0d0 
            FT(Li,Li)%B     = 1.0d0 
            FT(Li,Li)%C6    = 0.10d0
            FT(Li,Li)%C8    = 1.00d0
            FT(Li,Li)%F6    = 1.90d0
            FT(Li,Li)%F8    = 1.90d0
            
            FT(Li,K)%source = "Salanne et al., Phys. Chem. Chem. Phys., 2009, 11, 11501-11506"
            FT(Li,K)%alpha = 5.0d0 
            FT(Li,K)%B     = 1.0d0 
            FT(Li,K)%C6    = 0.30d0
            FT(Li,K)%C8    = 3.20d0
            FT(Li,K)%F6    = 1.90d0
            FT(Li,K)%F8    = 1.90d0
            
            FT(Li,Na)%source = "Salanne et al., J. Fluo. Chem. 130, 38-44 (2009) (error in paper, corrected here)"
            FT(Li,Na)%alpha = 5.0d0 
            FT(Li,Na)%B     = 1.0d0 
            FT(Li,Na)%C6    = 1.1d0 
            FT(Li,Na)%C8    = 7.00d0
            FT(Li,Na)%F6    = 1.90d0
            FT(Li,Na)%F8    = 1.90d0
            
            FT(Li,Zr)%source = "Salanne et al., J. Fluo. Chem. 130, 38-44 (2009) (error in paper, corrected here)"
            FT(Li,Zr)%alpha = 5.0d0  
            FT(Li,Zr)%B     = 1.0d0  
            FT(Li,Zr)%C6    = 2.7d0  
            FT(Li,Zr)%C8    = 27.4d0 
            FT(Li,Zr)%F6    = 1.90d0 
            FT(Li,Zr)%F8    = 1.90d0 
            
            FT(Li,Y)%source = ".NOT. Rollet et al., Journal of Fluorine Chemistry 134 (2012) 44-48"
            FT(Li,Y)%alpha = 5.0d0  
            FT(Li,Y)%B     = 1.0d0  
            FT(Li,Y)%C6    = 0.01d0 
            FT(Li,Y)%C8    = 0.1d0  
            FT(Li,Y)%F6    = 1.900d0
            FT(Li,Y)%F8    = 1.900d0
            dip(Y,Li)%source = ".NOT. Rollet et al., Journal of Fluorine Chemistry 134 (2012) 44-48"
            dip(Y,Li)%dampingb = 10.00d0 
            dip(Y,Li)%order  = 4
            dip(Y,Li)%dampingc  = 0.001d0 
            
            FT(K,K)%source = "Salanne et al., Phys. Chem. Chem. Phys., 2009, 11, 11501-11506"
            FT(K,K)%alpha = 5.0d0  
            FT(K,K)%B     = 1.0d0  
            FT(K,K)%C6    = 1.0d0  
            FT(K,K)%C8    = 10.0d0 
            FT(K,K)%F6    = 1.9d0  
            FT(K,K)%F8    = 1.9d0  
            
            FT(K,Zr)%source = "Salanne et al., J. Fluo. Chem. 130, 38-44 (2009) (error in paper, corrected here)"
            FT(K,Zr)%alpha = 5.0d0
            FT(K,Zr)%B     = 1.0d0
            FT(K,Zr)%C6    = 8.7d0
            FT(K,Zr)%C8    = 86.6d0
            FT(K,Zr)%F6    = 1.9d0
            FT(K,Zr)%F8    = 1.9d0
            
            FT(Y,Y)%source = ".NOT. Rollet et al., Journal of Fluorine Chemistry 134 (2012) 44-48"
            FT(Y,Y)%alpha = 5.0d0   
            FT(Y,Y)%B     = 1.0d0   
            FT(Y,Y)%C6    = 12.5040d0  
            FT(Y,Y)%C8    = 0.001d0    
            FT(Y,Y)%F6    = 1.900d0 
            FT(Y,Y)%F8    = 1.900d0 
            polarizability(Y) = 3.8d0
            
            FT(La,Th)%source = "copied from Th,Y"
            FT(La,Th)%alpha = 5.0d0  
            FT(La,Th)%B     = 1.0d0  
            FT(La,Th)%C6    = 3.160d0 
            FT(La,Th)%C8    = 31.6d0 
            FT(La,Th)%F6    = 1.900d0 
            FT(La,Th)%F8    = 1.900d0 
            dip(La,Th)%source = "copied from Th,Y"
            dip(La,Th)%dampingb = 10.00d0 
            dip(La,Th)%order  = 4
            dip(La,Th)%dampingc  = 0.001d0 
            dip(Th,La)%dampingb = 10.00d0 
            dip(Th,La)%order  = 4
            dip(Th,La)%dampingc  = 0.001d0 
            
            FT(La,Li)%source = "copied from Li,Y"
            FT(La,Li)%alpha = 5.0d0 
            FT(La,Li)%B     = 1.0d0 
            FT(La,Li)%C6    = 1.0000d-2
            FT(La,Li)%C8    = 1.0000d-1
            FT(La,Li)%F6    = 1.900d0
            FT(La,Li)%F8    = 1.900d0
            dip(La,Li)%source = "copied from Li,Y"
            dip(La,Li)%dampingb = 10.00d0 
            dip(La,Li)%order  = 4
            dip(La,Li)%dampingc  = 0.001d0 
            
            polarizability(La) = 7.5d0
            
            dip(Y,La)%source = "unknown"
            dip(Y,La)%dampingb = 10.00d0 
            dip(Y,La)%order  = 4
            dip(Y,La)%dampingc  = 0.001d0 
            dip(La,Y)%dampingb = 10.00d0 
            dip(La,Y)%order  = 4
            dip(La,Y)%dampingc  = 0.001d0 
            
            FT(O,O)%source = "Dario Corradini, 30/04/2013"
            FT(O,O)%alpha  = 2.406d0
            FT(O,O)%B      = 290.4d0
            FT(O,O)%C6     = 22.0000712d0
            FT(O,O)%C8     = 425.999477d0
            FT(O,O)%F6     = 1.400001255d0
            FT(O,O)%F8     = 1.400001255d0
            polarizability(O) = 10.74d0
            dip(O,O)%source = "Dario Corradini, 30/04/2013"
            dip(O,O)%dampingb    = 2.513d0
            dip(O,O)%order     = 4
            dip(O,O)%dampingc     = 2.227d0
            
            FT(O,F)%source = "Dario Corradini, 30/04/2013"
            FT(O,F)%alpha  = 2.495d0
            FT(O,F)%B      = 278.4d0
            FT(O,F)%C6     = 18.1659568d0
            FT(O,F)%C8     = 252.7839644d0
            FT(O,F)%F6     = 1.650000345d0
            FT(O,F)%F8     = 1.650000345d0
            dip(O,F)%source= "Dario Corradini, 30/04/2013"
            dip(O,F)%dampingb    = 2.298d0
            dip(O,F)%order     = 4
            dip(O,F)%dampingc     = 0.0d0
            dip(F,O)%source= "Dario Corradini, 30/04/2013"
            dip(F,O)%dampingb    = 2.298d0
            dip(F,O)%order     = 4
            dip(F,O)%dampingc     = 0.0d0

            
            FT(O,Ti)%source = "Dario Corradini, 30/04/2013"
            FT(O,Ti)%alpha  = 1.5157263637d0
            FT(O,Ti)%B      = 43.000376669d0
            FT(O,Ti)%C6     = 0.001d0
            FT(O,Ti)%C8     = 0.001d0
            FT(O,Ti)%F6     = 1.400001255d0
            FT(O,Ti)%F8     = 1.400001255d0
            dip(O,Ti)%source= "Dario Corradini, 30/04/2013"
            dip(O,Ti)%dampingb    = 2.0644340419d0
            dip(O,Ti)%order     = 4
            dip(O,Ti)%dampingc     = 2.1332671421d0
            dip(Ti,O)%source= "Dario Corradini, 30/04/2013"
            dip(Ti,O)%dampingb    = 2.0644340419d0
            dip(Ti,O)%order     = 4
            dip(Ti,O)%dampingc     = -1.9033036302d0
            
            FT(F,Ti)%source = "Dario Corradini, 30/04/2013"
            FT(F,Ti)%alpha  = 1.6567595403d0
            FT(F,Ti)%B      = 28.312934368d0
            FT(F,Ti)%C6     = 0.001d0
            FT(F,Ti)%C8     = 0.001d0
            FT(F,Ti)%F6     = 1.400001255d0
            FT(F,Ti)%F8     = 1.400001255d0
            dip(F,Ti)%source = "Dario Corradini, 30/04/2013"
            dip(F,Ti)%dampingb     = 2.2060686135d0
            dip(F,Ti)%order      = 4
            dip(F,Ti)%dampingc      = 2.9067812052d0
            dip(Ti,F)%source = "Dario Corradini, 30/04/2013"
            dip(Ti,F)%dampingb     = 2.2060686135d0
            dip(Ti,F)%order      = 4
            dip(Ti,F)%dampingc      = -2.6605685674d0

            FT(Ti,Ti)%source = "Dario Corradini, 30/04/2013"
            FT(Ti,Ti)%alpha  = 5.0d0
            FT(Ti,Ti)%B      = 1.0d0
            FT(Ti,Ti)%C6     = 0.001d0 
            FT(Ti,Ti)%C8     = 0.001d0 
            FT(Ti,Ti)%F6     = 1.400001255d0
            FT(Ti,Ti)%F8     = 1.400001255d0
            polarizability(Ti) =  2.9067812052d0
            dip(Ti,Ti)%source = "Dario Corradini, 30/04/2013"
            dip(Ti,Ti)%dampingb     = 10.0d0
            dip(Ti,Ti)%order      = 4
            dip(Ti,Ti)%dampingc      = 0.0d0

            
            call symetrizeInteractions

        end subroutine init
        
        subroutine zeroify
                FT%alpha = 0.d0
                FT%B = 0.d0
                FT%C6 = 0.d0
                FT%C8 = 0.d0
                FT%F6 = 0.d0
                FT%F8 = 0.d0
                dip%dampingb = 0.d0
                dip%order = 0.d0
                dip%dampingc = 0.d0
                polarizability = 0.d0
        end subroutine zeroify

        subroutine symetrizeInteractions
            integer :: i, j
            do i=1, 118
                do j= i, 118
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
            integer, intent(in) :: i, j
            real(REAL64), dimension(118,118), intent(inout) :: array
            if( array(i,j)==0.  .and. array(j,i)==0.  ) then
                !do nothing
            else if ( array(i,j)==0.d0  .and. array(j,i)/=0.d0  ) then
                array(i,j) = array(j,i)
            else if ( array(i,j)/=0.d0  .and. array(j,i)==0.d0  ) then
                array(j,i) = array(i,j)
            else if ( array(i,j)/=0.d0  .and. array(j,i)/=0.d0  ) then
                !do nothing
            end if
        end subroutine

end module
