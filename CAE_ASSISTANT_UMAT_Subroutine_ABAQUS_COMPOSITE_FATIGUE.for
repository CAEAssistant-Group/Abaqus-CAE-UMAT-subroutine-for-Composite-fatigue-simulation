C      #This is just a demo to show how the code is structured. For the full, working version, please visit our website.
C      ######################################################################
C      #################      CAE Assistant Company          ################
C      ##############         www CAEassistant com              #############
C      ###########   Copy right by CAE Assistant Company    ###############
C      ######################################################################
C      ONLY the BUYER  of this package has permission to use its codes.
C	 Any distribution of this subroutine is illegal and will be prosecuted 
C      ######################################################################
C      ######################################################################
C      CAE Assisitant Services: 
C      Toturial Packages,Consultancy,Articles,Q&A,Video Gallery,Online Course
C      ######################################################################
C      Need help with your project? 
C      You can get initial free consultation from (Support CAEassistant com)
C      ######################################################################
      ccc============================================================================================      
	   SUBROUTINE UMAT(STRESS,STATEV,DDSDDE,SSE,SPD,SCD,
     1 RPL,DDSDDT,DRPLDE,DRPLDT,
     2 STRAN,DSTRAN,TIME,DTIME,TEMP,DTEMP,PREDEF,DPRED,CMNAME,
     3 NDI,NSHR,NTENS,NSTATV,PROPS,NPROPS,COORDS,DROT,PNEWDT,
     4 CELENT,DFGRD0,DFGRD1,NOEL,NPT,LAYER,KSPT,KSTEP,KINC)
C
      INCLUDE 'ABA_PARAM.INC'
C
      CHARACTER*80 CMNAME
      DIMENSION STRESS(NTENS),STATEV(NSTATV),
     1 DDSDDE(NTENS,NTENS),DDSDDT(NTENS),DRPLDE(NTENS),
     2 STRAN(NTENS),DSTRAN(NTENS),TIME(2),PREDEF(1),DPRED(1),
     3 PROPS(NPROPS),COORDS(3),DROT(3,3),DFGRD0(3,3),DFGRD1(3,3)
      
      real*4 E_11,E_22,E_33,
     1 G_12,
     1 nu_12,
     1 CC(3,3),
     1 E11_0,  E22_0,  G12_0,
     1 E11_f,  E22_f,  G12_f,
     1 XT_0, XC_0, YT_0, YC_0, S12_0, 
     1 XT, XC, XX,XXX,
     1 YT, YC, YY,YYY,
     1 S12,
     1 eps(3), sig(3),
     1 sig_max(3),sig_min(3),
     1 sig_m(3), sig_a(3), 
     1 Fiber_Failure, Matrix_Failure,
     1 fatigue_f, fatigue_m,
     1  RR(3),
     1 sig_a_eq(3), 
     1 Nf(3),Nf_norm(3),NN,
     1 cycletime,
     1 DW_star,unifiedNf,
     1 Alfa1T,Alfa1C,Alfa2T,Alfa2C,Alfa3T,Alfa3C,Alfa4,Alfa5,Alfa6,
     1 Beta1T,Beta1C,Beta2T,Beta2C,Beta3T,Beta3C,Beta4,Beta5,Beta6,
     1 Landa1,Landa2,Landa3,Landa4,Landa5,Landa6,
     1 Gamma1,Gamma2,Gamma3,Gamma4,Gamma5,Gamma6
     
     
      integer i,j,FiberStaticFailure,MatrixStaticFailure
 
c     Material elastic properties for intact material
      E11_0 = 37030.0  

      YC_0= 720.0 
 
      eps12_f = 0.101
  
      ! 15 Hidden Lines

c     Stiffness degradation parameters
      Landa1=14.57    ;      Gamma1=0.3024
      
      
c     Strength degradation parameters

      Alfa6=0.16     ;     Beta6=9.11    
      
      ! 13 Hidden Lines
      timeperiod=0.1d0                     
            
      IF (TIME(2).lt. timeperiod) then 
    
c     Material elastic properties 
c     Material elastic properties for intact material
      ! 30 Hidde Lines
      sig_a_eq(:)=0
      
      Nf_norm(:)=0
      Nf(:)=0
      
      ! extra
      Fiber_Failure=0   !extra
      Matrix_Failure=0  !extra
      DW_star=0
      unifiedNf=0
      FiberStaticFailure=0
      MatrixStaticFailure=0
      
c     Life & time for the fist fatigue step
      NN=100.0  !wil be multiplied by 10 in the fatigue step
      cycletime=0.1d0 
      
      ELSE   ! ---------------------------------------------------------
      
      E_11=STATEV(7)
      E_22=STATEV(8)

      G_12=STATEV(10)

      nu_12=STATEV(13) 

      
      XT=STATEV(15) 
      XC=STATEV(16)
      YT=STATEV(17)    
      YC=STATEV(18) 
       
      S12=STATEV(21)   

      
      sig_max(1)= STATEV(24)
      sig_max(2)= STATEV(25)

      sig_max(3)= STATEV(27)


     
      sig_min(1)= STATEV(30)
      sig_min(2)= STATEV(31)

      sig_min(3)= STATEV(33)

     
      ! Aded in 14.for (no use)
      sig_m(1)=STATEV(36)
      sig_m(2)=STATEV(37)

      sig_m(3)=STATEV(39)

      
      
      sig_a(1)=STATEV(42)
      sig_a(2)=STATEV(43)

      sig_a(3)=STATEV(45)

 
      RR(1)=STATEV(48)
      RR(2)=STATEV(49)

      RR(3)=STATEV(51)

      
      
      sig_a_eq(1)=STATEV(54)
      sig_a_eq(2)=STATEV(55)

      sig_a_eq(3)=STATEV(57)
 
      
      Nf_norm(1)=STATEV(60)
      Nf_norm(2)=STATEV(61)

      Nf_norm(3)=STATEV(63)

      
       Nf(1)=STATEV(66)
       Nf(2)=STATEV(67)

       Nf(3)=STATEV(69)

       
       Fiber_Failure=STATEV(72)  
       Matrix_Failure= STATEV(73)
       DW_star=STATEV(74)
       unifiedNf=STATEV(75)
       FiberStaticFailure= STATEV(76)
       MatrixStaticFailure=STATEV(77)
       

c     For information
      cycletime=STATEV(79)      
      NN=STATEV(80)
                 
      END IF
      
     
c     Engineering total strain 
      eps(1)=STATEV(1) + DSTRAN(1)
      !Hidden Lines

      
      CALL StiffnesMatrix2d(E_11,E_22, 
     1                    G_12,
     1                    nu_12, CC )
     
        
        sig(:)=0      !Setting zero for confidence
         DO I=1,3
           do J=1,3
          sig(I) = sig(I) + 
     1                      CC(I,J)*eps(J)
           end do
         END DO
         
c  ****************************************************************************             
c  *******************  failure analysis block ************************
c     Checking Failure: 

c     0- Previous failure
      if (FiberStaticFailure==1.OR. MatrixStaticFailure==1) then
      go to 20
      end if
C      print*,'FiberStaticFailure**=',FiberStaticFailure 

c     1- Fiber  failure  
      call FailureCriterionFiber2d(sig(1), sig(3)    
     1              , XT, XC, S12, Fiber_Failure ) 
      
     
       if (Fiber_Failure>0) then
       call SuddenDegFiber2d( E_11,E_22,G_12,
     1                        nu_12,
     1                     XT,XC,YT,YC,S12)
       FiberStaticFailure=1
       go to 20     ! Go to the end of the program
       end if     
                                                 
c     2- Transverse  failure  
      call FailureCriterionMatrix2d 
     1           ( sig(2), sig(3),
     1           YT, YC, S12, Matrix_Failure )
     
C       print*,'Matrix_Failure**=',Matrix_Failure 
       if (Matrix_Failure>0) then   
      call SuddenDegMatrix2d (E_22,G_12,nu_12
     1                    ,YT,YC,S12 )  
        MatrixStaticFailure=1  
        go to 20 
        end if  

c     updating Max & Min values of the stress      
        ! 10 Hidden LInes

       
       
c   ******************* failure analysis block ***********************
c   ***************************************************************************
  
      !Hidden Lines
c   *************************************************************************************************************************************  
c   ***************** fatige failure analysis block *******************

c      cycletime= cycletime + 0.001  ! do not move this to the end
      ! 10 Hidden Lines
      

c     --------------------------------------------------------
c     --------- Calculating final life (Nf)-------------------

 
      if  ( sig_m(1) > 0 ) then 
      XX=XT
      else
      XX=XC
      end if
         
      
      if  ( sig_m(2) > 0 ) then 
      YY=YT
      else
      YY=YC
      end if
      

            
       DW_star = !Hidden Content
     
       
      unifiedNf = !Hidden Content
      
!       
!c     --------------------------------------------------------
!c     --------- Calculating final life (Nf)-------------------      
!    
!    
!c     -------------------------------------------------------------------------
!c     ----------- stiffness & strength degradation (5 components)--------------

c     In 11 direction:      
      if ( NN > unifiedNf ) then
          ! 30 Hidden Lines
      end if     
   
   
c     In 22 direction: 
      if ( NN > unifiedNf ) then
      ! 32 Hidden Lines
      end if   
      

c  =====================================      
c     In 12 direction:     
      if ( NN > unifiedNf ) then
           !Hidden Lines
      end if
     
c     ----------- stiffness & strength degradation (5 components) --------------
c     -------------------------------------------------------------------------     
        
        
      end if  ! end of fatigue analysis     
c   ***************** fatige failure analysis block *******************
c   ******************************************************************* 
       
  20   CALL StiffnesMatrix2d(E_11,E_22, 
     1                    G_12,
     1                    nu_12, CC ) 
     
     
      ! Hidden LInes
      STATEV(1) = eps(1) 
      STATEV(2) = eps(2) 
      STATEV(3) = eps(3) 

      
c     Stiffness engineering constants:
      STATEV(7)=E_11 
      STATEV(8)=E_22

      STATEV(10)=G_12

      STATEV(13)=nu_12 


c     Strength parameters:
      STATEV(15) = XT
      STATEV(16) = XC 
      STATEV(17) = YT 
      STATEV(18) = YC

      STATEV(21) = S12


c     Max_stress components:     
      STATEV(24) = sig_max(1)
      STATEV(25) = sig_max(2) 

      STATEV(27) = sig_max(3) 

      
c     Min_stress components:      
      STATEV(30) = sig_min(1)
      STATEV(31) = sig_min(2) 

      STATEV(33) = sig_min(3) 

      
c     Mean_stress components:      
      STATEV(36) = sig_m(1)
      STATEV(37) = sig_m(2)

      STATEV(39) = sig_m(3)

      
c     Stress amplitude components:      
      STATEV(42) = sig_a(1)
      STATEV(43) = sig_a(2)

      STATEV(45) = sig_a(3)
 
c     Stress ratio components 
      STATEV(48) =  RR(1)
      STATEV(49) =  RR(2)

      STATEV(51) =  RR(3)

    
c     Equivalent stress components(Goodman method only)    
      STATEV(54) = sig_a_eq(1)
      STATEV(55) = sig_a_eq(2)

      STATEV(57) = sig_a_eq(3)
    
      
c     Normalized life components:      
      STATEV(60) = Nf_norm(1)
      STATEV(61) = Nf_norm(2)

      STATEV(63) = Nf_norm(3)

      
c     Final life components:      
      STATEV(66) = Nf(1)
      STATEV(67) = Nf(2)

      STATEV(69) = Nf(3)

      
c     other usefull quatities:      
      STATEV(72) = Fiber_Failure
      STATEV(73) = Matrix_Failure  
      STATEV(74) = DW_star
      STATEV(75) = unifiedNf
      STATEV(76) = FiberStaticFailure
      STATEV(77) = MatrixStaticFailure
      
c     For information        
      STATEV(79)=cycletime
      STATEV(80) = NN
     
   

      return
      end 
      
  ! --------------Modified for 2D------------------------------------------------        
       
      SUBROUTINE StiffnesMatrix2d(E11,E22,G12,nu12,C ) 

      
c     DEFINING STIFFNESS MATRIX C6*6 IN TERMS OF ENGINEERING CONSTANTS
     
      !Hidden
      END SUBROUTINE StiffnesMatrix2d

c------------------------------------------------------
 
       SUBROUTINE SuddenDegFiber2d( E11,E22,G12,nu12
     1                          ,XT,XC,YT,YC,S12) 

!Hidden
      END SUBROUTINE SuddenDegFiber2d
c-------------------------------------------------------      
     
      SUBROUTINE SuddenDegMatrix2d(E22,G12,nu12
     1                          ,YT,YC,S12 ) 
      !Hidden

      END SUBROUTINE SuddenDegMatrix2d
c-------------------------------------------------------     
     
      SUBROUTINE FailureCriterionFiber2d(sig1, sig3,
     1                          XT, XC, S12, FiberFailure ) 
     
     !Hidden
      END SUBROUTINE FailureCriterionFiber2d
      
c-------------------------------------------------------     

     
      SUBROUTINE FailureCriterionMatrix2d(sig2, sig3,
     1                          YT, YC, S12, MatrixFailure ) 
     
      !Hidden       

      END SUBROUTINE FailureCriterionMatrix2d
c====================================================================
	  
 
	  
