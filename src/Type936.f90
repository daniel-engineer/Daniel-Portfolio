Subroutine Type936
!-----------------------------------------------------------------------------------------------------------------------
! This subroutine models a refrigerator.
!-----------------------------------------------------------------------------------------------------------------------
! Copyright © 2011 Thermal Energy System Specialists, LLC. All rights reserved.

!export this subroutine for its use in external DLLs.
!DEC$ATTRIBUTES DLLEXPORT :: TYPE936

!Use Statements
Use TrnsysConstants
Use TrnsysFunctions
      
!Variable Declarations
Implicit None !force explicit declaration of local variables
Double Precision Time,Timestep
Integer CurrentUnit,CurrentType
Double Precision aa,bb,capacitance,u_value,Ti,Tf,Tave,area,Q_skin,Q_stored,fvol_fridge,fvol_freezer,Tset_fridge, &
        Tset_freezer,deadband,cap_rated,COP_rated,capacity,COP,P_condfan,P_evapfan,Power,Q_rejected,Q_cond, &
        T_zone,control_now,control_last,T_control,T_evap,x(2),y(2),delt_now,delt_tot,Ti_now,Tave_tot,x_tot, &
        Q_evap,P_comp,control_prev
Integer n_temps_r,lu_data,n_temps_z,nx(2)
Logical found_end

!Get the Global Trnsys Simulation Variables
 Time=getSimulationTime()
 Timestep=getSimulationTimeStep()
 CurrentUnit = getCurrentUnit()
 CurrentType = getCurrentType()
 
!Set the Version Number for This Type
 If (getIsVersionSigningTime()) Then
     Call SetTypeVersion(17)
     Return
 Endif
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Do All of the Last Call Manipulations Here
 If (getIsLastCallofSimulation()) Then
    Return
 Endif

!-----------------------------------------------------------------------------------------------------------------------
!Perform Any "After Convergence" Manipulations That May Be Required
 If (getIsEndOfTimestep()) Then
    Call SetStaticArrayValue(1,getStaticArrayValue(2))
    Call SetStaticArrayValue(3,getStaticArrayValue(4))
    Return
 Endif

!-----------------------------------------------------------------------------------------------------------------------
!Do All of the "Very First Call of the Simulation Manipulations" Here
 If (getIsFirstCallofSimulation()) Then

!  Tell the TRNSYS Engine How This Type Works
    Call SetNumberofParameters(13)           
    Call SetNumberofInputs(2)      
    Call SetNumberofDerivatives(0)           
    Call SetNumberofOutputs(5)           
    Call SetIterationMode(1)           
    Call SetNumberStoredVariables(4,0)

!  Set the Correct Input and Output Variable Types
    Call SetInputUnits(1,'TE1')    
    Call SetInputUnits(2,'HT1')    
    Call SetOutputUnits(1,'TE1')    
    Call SetOutputUnits(2,'PW1')    
    Call SetOutputUnits(3,'PW1')    
    Call SetOutputUnits(4,'PW1')    
    Call SetOutputUnits(5,'DM1')    
    
    Return
 EndIf

!-----------------------------------------------------------------------------------------------------------------------
!Do All of the First Timestep Manipulations Here - There Are No Iterations at the Intial Time
 If (getIsStartTime()) Then

! Read in the Values of the Parameters from the Input File
    capacitance = getParameterValue(1)
    area = getParameterValue(2)
    fvol_fridge = getParameterValue(3)
    fvol_freezer = 1.-getParameterValue(3)
    Tset_fridge = getParameterValue(4)
    Tset_freezer = getParameterValue(5)
    deadband = getParameterValue(6)
    lu_data = JFIX(getParameterValue(7)+0.5)
    n_temps_z = JFIX(getParameterValue(8)+0.5)
    n_temps_r = JFIX(getParameterValue(9)+0.5)
    cap_rated = getParameterValue(10)
    COP_rated = getParameterValue(11)
    P_condfan = getParameterValue(12)
    P_evapfan = getParameterValue(13)

!  Check the Parameters for Problems
    If (capacitance <= 0.) Call FoundBadParameter(1,'Fatal','The thermal capacitance must be greater than 0.')
    If (area < 0.) Call FoundBadParameter(2,'Fatal','The surface area cannot be negative.')
    If (fvol_fridge < 0.) Call FoundBadParameter(3,'Fatal','The volume fraction for the refrigerator cannot be negative.')
    If (fvol_fridge > 1.) Call FoundBadParameter(3,'Fatal','The volume fraction for the refrigerator cannot be greater than 1.')
    If (deadband < 0.) Call FoundBadParameter(6,'Fatal','The temperature deadband for control cannot be negative.')
    If (lu_data < 10) Call FoundBadParameter(7,'Fatal','The logical unit number for the file with the refrigerator performance data cannot be less than 10.')
    If (n_temps_z < 1) Call FoundBadParameter(8,'Fatal','The number of unique zone temperatures for which there is performance data cannot be less than 1.')
    If (n_temps_r < 1) Call FoundBadParameter(9,'Fatal','The number of unique evaporator temperatures for which there is performance data cannot be less than 1.')
    If (cap_rated <= 0.) Call FoundBadParameter(10,'Fatal','The rated cooling capacity must be greater than 0.')
    If (COP_rated <= 0.) Call FoundBadParameter(11,'Fatal','The rated COP must be greater than 0.')
    If (P_condfan < 0.) Call FoundBadParameter(12,'Fatal','The condenser fan power cannot be negative.')
    If (P_evapfan < 0.) Call FoundBadParameter(13,'Fatal','The evaporator fan power cannot be negative.')

! Set the outputs to initial values.
    Call SetOutputValue(1,fvol_freezer*Tset_freezer+fvol_fridge*Tset_fridge)
    Call SetOutputValue(2,0.d0)
    Call SetOutputValue(3,0.d0)
    Call SetOutputValue(4,0.d0)
    Call SetOutputValue(5,0.d0)

!  Set the initial storage variables
    Call SetStaticArrayValue(1,fvol_freezer*Tset_freezer+fvol_fridge*Tset_fridge)
    Call SetStaticArrayValue(2,fvol_freezer*Tset_freezer+fvol_fridge*Tset_fridge)
    Call SetStaticArrayValue(3,0.d0)
    Call SetStaticArrayValue(4,0.d0)

    Return
 EndIf
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!ReRead the Parameters if Another Unit of This Type Has Been Called Last
 If (getIsReReadParameters()) Then
    capacitance = getParameterValue(1)
    area = getParameterValue(2)
    fvol_fridge = getParameterValue(3)
    fvol_freezer = 1.-getParameterValue(3)
    Tset_fridge = getParameterValue(4)
    Tset_freezer = getParameterValue(5)
    deadband = getParameterValue(6)
    lu_data = JFIX(getParameterValue(7)+0.5)
    n_temps_z = JFIX(getParameterValue(8)+0.5)
    n_temps_r = JFIX(getParameterValue(9)+0.5)
    cap_rated = getParameterValue(10)
    COP_rated = getParameterValue(11)
    P_condfan = getParameterValue(12)
    P_evapfan = getParameterValue(13)
 EndIf
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Get the Current Inputs to the Model
 T_zone = getInputValue(1) 
 u_value = getInputValue(2)

 If (u_value < 0.) Call FoundBadInput(2,'Fatal','The heat transfer coefficient is negative.')
 If (ErrorFound()) Return
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Retrieve the Values from Storage
 Ti = getStaticArrayValue(1)
 Tf = getStaticArrayValue(2)
 control_last = getStaticArrayValue(3)
!-----------------------------------------------------------------------------------------------------------------------

! Calculate the average temperature setting for the device
 T_control = fvol_freezer*Tset_freezer+fvol_fridge*Tset_fridge
 If (fvol_freezer > 0.) Then
    T_evap = Tset_freezer
 Else
    T_evap = Tset_fridge
 EndIf
! Get the performance of the device at the current conditions
 nx(1) = n_temps_r
 nx(2) = n_temps_z
 x(1) = T_evap
 x(2) = T_zone
 Call InterpolateData(lu_data,2,nx,2,x,y)
 If (ErrorFound()) Return
 capacity = cap_rated*y(1)
 COP = COP_rated*y(2)
! Set the new control signal to the value at the end of the timestep
 If (Ti >= (T_control+deadband/2.)) Then
    control_now = 1.
 ElseIf (Ti <= (T_control-deadband/2.)) Then
    control_now = 0.
 Else
    control_now = control_last
 EndIf
 delt_now = Timestep
 Ti_now = Ti
 found_end = .false.
 Tave_tot = 0.
 x_tot = 0.
 delt_tot = 0.

! The fridge is running
 30 If (control_now > 0.5) Then
! Run for the full timestep and see what happens
    control_prev=1.
! Set up the governing differential equation in the form dT/dt=aT+b
    bb = (-capacity+u_value*area*T_zone)/capacitance
    aa = -u_value*area/capacitance
! Solve the diffeq analytically
    If (aa == 0.) Then
       Tf = Ti_now+bb*delt_now
       Tave = Ti_now+bb*delt_now/2.
    Else
       Tf = Ti_now*(DEXP(aa*delt_now))+bb/aa*(DEXP(aa*delt_now))-bb/aa
       Tave = 1./aa/delt_now*(Ti_now+bb/aa)*((DEXP(aa*delt_now))-1.)-bb/aa
    EndIf
! Check the resultant temperature
    If (Tf >= (T_control-deadband/2.)) Then
       delt_now = delt_now
       control_now = 1.
       found_end = .true.
    Else
! Calculate the time to get to the setpoint
       Tf = T_control-deadband/2.
       If (aa == 0.) Then
          delt_now = DMIN1(delt_now,((Tf-Ti_now)/bb))
          Tf = Ti_now+bb*delt_now
          Tave = Ti_now+bb*delt_now/2.
       Else
          delt_now = DMIN1(delt_now,(DLOG((Tf+bb/aa)/(Ti_now+bb/aa))/aa))
          Tf = Ti_now*(DEXP(aa*delt_now))+bb/aa*(DEXP(aa*delt_now))-bb/aa
          Tave = 1./aa/delt_now*(Ti_now+bb/aa)*((DEXP(aa*delt_now))-1.)-bb/aa
       EndIf
       control_now = 0.
    EndIf
 Else
! Set up the governing differential equation in the form dT/dt=aT+b
    bb = (u_value*area*T_zone)/capacitance
    aa = -u_value*area/capacitance
    control_prev = 0.
! Solve the diffeq analytically
    If (aa == 0.) Then
       Tf = Ti_now+bb*delt_now
       Tave = Ti_now+bb*delt_now/2.
    Else
       Tf = Ti_now*(DEXP(aa*delt_now))+bb/aa*(DEXP(aa*delt_now))-bb/aa
       Tave = 1./aa/delt_now*(Ti_now+bb/aa)*((DEXP(aa*delt_now))-1.)-bb/aa
    EndIf
! Check the resultant temperature
    If (Tf <= (T_control+deadband/2.)) Then
       delt_now = delt_now
       control_now = 0.
       found_end = .true.
    Else
! Calculate the time to get to the setpoint
       Tf = T_control+deadband/2.
       If (aa == 0.) Then
          delt_now = DMIN1(delt_now,((Tf-Ti_now)/bb))
          Tf = Ti_now+bb*delt_now
          Tave = Ti_now+bb*delt_now/2.
       Else
          delt_now = DMIN1(delt_now,(DLOG((Tf+bb/aa)/(Ti_now+bb/aa))/aa))
          Tf = Ti_now*(DEXP(aa*delt_now))+bb/aa*(DEXP(aa*delt_now))-bb/aa
          Tave = 1./aa/delt_now*(Ti_now+bb/aa)*((DEXP(aa*delt_now))-1.)-bb/aa
       EndIf
       control_now = 1.
    EndIf
 EndIf
! Update the temperatures
 Tave_tot = Tave_tot+Tave*delt_now/Timestep
 Ti_now = Tf
! Update the run-time counter
 x_tot = x_tot+control_prev*delt_now/Timestep
! Set the remaining time      
 delt_tot = delt_tot+delt_now
 delt_now = Timestep-delt_tot
! Check to see if we should run again
 If (.not.found_end) Goto 30
! Calculate the energy flows from the refrigerator
 Q_skin = u_value*area*(T_zone-Tave_tot)
 Q_stored = capacitance*(Tf-Ti)/Timestep
! Calculate the energy removed from the space
 Q_evap = capacity*x_tot
! Calculate the power of the compressor
 P_comp = DMAX1(0.,(Q_evap/COP-P_evapfan*x_tot-P_condfan*x_tot))
! Calculate the total heat rejection
 Q_cond = Q_evap+P_comp
 Q_rejected = Q_cond+P_condfan*x_tot
 Power = P_condfan*x_tot+P_evapfan*x_tot+P_comp

!-----------------------------------------------------------------------------------------------------------------------
!Set the values in storage
 Call SetStaticArrayValue(1,Ti)
 Call SetStaticArrayValue(2,Tf)
 Call SetStaticArrayValue(3,control_last)
 Call SetStaticArrayValue(4,control_now)

!-----------------------------------------------------------------------------------------------------------------------
! Set outputs
 Call SetOutputValue(1,Tave_tot)
 Call SetOutputValue(2,Q_skin)
 Call SetOutputValue(3,Q_rejected)
 Call SetOutputValue(4,Power)
 Call SetOutputValue(5,x_tot)

 Return
End

