Subroutine Type5995
! TRNSYS Type 5995: Lumped capacitance model - Modified by Daniel Marques to turn into a model of 1-D heat conduction in a PCM slab
! ----------------------------------------------------------------------------------------------------------------------
! This component calculates the transient behavior of a PCM slab, modeled with the finit difference method employing heat capacity method to deal
! with phase change problem. It considers the PCM slab is like a wall. We employ the implicit scheme to solve 1-D heat conduction equation.
!-----------------------------------------------------------------------------------------------------------------------
! Copyright � 2024 University of Aveiro, Portugal, Daniel Marques. All rights reserved.

!export this subroutine for its use in external DLLs.
!DEC$ATTRIBUTES DLLEXPORT :: TYPE5995
 
!-----------------------------------------------------------------------------------------------------------------------
 !Use Statements 
 Use TrnsysConstants
 Use TrnsysFunctions
!-----------------------------------------------------------------------------------------------------------------------

!Variable Declarations
Implicit None !force explicit declaration of local variables

!-----------------------------------------------------------------------------------------------------------------------
!Define a derived type to hold multiple parameters - this is done to set the type of data each node will store
!real(8) is the same as Douple Precision to declare variables as floating-point numbers
type :: NodeData
    Double Precision :: temperature_init 
    Double Precision :: specific_heat
    Double Precision :: thermal_conductivity
    Double Precision :: liq_fract
    Double Precision :: temperature_final
end type NodeData

!Define a derived type to hold multiple parameters - this is done to set the type of data each boundary condition needs
type :: BCsData
    Double Precision :: environment_temperature
    Double Precision :: heat_transfer_coeff
end type BCsData

!Declare an array of NodeData with n elements - this is used to have an array of multiple NodeData structures
type(NodeData), allocatable :: NodesArray(:) !type(NodeData) :: NodesArray(11)
!Declare an allocatable array of BCsData with a size determined at runtime
type(BCsData) :: BCsArray(2) !previously I had: type(BCsData), allocatable :: BCsArray(:) 

!-----------------------------------------------------------------------------------------------------------------------
! Declare all other values needed for the code

Integer :: ii,ee,nn
Double Precision Time,Timestep
Integer CurrentUnit,CurrentType,mode,nBCs,ninp,jj,AA,BB
Double Precision density,thickness,mass_PCM,volume,Cp_PCM,K_PCM,area,T_0,Q_PCM,T_inf,E_in,T_i,T_f,T_bar, &
        Cp_solid,Cp_liquid,K_solid,K_liquid,T_solidus,T_liquidus,Fusion_Enthalpy,Liquid_Fraction,E_out,U_inf,deltaX,E_out_abs,volume_i
Logical found_end
 Double Precision, allocatable :: aa_TA(:),bb_TA(:),cc_TA(:),dd_TA(:),xx_TA(:) !code to declare the arrays needed for the Thomas Algorithm for FDM - implicit scheme
 Double Precision, allocatable :: c_prime(:), d_prime(:) !also needed fot the Thomas Algorithm

Double Precision :: temp !variable used also for Thomas Algorithm

!-----------------------------------------------------------------------------------------------------------------------
!Get the Global Trnsys Simulation Variables
 Time=getSimulationTime()
 Timestep=getSimulationTimeStep()
 CurrentUnit = getCurrentUnit()
 CurrentType = getCurrentType()

!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Set the Version Number for This Type
 If(getIsVersionSigningTime()) Then
     Call SetTypeVersion(17)
     Return
 Endif
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Do All of the Last Call Manipulations Here
 If(getIsLastCallofSimulation()) Then

 !-----------------------------------------------------------------------------------------------------------------------
! Deallocate BCsArray to ensure the code is not consuming memory resources in the computer anymore - this is just a good practice
 ! deallocate(BCsArray)
! Deallocating the Thomas Algorithms arrays to ensure we stop consuming memory resources
 deallocate(aa_TA,bb_TA,cc_TA,dd_TA,xx_TA,c_prime,d_prime)
 ! Deallocate NodesArray
 deallocate(NodesArray)

    Return
 Endif
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Perform Any "End of Timestep" Manipulations That May Be Required
 If(getIsEndOfTimestep()) Then
    Do ii=1,nn
        Call SetStaticArrayValue(ii,getStaticArrayValue(ii))
    End Do

    !I have decided to comment and avoid this Report - this was from the older version of Type59
	!If (getIsIncludedInSSR()) Then
       !Call updateReportMinMax(1,getOutputValue(1))
       !Call updateReportIntegral(1,getInputValue(2))
       !Call updateReportIntegral(2,getOutputValue(2))  
    !EndIf
	Return
 Endif
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Do All of the "Very First Call of the Simulation Manipulations" Here
 If (getIsFirstCallofSimulation()) Then
    ninp = getNumberOfInputs() !set the number of INPUTS to the number found in the deck
    nn = getParameterValue(14) ! Retrieve nn from the proforma parameters
    
!  Tell the TRNSYS Engine How This Type Works
    Call SetNumberofParameters(14)
    Call SetNumberofInputs(ninp)           
    Call SetNumberofDerivatives(0)           
    Call SetNumberofOutputs(5+nn)           
    Call SetIterationMode(1)           
    Call SetNumberStoredVariables(nn,0)

! Set the Correct Input and Output Variable Types
    Call SetInputUnits(1,'PW1')
    Do jj=3,ninp,2
    Call SetInputUnits(jj-1,'TE1')
    Call SetInputUnits(jj,'HT1')
    End Do
    Call SetOutputUnits(1,'TE1')
    Call SetOutputUnits(2,'PW1')
    Call SetOutputUnits(3,'DM1')
    Call SetOutputUnits(4,'AR1')
    Call SetOutputUnits(5,'EN1')
    Do jj=6,nn+5
    Call SetOutputUnits(jj,'TE1')
    End Do

 !  Set up this Type's entry in the SSR - also decided to comment and avoid this part of the code - it was from the original type59
    !If (getIsIncludedInSSR()) Then
       !Call setNumberOfReportVariables(2,1,4,0) !(nInt,nMinMax,nValues,nText)   
    !EndIf   
    
    Return
 EndIf
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Do All of the "Start Time" Manipulations Here - There Are No Iterations at the Intial Time
 If (getIsStartTime()) Then

!  Read in the Values of the Parameters from the Input File
    mode = jfix(getParameterValue(1) + 0.1)
    density = getParameterValue(2)
    thickness = getParameterValue(3)
    mass_PCM = getParameterValue(4)
    Cp_solid = getParameterValue(5)
    Cp_liquid = getParameterValue(6)
    T_0 = getParameterValue(7)
    K_solid = getParameterValue(8)
    K_liquid = getParameterValue(9)
    Fusion_Enthalpy = getParameterValue(10)
    T_solidus = getParameterValue(11)
    T_liquidus = getParameterValue(12)
    nBCs = getParameterValue(13)
    nn = getParameterValue(14) !initializing this variable for the Thomas algoritm equal to the number of nodes

!  Check the Parameters for Problems
	if (mode /= 1) Call FoundBadParameter(1,'Fatal','The mode must be 1.')
	if (density <= 0.0 ) Call FoundBadParameter(2,'Fatal','The density must be greater than 0.')
	if (thickness <= 0.0 )	Call FoundBadParameter(3,'Fatal','The thickness of the PCM slab must be greater than 0.')
	if (mass_PCM <= 0.0 ) Call FoundBadParameter(4,'Fatal','The mass of PCM must be greater than 0.')
	if (Cp_solid <= 0.0 ) Call FoundBadParameter(5,'Fatal','The specific heat solid must be greater than 0.')
    if (Cp_liquid <= 0.0 ) Call FoundBadParameter(6,'Fatal','The specific heat liquid must be greater than 0.')
    if (K_solid <= 0.0 ) Call FoundBadParameter(8,'Fatal','The thermal conductivity solid must be greater than 0.')
    if (K_liquid <= 0.0 ) Call FoundBadParameter(9,'Fatal','The thermal conductivity liquid must be greater than 0.')
    if (Fusion_Enthalpy <= 0.0 ) Call FoundBadParameter(10,'Fatal','The fusion latent heat must be greater than 0.')
    if (nBCs <= 0.0 ) Call FoundBadParameter(13,'Fatal','The number of boundary conditions must be greater than 0.')

!  Set up the SSR array - Decided to comment the whole piece of code - it was originally from the type59
    !If (getIsIncludedInSSR()) Then
       !Call initReportValue(1,'Density',density,'kg/m3')
       !Call initReportValue(2,'Volume',volume,'m3')
       !Call initReportValue(3,'Specific Heat',Cp_PCM,'kJ/kg-K')
       !Call initReportValue(4,'Surface Area',area,'m2')
       !Call initReportMinMax(1,'Temperature','C')
       !Call initReportIntegral(1,'Energy Input','kJ/hr','kJ')
       !Call initReportIntegral(2,'Heat Transfer to Surroundings','kJ/hr','kJ')          
    !EndIf
    
!Allocating the vairable nn to the arrays (different classes of arrays)

allocate(NodesArray(nn)) !allocating the array of nodes with the size nn= number of nodes decided in the proforma
allocate(aa_TA(nn),bb_TA(nn),cc_TA(nn),dd_TA(nn),xx_TA(nn),c_prime(nn),d_prime(nn)) !allocating the arrays with the size nn = number of nodes

! Initialize the array elements - since this is an initialization i placed it in the group of tasks to performe at getisstarttime()
Do ii=1,nn
    NodesArray(ii)%temperature_init = 0.0
    NodesArray(ii)%specific_heat = 0.0
    NodesArray(ii)%thermal_conductivity = 0.0
    NodesArray(ii)%liq_fract = 0.0
    NodesArray(ii)%temperature_final = 0.0
End Do

! Initialize all other arrays
    aa_TA = 0.0
    bb_TA = 0.0
    cc_TA = 0.0
    dd_TA = 0.0
    xx_TA = 0.0
    c_prime = 0.0
    d_prime = 0.0
    BCsArray%environment_temperature = 0.0
    BCsArray%heat_transfer_coeff = 0.0

!  Set the Initial Values of the Outputs
    Call SetOutputValue(1,0.d0)
    Call SetOutputValue(2,0.d0)
    Call SetOutputValue(3,0.d0)
    Call SetOutputValue(4,0.d0)
    Call SetOutputValue(5,0.d0)
    Do jj=6,nn+5
    Call SetOutputValue(jj,0.d0)
    End Do

!  Set Initial Values of Storage Variables - I use this to assign to the staticarrayvalues the T_0 (initial temperature of the PCM)
Do ii=1,nn
    Call SetStaticArrayValue(ii,T_0)
End Do

!  Initialize cumulative energy at the start of the simulation - this is useful for an ouput of the type
    E_out = 0.0 
!  Initialize temp variable at the start of the simulation - this is a variable that will be used in the Thomas Algorithm for solving the equations' system from the implicit scheme
    temp = 0.0
   
    Return
 Endif
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!ReRead the Parameters if Another Unit of This Type Has Been Called Last
 If(getIsReReadParameters()) Then
    mode = jfix(getParameterValue(1) + 0.1)
    density = getParameterValue(2)
    thickness = getParameterValue(3)
    mass_PCM = getParameterValue(4)
    Cp_solid = getParameterValue(5)
    Cp_liquid = getParameterValue(6)
    T_0 = getParameterValue(7)
    K_solid = getParameterValue(8)
    K_liquid = getParameterValue(9)
    Fusion_Enthalpy = getParameterValue(10)
    T_solidus = getParameterValue(11)
    T_liquidus = getParameterValue(12)
    nBCs = getParameterValue(13)
 Endif
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Perform one time calculations
volume = mass_PCM/density
area = volume/thickness
deltaX = thickness/(nn-1)
volume_i = area*deltaX
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Retrieve the Stored Variables - doing this to make sure that the Array of Nodes is always being updated with the new Initial temperature values accurately stored in staticarrayvalues
Do AA=1,nn
 NodesArray(AA)%temperature_init = getStaticArrayValue(AA)
End Do
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
!Allocate the rest of the initial values of the properties to the nodes, (Cp, K, fL), given the values it retrived of initial temperature in previous Do loop

Do AA=1,nn
    If (NodesArray(AA)%temperature_init < T_solidus) Then
        NodesArray(AA)%specific_heat = Cp_solid
        NodesArray(AA)%thermal_conductivity = K_solid
        NodesArray(AA)%liq_fract = 0.0
    Else If (NodesArray(AA)%temperature_init > T_liquidus) Then
        NodesArray(AA)%specific_heat = Cp_liquid
        NodesArray(AA)%thermal_conductivity = K_liquid
        NodesArray(AA)%liq_fract = 1.0
    Else
        NodesArray(AA)%specific_heat = (Cp_liquid+Cp_solid)/2.0 + Fusion_Enthalpy/(T_liquidus-T_solidus)
        NodesArray(AA)%thermal_conductivity = (NodesArray(AA)%temperature_init-T_solidus)/(T_liquidus-T_solidus)*K_liquid + (1-(NodesArray(AA)%temperature_init-T_solidus)/(T_liquidus-T_solidus))*K_solid
        NodesArray(AA)%liq_fract = (NodesArray(AA)%temperature_init-T_solidus)/(T_liquidus-T_solidus)
    Endif
End Do
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
! Allocate the BCsArray based on the value of nBCs
! allocate(BCsArray(nBCs))
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
! Get the Current Inputs to the Model
 E_in = getInputValue(1) ! variable where a heat gain or removal (W) to apply to all PCM domain is stored
 Do ee = 1,nBCs
    T_inf = getInputValue(ee*2)
    U_inf = getInputValue(ee*2+1)
    BCsArray(ee)%environment_temperature = T_inf
    BCsArray(ee)%heat_transfer_coeff = U_inf
 End Do
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
! Performing calculations based on the FDM to calculate the final temperatures of each node

! Defining the coefficients associated to nodes i-1 (aa), i (bb), i+1 (cc) and right-hand side of the equations (dd) to initialize and populate the arrays with size nn

! Surface node #1 - exterior boundary condition
aa_TA(1) = 0.0
bb_TA(1) = area * (BCsArray(1)%heat_transfer_coeff + (NodesArray(1)%thermal_conductivity + NodesArray(2)%thermal_conductivity)/(2*deltaX) + (density * NodesArray(1)%specific_heat * deltaX / (2*Timestep)))
cc_TA(1) = -(NodesArray(1)%thermal_conductivity + NodesArray(2)%thermal_conductivity) * area / (2*deltaX)
dd_TA(1) = (density * NodesArray(1)%specific_heat * area * deltaX * NodesArray(1)%temperature_init) / (2*Timestep) + (BCsArray(1)%heat_transfer_coeff * area * BCsArray(1)%environment_temperature) + E_in/(2*(nn-1))

! Interior nodes #2 - #nn-1 
Do ii=2,nn-1
    aa_TA(ii) = -(NodesArray(ii)%thermal_conductivity + NodesArray(ii-1)%thermal_conductivity) * volume_i / (deltaX*deltaX*2)
    bb_TA(ii) = volume_i * (density*NodesArray(ii)%specific_heat/Timestep + (2*NodesArray(ii)%thermal_conductivity+NodesArray(ii+1)%thermal_conductivity+NodesArray(ii-1)%thermal_conductivity)/(deltaX*deltaX*2))
    cc_TA(ii) = -(NodesArray(ii)%thermal_conductivity + NodesArray(ii+1)%thermal_conductivity)*volume_i/(deltaX*deltaX*2)
    dd_TA(ii) = density * NodesArray(ii)%specific_heat * volume_i * NodesArray(ii)%temperature_init / Timestep + E_in/(nn-1)
End Do

! Surface node #nn - interior boundary condition
aa_TA(nn) = -(NodesArray(nn)%thermal_conductivity + NodesArray(nn-1)%thermal_conductivity) * area / (2*deltaX)
bb_TA(nn) = area * (BCsArray(2)%heat_transfer_coeff + (NodesArray(nn)%thermal_conductivity + NodesArray(nn-1)%thermal_conductivity)/(2*deltaX) + (density * NodesArray(nn)%specific_heat * deltaX / (2*Timestep)))
cc_TA(nn) = 0.0
dd_TA(nn) = (density * NodesArray(nn)%specific_heat * area * deltaX * NodesArray(nn)%temperature_init) / (2 * Timestep) + (BCsArray(2)%heat_transfer_coeff * area * BCsArray(2)%environment_temperature) + E_in/(2*(nn-1))

! Thomas algorithm step 1 - Forward sweep
c_prime(1) = cc_TA(1)/bb_TA(1)
d_prime(1) = dd_TA(1)/bb_TA(1)

Do AA = 2,nn
    temp = bb_TA(AA) - aa_TA(AA)*c_prime(AA-1)
    c_prime(AA) = cc_TA(AA)/temp
    d_prime(AA) = (dd_TA(AA) - aa_TA(AA)*d_prime(AA-1))/temp
End Do

! Thomas algorithm step 2 - Backward substitution
xx_TA(nn) = d_prime(nn)
Do AA = (nn-1),1,-1
    xx_TA(AA) = d_prime(AA) - c_prime(AA) * xx_TA(AA+1)
End Do
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
! Set the values in storage - after calculations
Do BB=1,nn
NodesArray(BB)%temperature_final = xx_TA(BB)
Call SetStaticArrayValue(BB,NodesArray(BB)%temperature_final)
End Do
!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
! Perform calculations after solving the FDM implicit scheme at each time step

! Calculate average temperature_final of our PCM domain
T_f = 0.0 !Initialize T_f = 0.0
Liquid_Fraction = 0.0 !initialize Liquid_Fraction

Do BB=1,nn
T_f = T_f + NodesArray(BB)%temperature_final

! calculate the average liquid fraction of the all domain based on average temperature of all nodes
If (NodesArray(BB)%temperature_final < T_solidus) Then
        Liquid_Fraction = Liquid_Fraction + 0.0
    Else If (NodesArray(BB)%temperature_final > T_liquidus) Then
        Liquid_Fraction = Liquid_Fraction + 1.0
    Else
        Liquid_Fraction = Liquid_Fraction + (NodesArray(BB)%temperature_final-T_solidus)/(T_liquidus-T_solidus)
Endif
End Do

T_f = T_f/nn !calculates the average of final temperatures for all the nodes
Liquid_Fraction = Liquid_Fraction/nn

! calculate the absorbed/released energy during each time step
Q_PCM = 0.0 ! initialize
! Loop through all nodes to calculate the total heat variation
! node 1 first - surface node
Q_PCM = Q_PCM + density*area*deltaX*NodesArray(1)%specific_heat*(NodesArray(1)%temperature_final-NodesArray(1)%temperature_init)/(2*Timestep)
! all other interior nodes then
Do BB=2,nn-1
Q_PCM = Q_PCM + density*area*deltaX*NodesArray(BB)%specific_heat*(NodesArray(BB)%temperature_final-NodesArray(BB)%temperature_init)/Timestep
End Do
! final node - other surface node
Q_PCM = Q_PCM + density*area*deltaX*NodesArray(nn)%specific_heat*(NodesArray(nn)%temperature_final-NodesArray(nn)%temperature_init)/(2*Timestep)
! Q_PCM now contains the total heat variation for the entire PCM domain

! Calculate the cumulative energy absorbed/released by the PCM (E_out)
E_out = E_out + Q_PCM*Timestep

! Calculate the absolute value of E_out
E_out_abs = ABS(E_out)

!-----------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------
! Set the Outputs from this Model
 Call SetOutputValue(1,T_f)
 Call SetOutputValue(2,Q_PCM)
 Call SetOutputValue(3,Liquid_Fraction)
 Call SetOutputValue(4,area)
 Call SetOutputValue(5,E_out_abs) !Output the absolute value
 Do jj=6,nn+5
    Call SetOutputValue(jj,NodesArray(jj-5)%temperature_final)
 End Do
!-----------------------------------------------------------------------------------------------------------------------
 
 Return
End