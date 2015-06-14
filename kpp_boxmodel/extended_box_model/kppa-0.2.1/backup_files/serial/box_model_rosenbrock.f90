!-------------------- BEGIN box_model_rosenbrock.f90 BEGIN -------------------
! @file box_model_rosenbrock.f90                                              
! @author charlesj                                                            
! @date 2014-10-16 14:47:29.836414                                            
! @brief Solves the system y' = F(t,y) using a Rosenbrock method              
!                                                                             
! Solves the system y' = F(t,y) using a Rosenbrock method defined by:         
!                                                                             
!     G = 1 / (H*gamma) - Jacobian(t0,Y0)                                     
!     T_i = t0 + Alpha(i) * H                                                 
!     Y_i = Y0 + \sum_{j=1}^{i-1} A(i,j)*K_j                                  
!     G * K_i = F(T_i, Y_i) + \sum_{j=1}^S C(i,j)/H * K_j                     
!               + gamma(i)*dF/dT(t0, Y0)                                      
!     Y1 = Y0 + \sum_{j=1}^S M(j)*K_j                                         
!                                                                             
! For details on Rosenbrock methods and their implementations:                
!     (1) E. Harier and G. Wanner,                                            
!         "Solving Ordenary Differential Equations II: stiff and              
!         differential-algebraic problems." Computational Mathematics,        
!         Springer-Verlag (1996)                                              
!     (2) KPP - the Kinetic PreProcessor.                                     
!         http://people.cs.vt.edu/~asandu/Software/Kpp/                       
!                                                                             
! Rosenbrock implementations in both (1) and (2) inspired this code.          
! This code presents an interface similar to the KPP implementation           
! for compatibility with existing systems.                                    
!                                                                             
! -- Explanation of integer input parameters:                                 
!                                                                             
!     idata(1) == 0 : F = F(t,y) Depends on T (non-autonomous).               
!              != 0 : F = F(y)   Independent of T (autonomous).               
!     idata(2) == 0 : Use all values in tolerance vectors.                    
!              != 0 : Use only the first value in the tolerance vectors.      
!     idata(3) == 0 : Maximum number of integration steps = 100000.           
!              != 0 : Maximum number of integration steps = idata(3).         
!     idata(4) == 0 : Method is Ros4.                                         
!              == 1 : Method is Ros2.                                         
!              == 2 : Method is Ros3.                                         
!              == 3 : Method is Ros4.                                         
!              == 4 : Method is Rodas3.                                       
!              == 5 : Method is Rodas4.                                       
!              >= 6 : Error.                                                  
!     idata(5) == 0 : Assume tolerance vectors are reasonably valued.         
!              != 0 : Check tolerance vectors for unreasonable values.        
!                                                                             
! -- Explanation of real value input parameters:                              
!                                                                             
!     rdata(1): Lower bound on the integration step size.                     
!               Default: 0.0                                                  
!     rdata(2): Upper bound on the integration step size.                     
!               Default: abs(tend - tstart)                                   
!     rdata(3): Starting value for the integration step size.                 
!               Default: minimum step size                                    
!     rdata(4): Lower bound on step decrease factor.                          
!               Default: 0.2                                                  
!     rdata(5): Upper bound on step increase factor.                          
!               Default: 6.0                                                  
!     rdata(6): Step decrease factor after step rejection.                    
!               Default: 0.1                                                  
!     rdata(7): Safety factor in computation of new step size.                
!               Default: 0.9                                                  
!                                                                             
! -- Explanation of integer output parameters:                                
!                                                                             
!     idata(11): Number of function evaluations.                              
!     idata(12): Number of Jacobian evaluations.                              
!     idata(13): Number of steps taken.                                       
!     idata(14): Number of accepted steps.                                    
!     idata(15): Number of rejected steps.                                    
!     idata(16): Number of LU decompositions.                                 
!     idata(17): Number of forward/backward substitutions.                    
!     idata(18): Number of singular matrix decompositions.                    
!     idata(20): Integrator exit status.                                      
!                Zero indicates success.                                      
!                Positive values indicate success with warning.               
!                Negative values indicate failure.                            
!                                                                             
! -- Explanation of real-value output parameters:                             
!                                                                             
!     rdata(11): The time corresponding to the computed Y upon return.        
!     rdata(12): The last accepted step before exit.                          
!                Use this value as rdata(2] in subsequent runs.               
!     rdata(13): Scaled norm of the error vector on exit.                     
!                                                                             
! This file was generated by Kppa: http://www.paratools.com/Kppa              
!-----------------------------------------------------------------------------


MODULE box_model_rosenbrock

  USE box_model_parameters
  USE box_model_blas
  USE box_model_rates
  USE box_model_function
  USE box_model_decomp
  USE box_model_solve
  USE box_model_jacobian

  IMPLICIT NONE



    ! Machine epsilon
    REAL(8), PARAMETER :: REAL_EPSILON = EPSILON(REAL(1.0D0))

    ! Minimum time delta
    REAL(8), PARAMETER :: MIN_DELT = 10.0 * REAL_EPSILON

  CONTAINS

!--------------------------------- Integrate ---------------------------------
! Kppa-generated time stepping integrator                                     
!                                                                             
! @param[in,out] var    Variable species concentrations                       
! @param[in,out] fix    Fixed species concentrations                          
! @param[in]     idx    Current grid cell index                               
! @param[in]     tstart Integration start time                                
! @param[in]     tend   Integration end time                                  
! @param[in]     abstol Absolute integration tolerances for variable species  
! @param[in]     reltol Relative integration tolerances for variable species  
! @param[in,out] idata  Integer integration in/out parameters                 
! @param[in,out] rdata  Real value integration in/out parameters              
!-----------------------------------------------------------------------------
  SUBROUTINE Integrate(var, fix, idx, tstart, tend, abstol, reltol, idata, &
      rdata, TEMP)
    IMPLICIT NONE

    REAL(8), INTENT(INOUT) :: var(82)
    REAL(8), INTENT(INOUT) :: fix(1)
    INTEGER, INTENT(IN) :: idx
    REAL(8), INTENT(IN) :: tstart
    REAL(8), INTENT(IN) :: tend
    REAL(8), INTENT(IN) :: abstol(82)
    REAL(8), INTENT(IN) :: reltol(82)
    INTEGER, INTENT(INOUT) :: idata(20)
    REAL(8), INTENT(INOUT) :: rdata(20)
    REAL(8), INTENT(IN) :: TEMP


        ! .................. Rosenbrock method parameters ....................

        CHARACTER*20 :: name        ! Method name
        INTEGER :: nStage           ! Number of stages, from 2 to 6
        REAL(8) :: invLoEst       ! Inverse local order estimation
        REAL(8) :: M(6)           ! New step solution coefficients
        REAL(8) :: E(6)           ! Error estimation coefficients
        REAL(8) :: alpha(6)       ! Y_stage_i ~ Y( T + H*alpha_i )
        REAL(8) :: gam(6)         ! Gamma_i = \sum_j gamma_{i,j}

        ! Coefficient matrices A and C are strictly lower triangular.
        ! The subdiagonal elements are stored in row-wise order:
        ! A(2,1)=A(1), A(3,1)=A(2), A(3,2)=A(3), etc.
        REAL(8) :: A(15)
        REAL(8) :: C(15)

        ! F(i) == .TRUE.  : stage i will use the func. evaluation from stage i-1
        ! F(i) == .FALSE. : stage i will evaluate function
        LOGICAL :: F(6)

        ! .................... Integration parameters ....................

        REAL(8) :: spanT         ! Integration time span (positive value)
        LOGICAL :: autonomous   ! idata(1): .FALSE. if F = F(t,y)
        INTEGER :: nTol         ! idata(2): Length of the tolerance vectors, 1 = scalar
        INTEGER :: stepMax      ! idata(3): Maximum permitted steps
        REAL(8) :: minH          ! rdata(1): Integration step size lower bound
        REAL(8) :: maxH          ! rdata(2): Integration step size upper bound
        REAL(8) :: startH        ! rdata(3): Starting integration step size
        REAL(8) :: minFact       ! rdata(4): Lower bound on step decrease factor
        REAL(8) :: maxFact       ! rdata(5): Upper bound on step increase factor
        REAL(8) :: rejectFact    ! rdata(6): Step decrease factor after step rejection
        REAL(8) :: safeFact      ! rdata(7): Safety factor in computation of new step size

        ! .................... Local variables ....................

        ! Stage solution vectors
        REAL(8), ALLOCATABLE :: K(:,:)
        ! Variable concentrations after successful solve
        REAL(8), ALLOCATABLE :: newVar(:)
        ! Error in newVar
        REAL(8), ALLOCATABLE :: errVar(:)
        ! Function at time tstart
        REAL(8), ALLOCATABLE :: fcn0(:)
        ! Function at time T
        REAL(8), ALLOCATABLE :: fcn(:)
        ! Partial derivative of the function w.r.t T
        REAL(8), ALLOCATABLE :: dFdT(:)
        ! Reaction rates at time T
        REAL(8), ALLOCATABLE :: rct(:)
        ! Jacobian at time tstart
        REAL(8), ALLOCATABLE :: jac0(:)
        ! Stage computation left hand side matrix
        REAL(8), ALLOCATABLE :: slhs(:)

        INTEGER :: dir      ! +1 if time advances positively, -1 otherwise
        REAL(8) :: T         ! Model time
        REAL(8) :: deltaT    ! Model time delta
        REAL(8) :: H         ! Timestep
        REAL(8) :: newH      ! Updated timestep
        REAL(8) :: errNorm   ! The scaled norm of the error vector

        REAL(8) :: CHI

        INTEGER :: singRow  ! diag(singRow) == 0
        INTEGER :: decomps  ! Attempted decompositions counter

        INTEGER rejectH     ! Number of consecutive time step rejections
        INTEGER i, j        ! Iterators

        INTEGER :: nFun     ! Number of function evaluations
        INTEGER :: nJac     ! Number of Jacobian evaluations
        INTEGER :: nStp     ! Number of solver steps
        INTEGER :: nAcc     ! Number of accepted steps
        INTEGER :: nRej     ! Number of rejected steps
        INTEGER :: nDec     ! Number of matrix decompositions
        INTEGER :: nSol     ! Number of Ax=b solves
        INTEGER :: nSng     ! Number of singular decomposition results

        ! ................ Initialize the Rosenbrock method ................

        name = "Unknown"
        SELECT CASE (idata(4))
        CASE (1)
            CALL InitRos2
        CASE (2)
            CALL InitRos3
        CASE (0,3)
            CALL InitRos4
        CASE (4)
            CALL InitRodas3
        CASE (5)
            CALL InitRodas4
        CASE DEFAULT
            print *,"Kppa: Unknown method:", idata(4)
            idata(20) = -3
            RETURN
        END SELECT

        ! ................... Initialize local variables ...................

        ! Initialize statistics
        nFun = 0
        nJac = 0
        nStp = 0
        nAcc = 0
        nRej = 0
        nDec = 0
        nSol = 0
        nSng = 0

        ! Initialize step rejection counter
        rejectH = 0
        
        ! Initialize error norm
        errNorm = 0

        ! Initialize time
        IF (tend >= tstart) THEN
            dir = +1
        ELSE
            dir = -1
        END IF
        spanT = dir * (tend - tstart)
        T = tstart
        H = spanT

        ! Determine if F depends on time
        IF (idata(1) /= 0) THEN
            autonomous = .TRUE.
        ELSE
            autonomous = .FALSE.
        END IF

        ! Scalar tolerances limits the tolerance vectors to the first element.
        IF (idata(2) /= 0) THEN
            nTol = 1
        ELSE
            nTol = NVAR
        END IF

        ! Maximum number of steps before the method aborts
        IF (idata(3) /= 0) THEN
            stepMax = idata(3)
            IF (stepMax < 0) THEN
                CALL RosAbort(-3)
                RETURN
            END IF
        ELSE
            stepMax = 100000
        END IF

        ! Check tolerance vectors
        IF (idata(5) /= 0) THEN
            DO i=1,nTol
                IF (abstol(i) <= ZERO) THEN
                    CALL RosAbort(-4)
                    RETURN
                END IF
                IF (reltol(i) <= (10.0 * REAL_EPSILON) .OR. reltol(i) >= ONE) THEN
                    CALL RosAbort(-5)
                    RETURN
                END IF
            END DO
        END IF

        ! Lower bound on the step size: (positive value)
        minH = rdata(1)
        IF (minH < ZERO) THEN
            CALL RosAbort(-6)
            RETURN
        END IF

        ! Upper bound on the step size: (positive value)
        IF (rdata(2) /= 0) THEN
            maxH = MIN(ABS(rdata(2)), spanT)
            IF (maxH < ZERO) THEN
                CALL RosAbort(-7)
                RETURN
            END IF
        ELSE
            maxH = spanT
        END IF

        !  Starting step size: (positive value)
        IF (rdata(3) /= 0) THEN
            startH = MIN(ABS(rdata(3)), spanT)
            IF (startH < ZERO) THEN
                CALL RosAbort(-8)
                RETURN
            END IF
        ELSE
            startH = MAX(minH, MIN_DELT)
        END IF

        ! Lower bound on step decrease factor
        IF (rdata(4) /= 0) THEN
            minFact = rdata(4)
            IF (minFact < ZERO) THEN
                CALL RosAbort(-9)
                RETURN
            END IF
        ELSE
            minFact = 0.2
        END IF

        ! Upper bound on step increase factor
        IF (rdata(5) /= 0) THEN
            maxFact = rdata(5)
            IF (maxFact < minFact) THEN
                CALL RosAbort(-10)
                RETURN
            END IF
        ELSE
            maxFact = 6.0
        END IF

        ! Step decrease factor after step rejection
        IF (rdata(6) /= 0) THEN
            rejectFact = rdata(6)
            IF (rejectFact < ZERO) THEN
                CALL RosAbort(-11)
                RETURN
            END IF
        ELSE
            rejectFact = 0.1
        END IF

        ! Safety factor in the computation of new step size
        IF (rdata(7) /= 0) THEN
            safeFact = rdata(7)
            IF (safeFact < ZERO) THEN
                CALL RosAbort(-12)
                RETURN
            END IF
        ELSE
            safeFact = 0.9
        END IF

        ! Adjust timestep according to user-specified limits
        H = MIN(startH, maxH)
        IF (ABS(H) < 10 * REAL_EPSILON) THEN
            H = MIN_DELT
        END IF

        ! Allocate memory
        ALLOCATE(K(NVAR,nStage))
        ALLOCATE(newVar(NVAR))
        ALLOCATE(errVar(NVAR))
        ALLOCATE(fcn0(NVAR))
        ALLOCATE(fcn(NVAR))
        ALLOCATE(dFdT(NVAR))
        ALLOCATE(rct(NREACT))
        ALLOCATE(jac0(JAC_LU_NZ))
        ALLOCATE(slhs(JAC_LU_NZ))

        ! ............................ Integrate ............................

        ! Time integration loop
        DO WHILE (ABS(tend - T) > REAL_EPSILON)

           CHI = CHIBE(T/60.0d+00,48.0d+00,2.0d+00,TAG)
           
            ! Check step count
            IF (nStp > stepMax) THEN
                CALL RosAbort(-13)
                RETURN
            END IF

            ! Check timestep size
            IF (H < REAL_EPSILON) THEN
                CALL RosAbort(-14)
                RETURN
            END IF

            ! Update timestep
            H = MIN(H,ABS(tend-T))

            ! Compute reaction rates at the current time
            CALL Rates(T, idx, TEMP, rct, CHI)

            ! Compute the function at the current time
            CALL Fun(var, fix, rct, fcn0)
            nFun = nFun + 1

            ! Compute the Jacobian at the current time
            CALL Jac(var, fix, rct, jac0)
            nJac = nJac + 1

            ! Compute the function derivative with respect to time
            IF (.NOT. autonomous) THEN
                deltaT = SQRT(REAL_EPSILON) * MAX(MIN_DELT, ABS(T))
                CALL Rates(T+deltaT, idx, TEMP, rct, CHI)
                CALL Fun(var, fix, rct, fcn)
                nFun = nFun + 1
                dFdT = (fcn - fcn0) / deltaT
            END IF

            ! Repeat step calculation until step accepted
steploop:   DO
                singRow = 0
                decomps = 1

                ! Prepare the LHS matrix for stage calculations
                CALL RosenStageLHS(1.0/(dir*H*gam(1)), jac0, slhs)

                ! LU decompose stage LHS matrix
                singRow = Decomp(slhs)
                nDec = nDec + 1

                ! If the decomposition failed, half the timestep and try again
                DO WHILE (singRow /= 0)
                    WRITE(*,*) "Kppa:",name,": LU decomposition singular on row",(singRow-1)
                    nSng = nSng + 1

                    ! Reduce step size
                    H = H * HALF

                    ! Abort after eight failed decompositions
                    IF (decomps > 8 .OR. H < REAL_EPSILON) THEN
                        CALL RosAbort(-15)
                        RETURN
                    END IF

                    ! Build new stage LHS with reduced time step
                    CALL RosenStageLHS(1.0/(dir*H*gam(1)), jac0, slhs)

                    ! LU decompose stage LHS matrix
                    singRow = Decomp(slhs)
                    nDec = nDec + 1
                    decomps = decomps + 1
                END DO ! WHILE (singRow /= 0)

                ! Compute stage 0 using the previously-computed function
                fcn = fcn0
                K(:,1) = fcn0
                IF ((.NOT. autonomous) .AND. (gam(1) /= 0)) THEN
                    K(:,1) = K(:,1) +  dir*H*gam(1) * dFdT
                END IF

                ! Solve stage 1
                CALL Solve(slhs, K(:,1))
                nSol = nSol + 1

                ! Compute the remaining stages
                DO i=2,nStage
                    IF (F(i)) THEN
                        ! Apply coefficient matrix A
                        newVar = var
                        DO j=1,i-1
                            newVar = newVar + A((i-1)*(i-2)/2+j) * K(:,j)
                        END DO
                        ! Update reaction rates, if necessary
                        IF (.NOT. autonomous) THEN
                            CALL Rates(T+alpha(i)*dir*H, idx, TEMP, rct, CHI)
                        END IF
                        ! Evaluate the function
                        CALL Fun(newVar, fix, rct, fcn)
                        nFun = nFun + 1
                    END IF

                    ! Apply coefficient matrix C
                    K(:,i) = fcn
                    DO j=1,i-1
                        K(:,i) = K(:,i) + C((i-1)*(i-2)/2+j)/(dir*H) * K(:,j)
                    END DO

                    IF ((.NOT. autonomous) .AND. (gam(i) /= 0)) THEN
                        K(:,i) = K(:,i) + dir*H*gam(i) * dFdT
                    END IF

                    ! Solve stage i
                    CALL Solve(slhs, K(:,i))
                    nSol = nSol + 1
                END DO ! i=2,nStage

                ! Compute the new solution
                newVar = var
                DO j=1,nStage
                    newVar = newVar + M(j) * K(:,j)
                END DO

                ! Estimate error
                errVar = ZERO
                DO j=1,nStage
                    errVar = errVar + E(j) * K(:,j)
                END DO

                ! Calculate scaled norm of the error vector
                errNorm = RosenErrNorm(var, newVar, errVar, nTol, abstol, reltol)
                rdata(13) = errNorm
                IF (HUGE(errNorm) < ABS(errNorm) .OR. ISNAN(errNorm)) THEN
                    CALL RosAbort(-16)
                    RETURN
                END IF

                ! Calculate a new step size: minFact <= newH/H <= maxFact
                newH = H * MIN(maxFact,MAX(minFact,safeFact/(errNorm**invLoEst)))
                nStp = nStp + 1

                ! Decide to accept or reject step
                IF (errNorm <= ONE .OR. H <= minH) THEN
                    ! Step accepted
                    nAcc = nAcc + 1
                    var = newVar
                    T = T + dir*H
                    ! Adjust step size
                    newH = MAX(minH,MIN(newH,maxH))
                    IF (rejectH /= 0) THEN
                        newH = MIN(newH,H)
                    END IF
                    rejectH = 0
                    H = newH
                    ! Return to time loop
                    EXIT steploop
                ELSE
                    ! Step rejected
                    nRej = nRej + 1
                    IF (rejectH > 1) THEN
                        newH = H * rejectFact
                    END IF
                    rejectH = rejectH + 1
                    H = newH
                END IF

            END DO steploop

        END DO ! WHILE (ABS(tend - T) > REAL_EPSILON)

        ! ...................... Exit integrator ......................

        ! Set exit status
        idata(20) = 0

        ! Deallocate memory
        DEALLOCATE(K)
        DEALLOCATE(newVar)
        DEALLOCATE(errVar)
        DEALLOCATE(fcn0)
        DEALLOCATE(fcn)
        DEALLOCATE(rct)
        DEALLOCATE(dFdT)
        DEALLOCATE(jac0)
        DEALLOCATE(slhs)

        ! Collect statistics
        idata(11) = nFun
        idata(12) = nJac
        idata(13) = nStp
        idata(14) = nAcc
        idata(15) = nRej
        idata(16) = nDec
        idata(17) = nSol
        idata(18) = nSng
        ! Record exit time and last step size
        rdata(11) = T
        rdata(12) = H
        rdata(13) = errNorm

        RETURN

        ! .................... Internal subroutines ....................
        CONTAINS

        !-----------------------------------------------------------------------
        ! Records an error code in idata and prints an error message
        !
        ! @param[in] code  Error code
        !-----------------------------------------------------------------------
        SUBROUTINE RosAbort(code)
            IMPLICIT NONE

            INTEGER, INTENT(IN) :: code

            WRITE(*,*) "Kppa:",name,"T=",T,"H=",H
            idata(20) = code

            SELECT CASE(code)
            CASE (-3)
                WRITE(*,*) "Invalid maximum steps"
            CASE (-4)
                WRITE(*,*) "Unreasonable absolute tolerance"
            CASE (-5)
                WRITE(*,*) "Unreasonable relative tolerance"
            CASE (-6)
                WRITE(*,*) "Invalid step size lower bound"
            CASE (-7)
                WRITE(*,*) "Invalid step size upper bound"
            CASE (-8)
                WRITE(*,*) "Invalid starting step size"
            CASE (-9)
                WRITE(*,*) "Invalid lower bound on step decrease factor"
            CASE (-10)
                WRITE(*,*) "Invalid upper bound on step increase factor"
            CASE (-11)
                WRITE(*,*) "Invalid step decrease factor for rejected step"
            CASE (-12)
                WRITE(*,*) "Invalid new step safety factor"
            CASE (-13)
                WRITE(*,*) "Too many integration steps"
            CASE (-14)
                WRITE(*,*) "Step size too small (T + H/10 = T) or H < eps"
            CASE (-15)
                WRITE(*,*) "Matrix is repeatedly singular"
            CASE (-16)
                WRITE(*,*) "Error norm is Inf or NaN"
            END SELECT

        END SUBROUTINE RosAbort

        !-----------------------------------------------------------------------
        ! A two-stage L-stable method of order 2
        !
        ! E. Harier and G. Wanner, "Solving Ordenary Differential Equations II: 
        ! stiff and differential-algebraic problems." Computational Mathematics,
        ! Springer-Verlag (1996)
        !-----------------------------------------------------------------------
        SUBROUTINE InitRos2()
            IMPLICIT NONE

            name = "Ros2"

            nStage = 2

            invLoEst = 0.5 ! 1 / 2

            M(1) = 0.8786796564403575
            M(2) = 0.2928932188134525

            E(1) = 0.2928932188134525
            E(2) = 0.2928932188134525

            A(1) = 0.585786437626905

            C(1) = -1.17157287525381

            alpha(1) = 0.0
            alpha(2) = 1.0

            gam(1) =  1.7071067811865475
            gam(2) = -1.7071067811865475

            F(1) = .TRUE.
            F(2) = .TRUE.
        END SUBROUTINE InitRos2

        !-----------------------------------------------------------------------
        ! A three-stage L-stable method of order 3
        !
        ! E. Harier and G. Wanner, "Solving Ordenary Differential Equations II: 
        ! stiff and differential-algebraic problems." Computational Mathematics,
        ! Springer-Verlag (1996)
        !-----------------------------------------------------------------------
        SUBROUTINE InitRos3()
            IMPLICIT NONE

            name = "Ros3"

            nStage = 3

            ! Inverse estimation of local order: 1/3
            invLoEst = 0.3333333333333333

            ! Coefficients for new step solution
            M(1) = 1.0
            M(2) = 6.1697947043828245592553615689730
            M(3) = -0.4277225654321857332623837380651

            ! Coefficients for error estimation
            E(1) = 0.5
            E(2) = -2.9079558716805469821718236208017
            E(3) = 0.2235406989781156962736090927619

            ! Lower triangular coefficient matrix A
            A(1) = 1.0
            A(2) = 1.0
            A(3) = 0.0

            ! Lower triangular coefficient matrix C
            C(1) = -1.0156171083877702091975600115545
            C(2) = 4.0759956452537699824805835358067
            C(3) = 9.2076794298330791242156818474003

            ! Two function evaluations */
            F(1) = .TRUE.
            F(2) = .TRUE.
            F(3) = .FALSE.

            ! Y_stage_i ~ Y( T + H*Alpha_i )
            alpha(1) = 0.0
            alpha(2) = 0.43586652150845899941601945119356
            alpha(3) = 0.43586652150845899941601945119356

            ! Gamma_i = \sum_j  gamma_{i,j}
            gam(1) = 0.43586652150845899941601945119356
            gam(2) = 0.24291996454816804366592249683314
            gam(3) = 2.1851380027664058511513169485832
        END SUBROUTINE  InitRos3

        !-----------------------------------------------------------------------
        ! A four-stage L-stable method of order 4
        !
        ! E. Harier and G. Wanner, "Solving Ordenary Differential Equations II: 
        ! stiff and differential-algebraic problems." Computational Mathematics,
        ! Springer-Verlag (1996)
        !-----------------------------------------------------------------------
        SUBROUTINE InitRos4()
            IMPLICIT NONE

            name = "Ros4"

            ! Number of stages
            nStage = 4

            ! Inverse estimation of local order: 1/4
            invLoEst = 0.25

            ! Coefficients for new step solution
            M(1) = 2.255570073418735
            M(2) = 0.2870493262186792
            M(3) = 0.4353179431840180
            M(4) = 1.093502252409163

            ! Coefficients for error estimation
            E(1) = -0.2815431932141155
            E(2) = -0.07276199124938920
            E(3) = -0.1082196201495311
            E(4) = -1.093502252409163

            ! Lower triangular coefficient matrix A
            A(1) = 2.0
            A(2) = 1.867943637803922
            A(3) = 0.2344449711399156
            A(4) = 1.867943637803922
            A(5) = 0.2344449711399156
            A(6) = 0.0

            ! Lower triangular coefficient matrix C
            C(1) = -7.137615036412310
            C(2) =  2.580708087951457
            C(3) =  0.6515950076447975
            C(4) = -2.137148994382534
            C(5) = -0.3214669691237626
            C(6) = -0.6949742501781779

            ! Three function evaluations
            F(1) = .TRUE.
            F(2) = .TRUE.
            F(3) = .TRUE.
            F(4) = .FALSE.

            ! Y_stage_i ~ Y( T + H*Alpha_i )
            alpha(1) = 0.0
            alpha(2) = 1.145640000000000
            alpha(3) = 0.6552168638155900
            alpha(4) = 0.6552168638155900

            ! Gamma_i = \sum_j  gamma_{i,j}
            gam(1) = 0.5728200000000000
            gam(2) = -1.769193891319233
            gam(3) = 0.7592633437920482
            gam(4) = -0.1049021087100450
        END SUBROUTINE InitRos4

        !-----------------------------------------------------------------------
        ! A four-stage stiffly-stable method of order 4
        !
        ! E. Harier and G. Wanner, "Solving Ordenary Differential Equations II: 
        ! stiff and differential-algebraic problems." Computational Mathematics,
        ! Springer-Verlag (1996)
        !-----------------------------------------------------------------------
        SUBROUTINE InitRodas3()
            IMPLICIT NONE

            name = "Rodas3"

            ! Number of stages
            nStage = 4

            ! Inverse estimation of local order: 1/3
            invLoEst = 0.3333333333333333

            ! Coefficients for new step solution
            M(1) = 2.0
            M(2) = 0.0
            M(3) = 1.0
            M(4) = 1.0

            ! Coefficients for error estimation
            E(1) = 0.0
            E(2) = 0.0
            E(3) = 0.0
            E(4) = 1.0

            ! Lower triangular coefficient matrix A
            A(1) = 0.0
            A(2) = 2.0
            A(3) = 0.0
            A(4) = 2.0
            A(5) = 0.0
            A(6) = 1.0

            ! Lower triangular coefficient matrix C
            C(1) = 4.0
            C(2) = 1.0
            C(3) = -1.0
            C(4) = 1.0
            C(5) = -1.0
            C(6) = -2.66666666666667

            ! Three function evaluations
            F(1) = .TRUE.
            F(2) = .FALSE.
            F(3) = .TRUE.
            F(4) = .TRUE.

            ! Y_stage_i ~ Y( T + H*Alpha_i )
            alpha(1) = 0.0
            alpha(2) = 0.0
            alpha(3) = 1.0
            alpha(4) = 1.0

            ! Gamma_i = \sum_j  gamma_{i,j}
            gam(1) = 0.5
            gam(2) = 1.5
            gam(3) = 0.0
            gam(4) = 0.0
        END SUBROUTINE InitRodas3

        !-----------------------------------------------------------------------
        ! A six-stage stiffly-stable method of order 4
        !
        ! E. Harier and G. Wanner, "Solving Ordenary Differential Equations II: 
        ! stiff and differential-algebraic problems." Computational Mathematics,
        ! Springer-Verlag (1996)
        !-----------------------------------------------------------------------
        SUBROUTINE InitRodas4()
            IMPLICIT NONE

            name = "Rodas4"

            ! Number of stages
            nStage = 6

            ! Inverse estimation of local order: 1/4
            invLoEst = 0.25

            ! Coefficients for new step solution
            M(1) = 1.544000000000000
            M(2) = 6.019134481288629
            M(3) = 12.53708332932087
            M(4) = -0.6878860361058950
            M(5) = 1.0
            M(6) = 1.0

            ! Coefficients for error estimation
            E(1) = 0.0
            E(2) = 0.0
            E(3) = 0.0
            E(4) = 0.0
            E(5) = 0.0
            E(6) = 1.0

            ! Lower triangular coefficient matrix A
            A(1) = 1.544000000000000
            A(2) = 0.9466785280815826
            A(3) = 0.2557011698983284
            A(4) = 3.314825187068521
            A(5) = 2.896124015972201
            A(6) = 0.9986419139977817
            A(7) = 1.221224509226641
            A(8) = 6.019134481288629
            A(9) = 12.53708332932087
            A(10) = -0.6878860361058950
            A(11) = 1.221224509226641
            A(12) = 6.019134481288629
            A(13) = 12.53708332932087
            A(14) = -0.6878860361058950
            A(15) = 1.0

            ! Lower triangular coefficient matrix C
            C(1) = -5.668800000000000
            C(2) = -2.430093356833875
            C(3) = -0.2063599157091915
            C(4) = -0.1073529058151375
            C(5) = -9.594562251023355
            C(6) = -20.47028614809616
            C(7) = 7.496443313967647
            C(8) = -10.24680431464352
            C(9) = -33.99990352819905
            C(10) = 11.70890893206160
            C(11) = 8.083246795921522
            C(12) = -7.981132988064893
            C(13) = -31.52159432874371
            C(14) = 16.31930543123136
            C(15) = -6.058818238834054

            ! Six function evaluations
            F(1) = .TRUE.
            F(2) = .TRUE.
            F(3) = .TRUE.
            F(4) = .TRUE.
            F(5) = .TRUE.
            F(6) = .TRUE.

            ! Y_stage_i ~ Y( T + H*Alpha_i )
            alpha(1) = 0.000
            alpha(2) = 0.386
            alpha(3) = 0.210
            alpha(4) = 0.630
            alpha(5) = 1.000
            alpha(6) = 1.000

            ! Gamma_i = \sum_j  gamma_{i,j}
            gam(1) = 0.2500000000000000
            gam(2) = -0.1043000000000000
            gam(3) = 0.1035000000000000
            gam(4) = -0.03620000000000023
            gam(5) = 0.0
            gam(6) = 0.0
        END SUBROUTINE InitRodas4

      END SUBROUTINE Integrate



    !---------------------------------------------------------------------------
    ! Calculates the left hand side matrix for Rosenbrock stage calculation
    !
    ! @param[in]  diag   Value to add to diagonal elements
    ! @param[in]  jac    The Jacobian
    ! @param[out] slhs   Left hand side matrix for Rosenbrock stage calculation
    !---------------------------------------------------------------------------
    SUBROUTINE RosenStageLHS(diag, jac, slhs)
        IMPLICIT NONE

        REAL(8), INTENT(IN) :: diag
        REAL(8), INTENT(IN) :: jac(JAC_LU_NZ)
        REAL(8), INTENT(OUT) :: slhs(JAC_LU_NZ)

        slhs = -jac
        ! Adjust diagonal elements 
        slhs(1) = diag + slhs(1)
        slhs(2) = diag + slhs(2)
        slhs(3) = diag + slhs(3)
        slhs(6) = diag + slhs(6)
        slhs(14) = diag + slhs(14)
        slhs(31) = diag + slhs(31)
        slhs(43) = diag + slhs(43)
        slhs(45) = diag + slhs(45)
        slhs(47) = diag + slhs(47)
        slhs(49) = diag + slhs(49)
        slhs(53) = diag + slhs(53)
        slhs(55) = diag + slhs(55)
        slhs(58) = diag + slhs(58)
        slhs(61) = diag + slhs(61)
        slhs(64) = diag + slhs(64)
        slhs(67) = diag + slhs(67)
        slhs(71) = diag + slhs(71)
        slhs(74) = diag + slhs(74)
        slhs(78) = diag + slhs(78)

        slhs(81) = diag + slhs(81)
        slhs(83) = diag + slhs(83)
        slhs(85) = diag + slhs(85)
        slhs(87) = diag + slhs(87)
        slhs(93) = diag + slhs(93)
        slhs(105) = diag + slhs(105)
        slhs(117) = diag + slhs(117)
        slhs(126) = diag + slhs(126)
        slhs(130) = diag + slhs(130)
        slhs(134) = diag + slhs(134)
        slhs(138) = diag + slhs(138)
        slhs(144) = diag + slhs(144)
        slhs(148) = diag + slhs(148)
        slhs(154) = diag + slhs(154)
        slhs(158) = diag + slhs(158)
        slhs(164) = diag + slhs(164)
        slhs(170) = diag + slhs(170)
        slhs(178) = diag + slhs(178)
        slhs(182) = diag + slhs(182)
        slhs(188) = diag + slhs(188)
        slhs(196) = diag + slhs(196)
        slhs(204) = diag + slhs(204)
        slhs(212) = diag + slhs(212)
        slhs(231) = diag + slhs(231)
        slhs(236) = diag + slhs(236)
        slhs(242) = diag + slhs(242)
        slhs(252) = diag + slhs(252)
        slhs(256) = diag + slhs(256)
        slhs(265) = diag + slhs(265)
        slhs(273) = diag + slhs(273)
        slhs(283) = diag + slhs(283)
        slhs(293) = diag + slhs(293)
        slhs(302) = diag + slhs(302)
        slhs(309) = diag + slhs(309)
        slhs(315) = diag + slhs(315)
        slhs(327) = diag + slhs(327)
        slhs(340) = diag + slhs(340)
        slhs(349) = diag + slhs(349)
        slhs(361) = diag + slhs(361)
        slhs(371) = diag + slhs(371)
        slhs(386) = diag + slhs(386)
        slhs(406) = diag + slhs(406)
        slhs(420) = diag + slhs(420)
        slhs(436) = diag + slhs(436)
        slhs(470) = diag + slhs(470)
        slhs(490) = diag + slhs(490)
        slhs(518) = diag + slhs(518)
        slhs(536) = diag + slhs(536)
        slhs(545) = diag + slhs(545)
        slhs(554) = diag + slhs(554)
        slhs(569) = diag + slhs(569)
        slhs(594) = diag + slhs(594)
        slhs(613) = diag + slhs(613)
        slhs(666) = diag + slhs(666)
        slhs(685) = diag + slhs(685)
        slhs(731) = diag + slhs(731)
        slhs(762) = diag + slhs(762)
        slhs(796) = diag + slhs(796)
        slhs(822) = diag + slhs(822)
        slhs(839) = diag + slhs(839)
        slhs(871) = diag + slhs(871)
        slhs(907) = diag + slhs(907)
        slhs(968) = diag + slhs(968)

    END SUBROUTINE RosenStageLHS


    !---------------------------------------------------------------------------
    ! Returns the "scaled norm" of the error vector
    !---------------------------------------------------------------------------
    REAL(8) FUNCTION RosenErrNorm(var, newY, errY, nTol, abstol, reltol)
        IMPLICIT NONE

        REAL(8), INTENT(IN) :: var(NVAR)
        REAL(8), INTENT(IN) :: newY(NVAR)
        REAL(8), INTENT(IN) :: errY(NVAR)
        INTEGER, INTENT(IN) :: nTol
        REAL(8), INTENT(IN) :: abstol(NVAR)
        REAL(8), INTENT(IN) :: reltol(NVAR)

        REAL*8 :: err
        REAL(8) :: scl
        REAL(8) :: maxY
        INTEGER :: i

        err = ZERO
        IF (nTol > 1) THEN
            DO i=1,NVAR
                maxY = MAX(ABS(var(i)),ABS(newY(i)))
                scl = abstol(i) + reltol(i) * maxY
                err = err + (errY(i) * errY(i)) / (scl * scl)
            END DO
        ELSE
            DO i=1,NVAR
                maxY = MAX(ABS(var(i)),ABS(newY(i)))
                scl = abstol(1) + reltol(1) * maxY
                err = err + (errY(i) * errY(i)) / (scl * scl)
            END DO
        END IF

        RosenErrNorm = REAL(SQRT(err/DBLE(NVAR)))
    END FUNCTION RosenErrNorm


END MODULE box_model_rosenbrock
!---------------------- END box_model_rosenbrock.f90 END ---------------------
