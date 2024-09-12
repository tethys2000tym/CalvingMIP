!/*****************************************************************************
! *
! *  Elmer, A Finite Element Software for Multiphysical Problems
! *
! *  Copyright 1st April 1995 - , CSC - IT Center for Science Ltd., Finland
! * 
! *  This program is free software; you can redistribute it and/or
! *  modify it under the terms of the GNU General Public License
! *  as published by the Free Software Foundation; either version 2
! *  of the License, or (at your option) any later version.
! * 
! *  This program is distributed in the hope that it will be useful,
! *  but WITHOUT ANY WARRANTY; without even the implied warranty of
! *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! *  GNU General Public License for more details.
! *
! *  You should have received a copy of the GNU General Public License
! *  along with this program (in file fem/GPL-2); if not, write to the 
! *  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
! *  Boston, MA 02110-1301, USA.
! *
! *****************************************************************************/
!
!/******************************************************************************
! *
! *  Authors: Juha Ruokolainen, Peter Råback
! *  Email:   Peter.Raback@csc.fi
! *  Web:     http://www.csc.fi/elmer
! *  Address: CSC - IT Center for Science Ltd.
! *           Keilaranta 14
! *           02101 Espoo, Finland 
! *
! *  Original Date: 16.11.2005
! *
! *****************************************************************************/
!/******************************************************************************
! *  EXPORT SOLVER
! *  Modified and adapted to solve div(S)=0 by: Cruz Garcia Molina
! *  Email:   Cruz.Garcia-molina@univ-grenoble-alpes.fr
! *  Address: IGE - OSUG B
! *           460 rue de la Piscine 
! *           Domaine universitaire 
! *           38400 St Martin d'Hères, France
! *
! *  Original Date: 01.02.2021
! *
! *****************************************************************************/
!>  Solve the poisson equation. This solver aims to export the values of the
!>  scalars in the ACTIVE domain to the PASSIVE domain. The export is done along
!>  the perpendicular direction to the front. The front is given by the levelset (LS)
!>  and its normal is given by [grad(LS)/|grad(LS)|]|_(LS=0). The Variable to be
!>  exported to the PASSIVE domain is named AUXILIAR VARIABLE i, and the exported
!>   value is called VARIABLE i.
!------------------------------------------------------------------------------
SUBROUTINE LevelSetExportSolver( Model,Solver )
!------------------------------------------------------------------------------
  USE Types
  USE DefUtils
  USE SolverUtils
  USE MaterialModels
  USE Integration
  
  IMPLICIT NONE
!------------------------------------------------------------------------------ 
  TYPE(Model_t), TARGET :: Model
  TYPE(Solver_t) :: Solver
!------------------------------------------------------------------------------
!    Local variables
!------------------------------------------------------------------------------
  INTEGER :: n,t,istat,VarInd,VarIndMax
  
  TYPE(Matrix_t),POINTER  :: StiffMatrix
  TYPE(Nodes_t)   :: ElementNodes
  TYPE(Element_t),POINTER :: CurrentElement
  
  REAL(KIND=dp) :: Norm,RelativeChange
  LOGICAL :: AllocationsDone = .FALSE.,Parallel,GotIt
  INTEGER, POINTER :: NodeIndexes(:), SurfPerm(:)
  REAL(KIND=dp), POINTER :: ForceVector(:)
  REAL(KIND=dp), ALLOCATABLE :: LocalStiffMatrix(:,:),LocalForce(:)
  INTEGER :: NonlinearIter,dim
  REAL(KIND=dp) :: DsMax, PrevNorm
  REAL(KIND=dp), ALLOCATABLE :: GradLS(:)
  SAVE LocalStiffMatrix,  GradLS, &
       LocalForce, ElementNodes,AllocationsDone
  REAL(KIND=dp) :: at,totat,st,totst
  CHARACTER(LEN=MAX_NAME_LEN) :: varname,str,SolverName
  CHARACTER(LEN=5) :: indexauxiliar
  TYPE(ValueList_t), POINTER :: listsol
  TYPE(Variable_t),POINTER :: VarAux,LSexterno,VarEx
  SolverName = 'ExportSolver'
!-------------------------------------------------------------------------
!    Get variables needed for solution
!-------------------------------------------------------------------------
  CALL Info( SolverName,'-------------------------------------', Level=4 )
  CALL Info( SolverName, 'Solving for the advection equation ', Level=4 )
  CALL Info( SolverName,'-------------------------------------', Level=4 )

  IF ( .NOT. ASSOCIATED( Solver % Matrix ) ) RETURN
  
  dim = CoordinateSystemDimension()
  
  Parallel = (ParEnv % PEs > 1)  

!-------------------------------------------------------------------------
!    Allocate some permanent storage, this is done first time only
!-------------------------------------------------------------------------
  IF ( .NOT. AllocationsDone ) THEN
     N = Solver % Mesh % MaxElementNodes
     
     ALLOCATE( ElementNodes % x( N ),   &
          ElementNodes % y( N ),   &
          ElementNodes % z( N ),   &
          LocalForce( 2*N ),       &
          LocalStiffMatrix( 2*N,2*N ), &
          GradLS( N ), &
          STAT=istat)
     
     IF ( istat /= 0 ) THEN
        CALL Fatal( SolverName, 'Memory allocation error.' )
     END IF
     
     AllocationsDone = .TRUE.
  END IF
!-------------------------------------------------------------------------
!    Do some additional initialization, and go for it
!-------------------------------------------------------------------------
  listsol => GetSolverParams()
  
  NonlinearIter = ListGetInteger( Solver % Values, &
       'Nonlinear System Max Iterations',GotIt )
  IF ( .NOT.GotIt ) NonlinearIter = 1
  
!-------------------------------------------------------------------------
!    Setting the counter for the variables to be exported
!-------------------------------------------------------------------------
  VarIndMax=0
  DO WHILE( .TRUE. )
     VarIndMax = VarIndMax + 1
     str = ComponentName( 'Auxiliar', VarIndMax )
     varname = ListGetString( listsol, str, GotIt )
     IF(.NOT. GotIt) EXIT
  end do
  VarIndMax=VarIndMax-1
  if (VarIndMax==0) VarIndMax=1

  
!------------------------------------------------------------------------------

  totat = 0.0d0
  totst = 0.0d0
  
  CALL Info( SolverName,'-------------------------------------', Level=4 )
  CALL Info( SolverName, 'Updating the advected variable', Level=4 )
  CALL Info( SolverName,'-------------------------------------', Level=4 )
  
  at = CPUTime()

  ! Reading the levelset variable  
  LSexterno=>VariableGet(Model%Mesh%Variables,'LS',UnFoundFatal=.TRUE.)
 
  do VarInd=1,VarIndMax ! Loop over variables goes from 1 to the biggest number of 
  ! Auxiliar Variable which should coincide with the maximum number of Variable
     
     write(indexauxiliar, "(I2)" ) VarInd ! Reading the Auxiliar name variables,
     ! These are the variables that will be exported to the passive domain.
     if (VarInd>1) then
        str='Variable'//indexauxiliar
     else 
        str='Variable'
     end if
     varname = GetString(listsol,str,GotIt)
     IF ( .NOT.GotIt ) THEN
        WRITE(Message, '(a,a,a)') "The ",trim(str)," was not found."
        CALL Fatal(varname, Message)
     END IF
     VarEx => VariableGet(Model%Mesh%Variables, varname, UnFoundFatal=.True.)
     Solver % Variable => VarEx     ! Designs the "Variable i" as the solver's variable.
     IF ( SIZE(Solver % Variable % Values) == 0 ) RETURN
     SurfPerm => Solver % Variable % Perm

 !-------------------------------------------------------------------------
 !     Getting the auxiliar variable associated to the Variable i 
 !-------------------------------------------------------------------------
     indexauxiliar=trim(indexauxiliar)
     str='Auxiliar'//indexauxiliar
     varname = GetString(listsol,trim(str),GotIt)
     IF ( .NOT.GotIt ) THEN
        WRITE(Message, '(a,a,a)') "The ",trim(str)," was not found."
        CALL Fatal(varname, Message)
     END IF
     VarAux => VariableGet(Model%Mesh%Variables,varname,UnFoundFatal=.TRUE.)
 !-------------------------------------------------------------------------
   
     StiffMatrix => Solver % Matrix
     ForceVector => StiffMatrix % RHS

     if (VarInd==1 .or. Parallel) then
!--------------------------------------------------------------------------
        CALL DefaultInitialize()
!--------------------------------------------------------------------------
     end if
!--------------------------------------------------------------------------
!      Do the assembly for bulk elements
!--------------------------------------------------------------------------
        DO t=1,Solver % Mesh % NumberOfBulkElements ! Loop over the elements
           CurrentElement => Solver % Mesh % Elements(t)
        
!--------------------------------------------------------------------------
!        Set the current element pointer in the model structure to
!        reflect the element being processed
!--------------------------------------------------------------------------
           Model % CurrentElement => CurrentElement
           n = CurrentElement % TYPE % NumberOfNodes
           NodeIndexes => CurrentElement % NodeIndexes
           
!--------------------------------------------------------------------------
!        Get element nodal coordinates
!--------------------------------------------------------------------------
           ElementNodes % x(1:n) = Solver % Mesh % Nodes % x(NodeIndexes)
           ElementNodes % y(1:n) = Solver % Mesh % Nodes % y(NodeIndexes)
           ElementNodes % z(1:n) = Solver % Mesh % Nodes % z(NodeIndexes)
           
!--------------------------------------------------------------------------
!          Getting LevelSet variable
!--------------------------------------------------------------------------
           GradLS(1:n) = LSexterno%Values(LSexterno%Perm(NodeIndexes))
        
!--------------------------------------------------------------------------
           if ( VarInd==1 .or. Parallel ) then
              CALL LocalMatrix( LocalStiffMatrix, LocalForce, &
                 GradLS, CurrentElement, n, ElementNodes )
!--------------------------------------------------------------------------
!      Update global matrix and rhs vector from local matrix & vector
!--------------------------------------------------------------------------
 
              CALL DefaultUpdateEquations( LocalStiffMatrix, LocalForce )
           end if
        END DO     !  Of BulkElements

     if (VarInd==1 .or. Parallel) then
        CALL DefaultFinishBulkAssembly()
!------------------------------------------------------------------------------
!    FinishAssemebly must be called after all other assembly steps, but before
!    Dirichlet boundary settings. Actually no need to call it except for
!    transient simulations.
!------------------------------------------------------------------------------
        CALL DefaultFinishAssembly()
     end if
!------------------------------------------------------------------------------
!    Dirichlet boundary conditions
!--------------------------------------------------------------------------
        CALL DefaultDirichletBCs( CurrentModel % Solver, VarEx )
!--------------------------------------------------------------------------
        CALL Info( SolverName, 'Assembly done', Level=4 )
        at = CPUTime() - at
        totat = totat + at
     
!--------------------------------------------------------------------------
!     Solve the system and check for convergence
!--------------------------------------------------------------------------
        st = CPUTime()
        PrevNorm = Solver % Variable % Norm
        Norm = DefaultSolve()
        RelativeChange = Solver % Variable % NonlinChange

        WRITE( Message, '(a,ES12.3,a,a)') 'Result Norm   : ',Norm,' variable: ',trim(varname)
        CALL Info( SolverName, Message, Level=3 )
        WRITE( Message, "(a,ES12.3,a,a)" ) 'Relative Change : ',RelativeChange,' variable: ',trim(varname)
        CALL Info( SolverName, Message, Level=3 )

        WRITE( Message, * ) 'Result Norm   : ',Norm
        CALL Info( SolverName, Message, Level=1 )
        WRITE( Message, * ) 'Relative Change : ',RelativeChange
        CALL Info( SolverName, Message, Level=1 )
        
!--------------------------------------------------------------------------
     DO t=1,Solver % Mesh % NumberOfBulkElements
        CurrentElement => Solver % Mesh % Elements(t)
        !------------------------------------------------------------------
        !      Set the current element pointer in the model structure to
        !      reflect the element being processed
        !------------------------------------------------------------------
        n = CurrentElement % TYPE % NumberOfNodes
        NodeIndexes => CurrentElement % NodeIndexes
        !------------------------------------------------------------------
        ! Passing the value of the exported value to the variable that is 
        ! used by the other solvers, here called Auxiliar Variable.
        !------------------------------------------------------------------
        IF (.NOT.CheckPassiveElement(CurrentElement)) THEN
           VarAux%Values(VarAux%Perm(NodeIndexes)) = VarEx%Values(VarEx%Perm(NodeIndexes))
        END IF
     END DO
     WRITE(Message,'(a,F8.2)') 'Assembly done in time (s):',totat
     CALL Info( SolverName,Message, Level=4 )
  
     WRITE(Message,'(a,F8.2)') 'Solution done in time (s):',totst
     CALL Info( SolverName,Message, Level=4 )
     DsMax = ABS( Norm - PrevNorm ) 
     WRITE(Message,'(a,a,a,ES12.3)') 'Maximum ',varname,' Change',dsmax
     CALL Info( SolverName,Message, Level=4 )
     WRITE(Message,'(a,a,a,ES12.3)') 'res: ',varname,'Max Change Change'
     CALL ListAddConstReal(Model % Simulation,Message,dsmax)
     
  end do ! end loop over variables
  varname = GetString(listsol,'Variable',GotIt)
  ! I set again the solver's variable to be Variable if I don't the solver
  ! would have a different variable at the end than the "principal one" and
  ! it leads to a problem when calling the solver again.
  Solver % Variable => VariableGet(Model%Mesh%Variables,varname,UnFoundFatal=.TRUE.)

!--------------------------------------------------------------------------

CONTAINS

SUBROUTINE LocalMatrix( StiffMatrix,ForceVector,  &
     GradLS, Element,n,Nodes )
!--------------------------------------------------------------------------

  REAL(KIND=dp), DIMENSION(:)   :: ForceVector, GradLS
  REAL(KIND=dp), DIMENSION(:,:) :: StiffMatrix
  INTEGER :: n
  TYPE(Nodes_t) :: Nodes
  TYPE(Element_t), POINTER :: Element
  
!--------------------------------------------------------------------------
!    Local variables
!--------------------------------------------------------------------------
!
  REAL(KIND=dp) :: ddBasisddx(n,3,3)
  REAL(KIND=dp) :: Basis(2*n)
  REAL(KIND=dp) :: dBasisdx(2*n,3),SQRTElementMetric
  REAL(KIND=dp) :: Force, NormGradLS(3), NormGradLSAbs
  REAL(KIND=dp) :: A,Load
  REAL(KIND=dp) :: VNorm,hK,mK
  REAL(KIND=dp) :: Pe,Tau
  
  INTEGER :: i,c,p,q,t,dim,N_Integ,NBasis
  REAL(KIND=dp) :: s,u,v,w,signo
  TYPE(GaussIntegrationPoints_t), TARGET :: IntegStuff
  REAL(KIND=dp) :: SU(n),SW(n)
  REAL(KIND=dp), DIMENSION(:), POINTER :: U_Integ,V_Integ,W_Integ,S_Integ
  
  LOGICAL :: stat,InvertDomain
  
!--------------------------------------------------------------------------

  dim = CoordinateSystemDimension()
  c = dim + 1
  
  ForceVector = 0.0D0
  StiffMatrix = 0.0D0
  Load = 0.0d0
  NormGradLS = 0.0d0
  
  NBasis = n

!--------------------------------------------------------------------------
!    Integration stuff
!--------------------------------------------------------------------------
  IntegStuff = GaussPoints( element )
  U_Integ => IntegStuff % u
  V_Integ => IntegStuff % v
  W_Integ => IntegStuff % w
  S_Integ => IntegStuff % s
  N_Integ =  IntegStuff % n
  
!--------------------------------------------------------------------------
!    Stabilization parameters: hK, mK (take a look at Franca et.al.)
!    If there is no convection term we don t need stabilization.
!--------------------------------------------------------------------------
  hK = element % hK
  mK = element % StabilizationMK
  signo=1.0d0
  InvertDomain = ListGetLogical( Solver % Values,'Invert Domain',GotIt )
  if (InvertDomain) signo=-1.0d0
!------------------------------------------------------------------------------
!    Now we start integrating
!--------------------------------------------------------------------------
  DO t=1,N_Integ
     
     u = U_Integ(t)
     v = V_Integ(t)
     w = W_Integ(t)
     
!--------------------------------------------------------------------------
!      Basis function values & derivatives at the integration point
!--------------------------------------------------------------------------
     stat = ElementInfo( Element,Nodes,u,v,w,SQRTElementMetric, &
          Basis,dBasisdx,ddBasisddx)
     
     s = SQRTElementMetric * S_Integ(t)
!--------------------------------------------------------------------------
!      Coordinatesystem dependent info
!--------------------------------------------------------------------------
     IF ( Coordinates == AxisSymmetric .OR. &
          Coordinates == CylindricSymmetric ) THEN
        s = s * SUM( Nodes % x(1:n)*Basis(1:n) )
     END IF

!--------------------------------------------------------------------------
         
     DO i=1,dim
        NormGradLS(i) =signo*SUM( dBasisdx(1:n,i) * GradLS(1:n) )
     END DO
     NormGradLSAbs = SQRT( SUM( NormGradLS(1:dim) * NormGradLS(1:dim) ) )
     
     IF ( NormGradLSAbs > 10*AEPS ) THEN
        NormGradLS = NormGradLS / NormGradLSAbs
     END IF
     
!--------------------------------------------------------------------------
!           Stabilization parameter Tau
!--------------------------------------------------------------------------
     VNorm = SQRT( SUM(NormGradLS(1:dim)**2))
     Pe  = 1.0d0
     Tau = 0.0D0
     IF ( VNorm /= 0.0 ) THEN
        Tau = hK * Pe / (2 * VNorm)
     END IF
        
!--------------------------------------------------------------------------
!           Compute residual & stablization vectors
!--------------------------------------------------------------------------
     DO p=1,N
        SU(p) = 0.0d0
        DO i = 1,dim
           SU(p) = SU(p) + dBasisdx(p,i) * NormGradLS(i)
        END DO
        
        SW(p) = 0.0d0
        DO i = 1,dim
           SW(p) = SW(p) + dBasisdx(p,i) * NormGradLS(i)
        END DO
     END DO

!--------------------------------------------------------------------------
!      Loop over basis functions of both unknowns and weights
!--------------------------------------------------------------------------
     DO p=1,NBasis
        DO q=1,NBasis
!--------------------------------------------------------------------------
!           The stiffness matrix
!--------------------------------------------------------------------------
           A = 0.0d0
           
           DO i=1,dim
              A = A + NormGradLS(i) * dBasisdx(q,i) * Basis(p)
           END DO
!------------------------------------------------------------------------------
!           Next we add the stabilization...
!--------------------------------------------------------------------------
           A = A + Tau * SU(q) * SW(p)
           StiffMatrix(p,q) = StiffMatrix(p,q) + s * A
           
        END DO
     END DO


!--------------------------------------------------------------------------
!      The righthand side...
!--------------------------------------------------------------------------
     Force = 0.0d0

     DO p=1,NBasis
        Load = Basis(p) + Tau * SW(p)
        ForceVector(p) = ForceVector(p) + s * Force * Load
     END DO
  END DO

!--------------------------------------------------------------------------
END SUBROUTINE LocalMatrix
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
END SUBROUTINE LevelSetExportSolver
!--------------------------------------------------------------------------

