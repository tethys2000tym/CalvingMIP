!******************************************************************************
! *
! *  Authors: Fabien Gillet-Chaulet and Cruz Garcia-Molina
! *  Email:   Cruz.Garcia-molina@univ-grenoble-alpes.fr fabien.gillet-chaulet@univ-grenoble-alpes.fr
! *  Address: IGE - OSUG B
! *           460 rue de la Piscine 
! *           Domaine universitaire 
! *           38400 St Martin d'Hères, France
! *
! *  Original Date: 16.03.2021
!------------------------------------------------------------------------------
 FUNCTION passive_xc(Model,nodenumber,VarIn) RESULT(VarOut)
    USE DefUtils
    implicit none
   !-----------------
   TYPE(Model_t) :: Model
    INTEGER :: nodenumber
   REAL(kind=dp),INTENT(IN)  :: VarIn
   REAL(kind=dp) :: VarOut
   LOGICAL,SAVE :: FirstTime=.TRUE.
   REAL(kind=dp),SAVE :: xc0
   REAL(kind=dp) :: Time,xc
   !------------------------------------------------------------------------------
   IF (FirstTime) THEN
           xc0 = ListGetConstReal( Model %  Constants,'x_crit',UnFoundFatal=.TRUE.)
           FirstTime=.FALSE.
   ENDIF
   VarOut=VarIn-xc0
   End FUNCTION Passive_xc

      FUNCTION passive_xct(Model,nodenumber,VarIn) RESULT(VarOut)
       USE DefUtils
       implicit none
       !-----------------
       TYPE(Model_t) :: Model
       INTEGER :: nodenumber
       REAL(kind=dp),INTENT(IN) :: VarIn 
       REAL(kind=dp) :: VarOut 
       LOGICAL,SAVE :: FirstTime=.TRUE.
       REAL(kind=dp),SAVE :: xc0,rate
       REAL(kind=dp) :: Time,xc
       !TYPE(Variable_t), POINTER :: xc

       !------------------------------------------------------------------------------

       IF (FirstTime) THEN
         xc0 = ListGetConstReal( Model % Constants, 'x_crit', UnFoundFatal=.TRUE. )
         rate = ListGetConstReal( Model % Constants, 'x_crit rate', UnFoundFatal=.TRUE. )
         FirstTime=.FALSE.
       ENDIF

       Time = GetTime()
       xc=xc0+rate*Time
       VarOut=VarIn-xc

       End FUNCTION passive_xct

       FUNCTION LevelSetpassive(Model,nodenumber,VarIn) RESULT(VarOut)
       USE DefUtils
       implicit none
       !-----------------
       TYPE(Model_t) :: Model
       INTEGER :: nodenumber
       REAL(kind=dp),INTENT(IN) :: VarIn 
       REAL(kind=dp) :: VarOut 

       TYPE(Variable_t),POINTER :: LS
       TYPE(Element_t),POINTER :: Element
       INTEGER :: n
       REAL(kind=dp),ALLOCATABLE,SAVE :: NodalLS(:)
       LOGICAL,SAVE :: FirstTime=.TRUE.

       If (FirstTime) THEN
         FirstTime=.FALSE.
         N = Model%Mesh%MaxElementNodes
         Allocate(NodalLS(N))
       End If

       Element => CurrentModel % CurrentElement
       IF (.NOT.ANY(Element % NodeIndexes.EQ.nodenumber)) THEN
         PRINT *,Element%ElementIndex,Element % NodeIndexes,nodenumber
         CALL FATAL('PASSIVE','Current node is not in current Element')
       END IF
       n = GetElementNOFNodes(Element)

       LS => VariableGet(Model%Mesh%Variables,'LS',UnFoundFatal=.TRUE.)
       NodalLS(1:n) = LS%Values(LS%Perm(Element%NodeIndexes(1:n)))
       
       !Remember if VarOut > 0 then PASSIVE = .TRUE.
       IF (VarIN.LT.0._dp) THEN ! .not.VarExport
         IF (ANY(NodalLS(1:n).LE.0._dp)) THEN ! All the nodes should be passive to have an element passive
          VarOut=-1.0
         ELSE
          VarOut=+1.0_dp
        END IF
       ELSE
        IF (ALL(NodalLS(1:n).GE.0._dp)) THEN ! CAREFUL!! ALL THE ACTIVE NODES WERE SET TO PASSIVE AND VICEVERSA
         VarOut=-1.0 !If any of the nodes is passive or touching the border LS=0 the element is PASSIVE
        ELSE
         VarOut=+1.0_dp
        END IF
       ENDIF

       End FUNCTION LevelSetpassive

