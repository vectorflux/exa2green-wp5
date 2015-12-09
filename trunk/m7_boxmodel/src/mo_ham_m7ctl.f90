!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!>
!! \filename 
!! mo_ham_m7ctl.f90
!!
!! \brief
!! mo_ham_m7ctl contains parameters, switches and initialization routines for the m7 aerosol module.
!!
!! \author Elisabetta Vignatti (JRC/EI)
!! \author Philip Stier (MPI-Met)
!!
!! \responsible_coder
!! Martin G. Schultz, m.schultz@fz-juelich.de
!!
!! \revision_history
!!   -# E. Vignati and J. Wilson (JRC/EI) - original code (2000)
!!   -# P. Stier (MPI-Met) (2001/2002)
!!   -# J. Kazil (MPI-Met) (2008)
!!   -# D. O'Donnell (MPI-Met) (2007-2007)
!!   -# M.G. Schultz (FZ Juelich) - new module struture (2009)
!! 
!! \limitations
!! Currently, there are two index lists for aerosol species: aero_idx in mo_species
!! and m7_aerospec in this module. I hope these are identical for the current model set-up 
!! in preparation for CMIP5. Later, one may wish to distinguish between the two: aero_idx
!! could contain additional aerosol species (e.g. from MOZART or climatologies), and this could
!! mess up the M7 code. If this can be generalized: fine. if not we should keep the two 
!! lists separate. mo_ham_rad (for example) works on aero_idx to be independent of M7 specifics.
!!
!! \details
!! None
!!
!! \bibliographic_references
!! None
!!
!! \belongs_to
!!  HAMMOZ
!!
!! \copyright
!! Copyright and licencing conditions are defined in the ECHAM-HAMMOZ
!! licencing agreement to be found at:
!! https://redmine.hammoz.ethz.ch/projects/hammoz/wiki/1_Licencing_conditions
!! The ECHAM-HAMMOZ software is provided "as is" and without warranty of any kind.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE mo_ham_m7ctl

  USE mo_kind,             ONLY: dp
  USE mo_constants,        ONLY: api, avo
  USE mo_species,          ONLY: nmaxspec

  IMPLICIT NONE

  PRIVATE

  ! -- subroutines
  PUBLIC :: sethamM7, m7_initialize

  ! -- variables
!gf
  PUBLIC :: kproma, kbdim, klev, pbl
!gf
  PUBLIC :: nmod, naerocompmax, nsol
  PUBLIC :: lscoag, lscond, nwater, nsnucl, nonucl
  PUBLIC :: inucs, iaits, iaccs, icoas, iaiti, iacci, icoai

  PUBLIC :: critn, cmin_aernl, cmin_aerml
  PUBLIC :: wna2so4, wh2so4, wnacl, wnahso4
  PUBLIC :: dna2so4, dh2so4, dnacl, dnahso4
  PUBLIC :: dbc, doc, ddust, crh

  PUBLIC :: sigma, sigmaln
  PUBLIC :: crdiv, caccso4, gmb, wvb, dh2o
  PUBLIC :: bk, rerg, r_kcal

  PUBLIC :: immr2ug, immr2molec, ivmr2molec  ! unit conversion for M7 species
  PUBLIC :: m7_gasspec, m7_aerospec, m7_naerospec, m7_ngasspec, m7_aero_idx
  PUBLIC :: m7_aerounitconv, m7_gasunitconv

  PUBLIC :: so4_coating_threshold
  PUBLIC :: cmr2ras, cmr2mmr, cmedr2mmedr, cmr2ram, ram2cmr

  !--- 1) Define and pre-set switches for the processes of M7: -----------------------

  !--- Physical:

  LOGICAL :: lscoag     = .TRUE.    ! Coagulation
  LOGICAL :: lscond     = .TRUE.    ! Condensation of H2SO4
  
  INTEGER :: nwater     = 1         ! Aerosol water uptake scheme:
                                    !
                                    ! nwater = 0 Jacobson et al., JGR 1996
                                    !        = 1 Kappa-Koehler theory based approach (Petters and Kreidenweis, ACP 2007)
  
  INTEGER :: nsnucl     = 2         ! Choice of the H2SO4/H2O nucleation scheme:
                                    ! 
                                    ! nsnucl = 0 off
                                    !        = 1 Vehkamaeki et al., JGR 2002
                                    !        = 2 Kazil and Lovejoy, ACP 2007
  
  INTEGER :: nonucl     = 1         ! Choice of the organic nucleation scheme:
                                    ! 
                                    ! nonucl = 0 off
                                    !        = 1 Activation nucleation, Kulmala et al., ACP 2006
                                    !        = 2 Activation nucleation, Laakso et al., ACP 2004
  
  !--- Technical:

  ! Module variables
  INTEGER, PARAMETER     :: immr2ug    = 1        ! Mass mixing ratio to ug m-3
  INTEGER, PARAMETER     :: immr2molec = 2        ! Mass mixing ratio to molecules cm-3
  INTEGER, PARAMETER     :: ivmr2molec = 3        ! Volume mixing ratio to molecules cm-3

  INTEGER                :: m7_ngasspec  = 0
  INTEGER                :: m7_naerospec = 0
  INTEGER                :: m7_gasspec(nmaxspec)      ! gas species indices for M7 processes
  INTEGER                :: m7_aerospec(nmaxspec)     ! aero species indices for M7 processes
  INTEGER                :: m7_aero_idx(nmaxspec)     ! mapping from speclist to m7_aerospec
  INTEGER                :: m7_gasunitconv(nmaxspec)  ! unit conversion flag for m7_gasspec
  INTEGER                :: m7_aerounitconv(nmaxspec) ! unit conversion flag for m7_aerospec


  !--- 2) Numbers of compounds and modes of m7: --------------------------------------

!gf  INTEGER            :: naerocomp
  INTEGER, PARAMETER            :: naerocompmax=46

  INTEGER, PARAMETER :: nmod=7,            & !number of modes
                        nsol=4               !number of soluble  compounds 
!gf

  INTEGER,  PARAMETER :: kproma = 1,    & ! ECHAM6 geographic block number of locations
                         kbdim  = 1,    & !   "    geographic block maximum number of locations
                         klev   = 1       !   "    number of levels
  
  REAL(dp), PARAMETER :: pbl    = 1._dp   ! Planetary boundary top level

!gf



  !--- 3) List of indexes corresponding to the compound masses and mode numbers:------

  !--- 3.1) Mass index (in array aerml and ttn): 
  !
  !         Attention:
  !         The mass of sulfate compounds is always given in [molec. cm-3] 
  !         whilst the mass of other compounds is given in [ug cm-3].
  !
  !         Compounds:
  !
  !           so4 = sulphate
  !           bc  = black carbon
  !           oc  = organic carbon, 
  !           ss  = sea salt
  !           du  = dust 
  !
  !         Modes:
  !
  !           n   = nucleation mode
  !           k   = aitken mode 
  !           a   = accumulation mode
  !           c   = coarse mode
  !
  !         Type:
  !
  !           s   = soluble mode
  !           i   = insoluble mode
  
  ! M7 aerosol mode indices
  ! i*[n|k|a|c][i|s]   :   n = nucleation mode
  !                        k = aitken mode
  !                        a = accumulation mode
  !                        c = coarse mode
  !                        i = insoluble
  !                        s = soluble
  ! empty matrix entries are not populated.
  INTEGER, PUBLIC :: iso4ns, iso4ks, iso4as, iso4cs, &  
                     ibcks,  ibcas,  ibccs,  ibcki,  &
                     iocks,  iocas,  ioccs,  iocki,  &
                     issas,  isscs,                  &
                     iduas,  iducs,  iduai,  iduci

  TYPE, PUBLIC :: lognormal_mode 
     CHARACTER (LEN=32) :: modename          ! Long mode name, e.g. "nucleation soluble"
     CHARACTER (LEN=2)  :: shortname         ! Short mode name, e.g. "NS"
     INTEGER            :: self              ! =mode index, for quick comparisons, etc
     LOGICAL            :: lsoluble          ! Mode soluble (T/F)
     INTEGER            :: idt_no            ! Tracer identity for aerosol number
  END TYPE lognormal_mode                    ! sigma, sigmaln and the conversion factors are
                                             ! kept separate to avoid too many impacts on 
                                             ! other modules and subroutines
  TYPE(lognormal_mode), TARGET, PUBLIC :: m7mode(nmod)

  !--- 3.2) Number index (in array aernl):
  !

  INTEGER, PARAMETER ::                                      &
           inucs=1,  iaits=2,  iaccs=3,  icoas=4,  iaiti=5,  iacci=6,  icoai=7    
  ! MODE:           |         |         |         |         |
  !         nucl.   | aitk.   | acc.    | coar.   | aitk.   | acc.    | coar.   |
  !         soluble | soluble | soluble | soluble | insol.  | insol.  | insol.  |


  !--- 4) Definition of the modes of M7: ------------------------------------------------------

  !--- 4.1) Threshold radii between the different modes [cm]:
  !         Used for the repartititioning in m7_dconc.
  !         crdiv(jclass) is the lower bound and crdiv(jclass+1) is 
  !         the upper bound of the respective geometric mode.

  REAL(dp) :: crdiv(4)=(/ 0.0005E-4_dp, 0.005E-4_dp, 0.05E-4_dp, 0.5E-4_dp /)    
  !                             |         |        |      
  !                             |         |        |
  !                 nucleation -- aitken  -  accum -- coarse mode

  !--- 4.2) Standard deviation for the modes:

  REAL(dp), PARAMETER :: sigma(nmod)=(/ 1.59_dp, 1.59_dp, 1.59_dp, 2.00_dp, 1.59_dp, 1.59_dp, 2.00_dp /)

  !--- Natural logarithm of the standard deviation of each mode:
  !    Calulated in m7_initialize. 

  REAL(dp) :: sigmaln(nmod)

  !--- 5) Conversion factors for lognormal particle size distributions: -------------
  !       Calulated in m7_initialize. 

  REAL(dp) :: cmr2ras(nmod) ! Conversion factor: count median radius to radius of average surface

  REAL(dp) :: cmr2mmr(nmod) ! Conversion factor: count median radius to mass mean radius

  REAL(dp) :: cmedr2mmedr(nmod) ! Conversion factor: count median radius to mass median radius

  REAL(dp) :: cmr2ram(nmod) ! Conversion factor: count median radius to radius of average mass

  REAL(dp) :: ram2cmr(nmod) ! Conversion factor: radius of average mass to count median radius


  !--- 6) Assumed thresholds for occurence of specific quantities: -------------

  REAL(dp), PARAMETER :: cmin_aerml     = 1.E-15 , & ! Aerosol mass
                         cmin_aernl     = 1.E-10     ! Aerosol number               
  
  !--- 7) Chemical constants: ----------------------------------------------------
  !
  !--- Accomodation coefficient of H2SO4 on aerosols:
  !    (reduced for insoluble modes)

  REAL(dp), PARAMETER :: caccso4(nmod) = (/ 1.0_dp, 1.0_dp, 1.0_dp, 1.0_dp, 0.3_dp, 0.3_dp, 0.3_dp /)

  !--- Critical relative humidity:

  REAL(dp), PARAMETER :: crh    = 0.45_dp           ! Assumed relative humidity for the 
                                             ! Na2SO4 / NaCl system below which 
                                             ! crystalization occurs.
                                             ! (estimated from Tang, I.N.; JGR 102, D2 1883-1893)

  !--- 8) Physical constants: ----------------------------------------------------
  !
  !--- 8.1) General physical constants: 

  REAL(dp), PARAMETER :: bk      = 1.38e-16_dp,   & ! Bolzman constant []   ! ### use ak from mo_constants and scale!
                         rerg    = 8.314E+7_dp,   & ! Ideal gas constant [erg.K-1.mole-1]  ! ### use ar from mo_constants and scale
                         r_kcal  = 1.986E-3_dp      ! Ideal gas constant [kcal K-1.mole-1] ! ### scale from rerg or ar
  
  !--- 8.2) Type specific physical constants:
  !
  REAL(dp), PARAMETER :: dh2so4  = 1.841_dp,      & ! Density          H2SO4  [g cm-3]
                         ddust   = 2.650_dp,      & ! Density          du     [g cm-3]
                         dbc     = 2._dp,         & ! Density          bc     [g cm-3]
                         doc     = 2._dp,         & ! Density          oc     [g cm-3]
                         dnacl   = 2.165_dp,      & ! Density          NaCl   [g cm-3]
                         dna2so4 = 2.68_dp,       & ! Density          Na2SO4 [g cm-3]
                         dnahso4 = 2.435_dp,      & ! Density          NaHSO4 [g cm-3]
                         dh2o    = 1.0_dp,        & ! Density          H2O    [g cm-3]

                         wh2so4  = 98.0734_dp,    & ! Molecular weight H2SO4  [g mol-1]
                         wh2o    = 18.0_dp,       & ! Molecular weight H2O    [g mol-1]
                         wna     = 22.99_dp,      & ! Atomic    weight Na     [g mol-1]
                         wcl     = 35.453_dp,     & ! Atomic    weight Cl     [g mol-1]
                         wnacl   = 58.443_dp,     & ! Molecular weight NaCl   [g mol-1]
                         wna2so4 = 142.0376_dp,   & ! Molecular weight Na2SO4 [g mol-1]
                         wnahso4 = 120.0555_dp      ! Molecular weight NaHSO4 [g mol-1]

  !--- 9) Assumed parameters: ------------------------------------------------------

  REAL(dp), PARAMETER :: so4_coating_threshold = 1.0_dp   ! Assumed required layer thickness of
                                                          ! sulfate to transfer an insoluble 
                                                          ! particle to a soluble mode. It is 
                                                          ! given in units of layers of 
                                                          ! monomolecular sulfate. Determines the
                                                          ! transfer rate from insoluble to 
                                                          ! soluble modes. 
  
  !--- 10) Nucleation constants: ---------------------------------------------------
  
  REAL(dp) :: critn ! Smallest possible number of H2SO4 molecules in a nucleation mode particle
  
  !--- 11) Data used for the calculation of the aerosol properties -----------------
  !        under ambient conditions:
  !        (Included the conversion from Pa to hPa in the first parameter.)

  REAL(dp), PARAMETER :: wvb(17)=                                                   &
                     (/   95.80188_dp,     -28.5257_dp,     -1.082153_dp,     0.1466501_dp, &
                         -20627.51_dp,    0.0461242_dp,     -0.003935_dp,      -3.36115_dp, &
                       -0.00024137_dp,  0.067938345_dp, 0.00000649899_dp,   8616124.373_dp, &
                       1.168155578_dp, -0.021317481_dp,   0.000270358_dp, -1353332314.0_dp, &
                      -0.002403805_dp                                              /)

  REAL(dp), PARAMETER :: gmb(9)=                                                 &
                     (/ 1.036391467_dp, 0.00728531_dp, -0.011013887_dp, -0.068887407_dp, &
                        0.001047842_dp, 0.001049607_dp, 0.000740534_dp, -1.081202685_dp, &
                       -0.0000029113_dp                                         /)

  !--- 4) Logical mask for coagulation kernel: -------------------------------------
  !       (The coagulation kernel mask is symmetric and not all 
  !       values are used for physical considerations. As its 
  !       calculation is very expensive, a mask is used to 
  !       calculate only the necessarey elements.)

  TYPE, PUBLIC :: t_coag
     INTEGER :: mode1
     INTEGER :: mode2
  END TYPE t_coag

  INTEGER, PARAMETER, PUBLIC :: ncoag = 16

  TYPE(t_coag), PUBLIC :: coag_modes(ncoag)

  !--- 12) Service routines for initialization and auxiliary computations ----------

CONTAINS

  SUBROUTINE m7_initialize

    ! Purpose:
    ! ---------
    ! Initializes constants and parameters 
    ! used in the m7 aerosol model.
    !
    ! Author:
    ! ---------
    ! Philip Stier, MPI                          may 2001
    ! Declan O'Donnell, MPI-M, 2008
    !
    ! Interface:
    ! ---------
    ! *m7_initialize*  is called from *start_ham* in mo_ham_init
    !
    USE mo_ham,              ONLY: m7mode

    IMPLICIT NONE

    INTEGER :: jclass
					
    !---executable procedure

    m7mode(1)%modename   = "Nucleation soluble"
    m7mode(1)%shortname  = "NS"
    m7mode(1)%self       = 1
    m7mode(1)%lsoluble   = .TRUE.
    m7mode(1)%lsoainmode = .FALSE.

    m7mode(2)%modename   = "Aitken soluble"
    m7mode(2)%shortname  = "KS"
    m7mode(2)%self       = 2
    m7mode(2)%lsoluble   = .TRUE.               
    m7mode(2)%lsoainmode = .TRUE.

    m7mode(3)%modename   = "Accumulation soluble"
    m7mode(3)%shortname  = "AS"
    m7mode(3)%self       = 3
    m7mode(3)%lsoluble   = .TRUE.                
    m7mode(3)%lsoainmode = .TRUE.

    m7mode(4)%modename   = "Coarse soluble"
    m7mode(4)%shortname  = "CS"
    m7mode(4)%self       = 4
    m7mode(4)%lsoluble   = .TRUE.                 
    m7mode(4)%lsoainmode = .TRUE.

    m7mode(5)%modename   = "Aitken insoluble"
    m7mode(5)%shortname  = "KI"
    m7mode(5)%self       = 5
    m7mode(5)%lsoluble   = .FALSE.                 
    m7mode(5)%lsoainmode = .TRUE.

    m7mode(6)%modename   = "Accumulation insoluble"
    m7mode(6)%shortname  = "AI"
    m7mode(6)%self       = 6
    m7mode(6)%lsoluble   = .FALSE.                 
    m7mode(6)%lsoainmode = .FALSE.

    m7mode(7)%modename   = "Coarse insoluble"
    m7mode(7)%shortname  = "CI"
    m7mode(7)%self       = 7
    m7mode(7)%lsoluble   = .FALSE.
    m7mode(7)%lsoainmode = .FALSE.
    
    ! The following properties could be incorporated into m7mode but it means updating
    ! many impacted modules and subroutines...

    DO jclass=1, nmod

       !--- 1) Calculate conversion factors for lognormal distributions:----
       !       Radius of average mass (ram) to count median radius (cmr) and 
       !       vice versa. Count median radius to radius of average 
       !       mass (ram).
       !       These factors depend on the standard deviation (sigma)
       !       of the lognormal distribution.
       !       (Based on the Hatch-Choate Conversins Equations;
       !        see Hinds, Chapter 4.5, 4.6 for more details.
       !        In particular equation 4.53.)

       !--- Count Median Radius to Mass Median Radius:

       cmedr2mmedr(jclass) = EXP(3.0_dp*(LOG(sigma(jclass)))**2)

       !--- Count Median Radius to Mass Mean Radius:

       cmr2mmr(jclass) = EXP(3.5_dp*(LOG(sigma(jclass)))**2)

       !--- Count Median Radius to Radius of Average Mass:

       cmr2ram(jclass) = EXP(1.5_dp*(LOG(sigma(jclass)))**2)

       !--- Radius of Average Mass to Count Median Radius:

       ram2cmr(jclass) = 1._dp / cmr2ram(jclass)

       !--- Count Median Radius to Radius of Average Surface:

       cmr2ras(jclass) = EXP(1.0_dp*(LOG(sigma(jclass)))**2)


       !--- 2) Calculate the natural logarithm of the standard deviation:

       sigmaln(jclass) = LOG(sigma(jclass))

    END DO

    !--- 3) Nucleation mode constants:
    !
    !    3.1) Set the lower mode boundary particle dry radius for the nucleation
    !         mode (does not depend on the choice of the nucleation scheme, as
    !         we use different ones which produce particles of different sizes):
    
    crdiv(1) = 0.5E-7_dp ! cm
    
    !    3.2) Smallest possible number of H2SO4 molecules in a nucleation mode
    !         particle:
    
    critn = crdiv(1)**(3.0_dp)*api*avo*dh2so4/wh2so4/0.75_dp
    
    !--------------------------------------------------------------------

    ! nucleation mode 
    coag_modes(1)%mode1 = inucs
    coag_modes(1)%mode2 = inucs
    coag_modes(2)%mode1 = inucs
    coag_modes(2)%mode2 = iaits
    coag_modes(3)%mode1 = inucs
    coag_modes(3)%mode2 = iaccs
    coag_modes(4)%mode1 = inucs
    coag_modes(4)%mode2 = icoas
    coag_modes(5)%mode1 = inucs
    coag_modes(5)%mode2 = iaiti
    coag_modes(6)%mode1 = inucs
    coag_modes(6)%mode2 = iacci
    coag_modes(7)%mode1 = inucs
    coag_modes(7)%mode2 = icoai

    ! aitken soluble mode
    coag_modes(8)%mode1 = iaits
    coag_modes(8)%mode2 = iaits
    coag_modes(9)%mode1 = iaits
    coag_modes(9)%mode2 = iaccs
    coag_modes(10)%mode1 = iaits
    coag_modes(10)%mode2 = icoas
    coag_modes(11)%mode1 = iaits
    coag_modes(11)%mode2 = iaiti
    coag_modes(12)%mode1 = iaits
    coag_modes(12)%mode2 = iacci
    coag_modes(13)%mode1 = iaits
!gf(#135)    coag_modes(13)%mode2 = icoas
    coag_modes(13)%mode2 = icoai

    ! accumulation soluble mode
    coag_modes(14)%mode1 = iaccs
    coag_modes(14)%mode2 = iaccs
    coag_modes(15)%mode1 = iaccs
!gf #158    coag_modes(15)%mode2 = iacci
    coag_modes(15)%mode2 = iaiti

    ! aitken insoluble mode
    coag_modes(16)%mode1 = iaiti
    coag_modes(16)%mode2 = iaiti

  END SUBROUTINE m7_initialize

  SUBROUTINE sethamM7
    
    ! *sethamM7* modifies pre-set switches of the aeroM7ctl
    !             namelist for the configuration of the 
    !             M7 component of the ECHAM/HAM aerosol model
    ! 
    ! Authors:
    ! --------
    ! Philip Stier, MPI-M                                                12/2002
    ! Jan Kazil, MPI-M                                       2008-03-03 20:34:52
    !
    ! *sethamM7* is called from *init_subm* in mo_submodel_interface
    !
    
    USE mo_namelist,  ONLY: open_nml, position_nml, POSITIONED
    
    IMPLICIT NONE
    
    INCLUDE 'ham_m7ctl.inc'
    
    ! Local variables:
    
    INTEGER :: ierr, inml, iunit

    ! preset default values
    m7_gasspec(:)  = 0
    m7_aerospec(:) = 0
    m7_aero_idx(:) = 0
 
    ! Read the namelist with the switches:
    
      inml = open_nml('namelist.echam') 
      iunit = position_nml ('HAM_M7CTL', inml, status=ierr)
      SELECT CASE (ierr)
      CASE (POSITIONED)
      READ (iunit, ham_m7ctl)
      END SELECT
        
  END SUBROUTINE sethamM7

END MODULE mo_ham_m7ctl
