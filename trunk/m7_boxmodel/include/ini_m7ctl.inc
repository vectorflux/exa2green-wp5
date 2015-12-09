!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!>
!! \filename 
!! ini_m7ctl
!!
!! \brief
!! namelist for the initialization of the M7 boxmodel
!!
!! \author Grazia Frontoso (C2SM-ETHZ)
!!
!! \responsible_coder
!! Grazia Frontoso, grazia.frontoso@env.ethz.ch
!!
!! \limitations
!! None
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

NAMELIST /ini_m7ctl/  pap,      & !< Pressure (Pa)
                      pt,       & !< Temperature (K)
                      prh,      & !< RH (as saturation ratio)
                      pipr,     & !< Ionization rate [cm-3 s-1]
                      pdgso4,   & !< H2SO4(g) production rate (cm-3 s-1)
                      pgso4,    & !< H2SO4(g) concentrations (cm-3)
                      paclc,    & !< Cloud cover [0,1] (set to zero unless you know what you are doing)
                      pforest,  & !< Forest fraction
                      paerml,   & !< Aerosol mass concentration for individual compounds
                                  !< (molec. cm-3 for sulfate and
                                  !< ug m-3 for bc, oc, ss, and dust)
                      paernl,   & !< Particle number concentrations for each mode (cm-3)
                      pm6rp,    & !< Particle mean mode actual radius for each mode (wet for soluble and dry for insoluble modes) (cm) 
                      pm6dry,   & !< Particle dry radius for each soluble modes (cm)
                      prhop,    & !< Mean mode particle density for each mode (g cm-3)
                      pww         !< Aerosol water content for each mode [kg(water) m-3(air)]