!! --------------------------------------------------------------------------------------------------
!!mgs : henry and dryreac removed from trlist (see speclist!)
!!mgs : Cleanups ToDo:
!!      -- ndrydep, nwetdep, n... :  consistent scheme with -1 = interactive (choice in submodelctl)
!!                                                           0 = OFF
!!                                                           1 = prescribed (boundary condition)
!! --------------------------------------------------------------------------------------------------

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!>
!! Definition of tracer information data type and variable
!!
!!
!! @author 
!! <ol>
!! <li>ECHAM5 developers
!! <li>M. Schultz (FZ-Juelich)
!! <li>S. Rast (MPI-Met)
!! <li>K. Zhang(MPI-Met)
!! </ol>
!!
!! $Id: 1423$
!!
!! @par Revision History
!! <ol>
!! <li>ECHAM5 developers - (before 2009)
!! <li>M. Schultz   (FZ-Juelich), S. Rast (MPI-Met) -  new tracer defination - (2009-05-xx) 
!! <li>K. Zhang     (MPI-Met)    -  implementation in ECHAM6 and doxygen support  - (2009-07-20)
!! </ol>
!!
!! @par This module is used by
!! to_be_added
!!  
!! @par Notes
!! 
!!
!! @par Current responsible coder
!! kai.zhang@zmaw.de
!!
!! @par Copyright
!! 2009 by MPI-M
!! This software is provided for non-commercial use only.
!! See the LICENSE and the WARRANTY conditions.
!!
!! @par License
!! The use of ECHAM is hereby granted free of charge for an unlimited time,
!! provided the following rules are accepted and applied:
!! <ol>
!! <li> You may use or modify this code for your own non commercial and non
!! violent purposes.
!! <li> The code may not be re-distributed without the consent of the authors.
!! <li> The copyright notice and statement of authorship must appear in all
!! copies.
!! <li> You accept the warranty conditions (see WARRANTY).
!! <li> In case you intend to use the code commercially, we oblige you to sign
!! an according license agreement with MPI-M.
!! </ol>
!!
!! @par Warranty
!! This code has been tested up to a certain level. Defects and weaknesses,
!! which may be included in the code, do not establish any warranties by the
!! authors.
!! The authors do not make any warranty, express or implied, or assume any
!! liability or responsibility for the use, acquisition or application of this
!! software.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE mo_tracdef


  USE mo_kind,           ONLY: dp

  
  IMPLICIT NONE

  !-- flag values

  !   'nphase'
  PUBLIC :: GAS
  PUBLIC :: AEROSOL
  PUBLIC :: GAS_OR_AEROSOL

  ! phase indicator  (nphase)
  !
  INTEGER, PARAMETER :: GAS            = 1  ! gas
  INTEGER, PARAMETER :: AEROSOL        = 2  ! aerosol (for species definition)
  INTEGER, PARAMETER :: GAS_OR_AEROSOL = 3  ! gas or aerosol (for species definition)

END MODULE mo_tracdef

