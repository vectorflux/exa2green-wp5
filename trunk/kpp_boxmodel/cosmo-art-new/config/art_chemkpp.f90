!     Last change:  BV   15 Apr 2009    4:25 pm
!+ Module for
!------------------------------------------------------------------------------

MODULE art_chemkpp

!------------------------------------------------------------------------------
!
! Description:
!
! Current Code Owner: IMK, Heike Vogel
!  phone:  +49  7216082 3951
!  fax:    +49  7216082 4742
!  email:  heike.vogel@kit.edu    
!
! History:
! Version    Date       Name
! ---------- ---------- ----
! 1.0        !DATE!     Heike Vogel
!  Initial Release
! 1.1        31.01.2012 Martin Wlotzka, Isabel Kraut, Heike Vogel
!  Switch for KPP
!
! Code Description:
! Language: Fortran 90.
! Software Standards: "European Standards for Writing and
! Documenting Exchangeable Fortran 90 Code".
!==============================================================================

  USE data_parameters,    ONLY: ireals, iintegers
  USE data_modelconfig,   ONLY: ie, je, ke,istart,iend,jstart,jend,degrad,dlon,dlat,dt, dt2
  USE data_parallel,      ONLY:  my_cart_id
  USE data_fields     ,   ONLY: p0, pp, t
  USE data_cosmo_art,     ONLY: vd,vdepa,vseda,                       &
                                mjval, isp_aero, lgas, laero, lseas,  &
                                lonly_dust, ustern,k_het,ledms,       &
                                kpp_conc, kpp_photo, kpp_temp, kpp_m, &
                                cradicals, kpp_het,                   &
                                eseasa, eseasb, eseasc,               &
                                esoila, esoilb, esoilc,               &
                                qseas1, qseas2, qseas3,               &
                                qdust1, qdust2, qdust3
  USE data_runcontrol,    ONLY: nold, nnow, nnew, ntstep
  USE art_gas_const
  USE art_aerosol_const
  USE art_species_data ! CK 20110426 for the shortcut indizes
  USE data_parallel,      ONLY : nboundlines,my_cart_id,icomm_cart,imp_reals
  USE parallel_utilities, ONLY : global_values
  USE environment,        ONLY: model_abort
  USE time_utilities,     ONLY : get_timings, i_art_vorher, i_art_chemie, i_art_aerosol
  USE data_constants  ,   ONLY : r_earth


  USE art_deposition, ONLY : wstern,ra

  USE kpp_Main,           ONLY : kpp_step
  
  USE src_tracer,         ONLY: trcr_get, trcr_meta_get, trcr_errorstr, trcr_get_ntrcr,         &
                              trcr_get_block

!==============================================================================

IMPLICIT NONE

!==============================================================================
!

! Variables used for the whole module
!-------------------------------------

  INTEGER (KIND=iintegers)    :: izerror 
              
  CHARACTER (LEN=80)          :: yzerrmsg
                  
  CHARACTER (LEN=25)          :: yzroutine

  ! Tracer pointers:
   REAL (KIND=ireals), POINTER ::                        &
     cgas_new   (:,:,:,:)  => NULL(),                    &! cgas at nnew
     caero_new  (:,:,:,:)  => NULL()                      ! caero at nnew

CONTAINS

!************************************************************************

SUBROUTINE cchemkpp(dtmod,kkz,zacthour)


 IMPLICIT NONE


 REAL (KIND=ireals)                       ::         &
    dtmod,                                           &
    zeit,dx,dy,zacthour

 INTEGER (KIND=iintegers)                 ::         &
    kkz,                                             &
    icnt,                                            &
    j,l, ii,jj, ll,                                  &
    errmark,                                         &
    stat_var

 INTEGER (KIND=iintegers) ::                         &
    idx_s_aero, idx_e_aero, loc_isp_aero

 REAL (KIND=ireals), ALLOCATABLE ::                  &
    rand(:,:)

 icnt   = 0
 zeit   = zacthour * 3600._ireals

   dx= r_earth * dlon * degrad
   dy= r_earth * dlat * degrad

 IF (laero) THEN

   IF (lonly_dust) THEN
     idx_s_aero = trcr_idx_aero(vsoila)
     idx_e_aero = trcr_idx_aero(vsoilc3)
     loc_isp_aero = 9
   ELSE
     idx_s_aero = trcr_idx_aero(1)
     idx_e_aero = trcr_idx_aero(isp_aero)
     loc_isp_aero = l1ae
   END IF
   CALL trcr_get_block(izerror, idx_start = idx_s_aero, idx_end = idx_e_aero,  &
                       ptr_tlev = nnew, ptr = caero_new)
   IF (izerror /= 0) THEN
     yzerrmsg = trcr_errorstr(izerror)
     CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
   ENDIF

   vdep = 0._ireals
   vsed = 0._ireals
   DO l = 1, loc_isp_aero
     DO j = 1,(iend-nboundlines)*(jend-nboundlines)
       jj = j+nboundlines-(INT((j-1)/(jend-nboundlines))*(jend-nboundlines))
       ii = INT((j-1)/(jend-nboundlines))+jstart
       cblk(j,l) = caero_new(ii,jj,kkz,l)
     END DO
   END DO

   IF (lonly_dust) THEN
     esoila(:)       = 0._ireals
     esoilb(:)       = 0._ireals
     esoilc(:)       = 0._ireals
     IF (kkz.eq.(kmax)) THEN
       DO j = 1,imax*jmax
         jj = j+nboundlines-(INT((j-1)/(jend-nboundlines))*(jend-nboundlines))
         ii = INT((j-1)/(jend-nboundlines))+jstart
         esoila(j) = qdust1(ii,jj)
         esoilb(j) = qdust2(ii,jj)
         esoilc(j) = qdust3(ii,jj)
       ENDDO
     ENDIF   ! k = ke
   ELSE
     eseasa(:)       = 0._ireals
     eseasb(:)       = 0._ireals
     eseasc(:)       = 0._ireals
     esoila(:)       = 0._ireals
     esoilb(:)       = 0._ireals
     esoilc(:)       = 0._ireals
     IF (kkz.eq.(kmax)) THEN
       DO j = 1,imax*jmax
         jj = j+nboundlines-(INT((j-1)/(jend-nboundlines))*(jend-nboundlines))
         ii = INT((j-1)/(jend-nboundlines))+jstart
         eseasa(j) = qseas1(ii,jj)
         eseasb(j) = qseas2(ii,jj)
         eseasc(j) = qseas3(ii,jj)
         esoila(j) = qdust1(ii,jj)
         esoilb(j) = qdust2(ii,jj)
         esoilc(j) = qdust3(ii,jj)
       ENDDO
     ENDIF   ! k = ke
   END IF

 END IF    ! laero

 CALL org_chem (kkz,dtmod,zeit,errmark)


 IF (laero) THEN

   DO l = 1, loc_isp_aero
     DO j = 1,(iend-nboundlines)*(jend-nboundlines)
       jj = j+nboundlines-(INT((j-1)/(jend-nboundlines))*(jend-nboundlines))
       ii = INT((j-1)/(jend-nboundlines))+jstart
       caero_new(ii,jj,kkz,l) = cblk(j,l)
     END DO
   END DO

   IF (lonly_dust) THEN

     IF(kkz == ke) THEN
       DO j = 1,(iend-nboundlines)*(jend-nboundlines)
         jj = j+nboundlines-(INT((j-1)/(jend-nboundlines))*(jend-nboundlines))
         ii = INT((j-1)/(jend-nboundlines))+jstart
         vdepa(ii,jj, vsoila )   =   vdep(j, vdmsoila )
         vdepa(ii,jj, vsoilb )   =   vdep(j, vdmsoilb )
         vdepa(ii,jj, vsoilc )   =   vdep(j, vdmsoilc )
         vdepa(ii,jj, vsoila0)   =   vdep(j, vdnsoila )
         vdepa(ii,jj, vsoilb0)   =   vdep(j, vdnsoilb )
         vdepa(ii,jj, vsoilc0)   =   vdep(j, vdnsoilc )
       END DO
     END IF    !k = ke

     DO j = 1,(iend-nboundlines)*(jend-nboundlines)
       jj = j+nboundlines-(INT((j-1)/(jend-nboundlines))*(jend-nboundlines))
       ii = INT((j-1)/(jend-nboundlines))+jstart
       !h       sedimentation
       vseda(ii,jj,kkz, vsoila )     =  - vsed(j, vsmsoila )
       vseda(ii,jj,kkz, vsoilb )     =  - vsed(j, vsmsoilb )
       vseda(ii,jj,kkz, vsoilc )     =  - vsed(j, vsmsoilc )
       vseda(ii,jj,kkz, vsoila0 )    =  - vsed(j, vsnsoila )
       vseda(ii,jj,kkz, vsoilb0 )    =  - vsed(j, vsnsoilb )
       vseda(ii,jj,kkz, vsoilc0 )    =  - vsed(j, vsnsoilc )
     ENDDO

   ELSE

     IF(kkz == ke) THEN
       DO j = 1,(iend-nboundlines)*(jend-nboundlines)
         jj = j+nboundlines-(INT((j-1)/(jend-nboundlines))*(jend-nboundlines))
         ii = INT((j-1)/(jend-nboundlines))+jstart
         vdepa(ii,jj, vso4aj )  =  vdep(j, vdmacc )
         vdepa(ii,jj, vso4ai )  =  vdep(j, vdmnuc )
         vdepa(ii,jj, vnh4aj )  =  vdep(j, vdmacc )
         vdepa(ii,jj, vnh4ai )  =  vdep(j, vdmnuc )
         vdepa(ii,jj, vno3aj )  =  vdep(j, vdmacc )
         vdepa(ii,jj, vno3ai )  =  vdep(j, vdmnuc )
         vdepa(ii,jj, vorg1j)   =  vdep(j, vdmacc ) !eathana
         vdepa(ii,jj, vorg1i)   =  vdep(j, vdmnuc ) !eathana
         vdepa(ii,jj, vorg10j)   =  vdep(j, vdmacc ) !eathana
         vdepa(ii,jj, vorg10i)   =  vdep(j, vdmnuc ) !eathana
         vdepa(ii,jj, vorg100j)  =  vdep(j, vdmacc ) !eathana
         vdepa(ii,jj, vorg100i)  =  vdep(j, vdmnuc ) !eathana
         vdepa(ii,jj, vorg1000j) =  vdep(j, vdmacc ) !eathana
         vdepa(ii,jj, vorg1000i) =  vdep(j, vdmnuc ) !eathana
         vdepa(ii,jj, vorgpaj)   =  vdep(j, vdmacc )
         vdepa(ii,jj, vorgpai)   =  vdep(j, vdmnuc )
         vdepa(ii,jj, vso4ajm)   =  vdep(j, vdmaccm )
         vdepa(ii,jj, vso4aim)   =  vdep(j, vdmnucm )
         vdepa(ii,jj, vnh4ajm)   =  vdep(j, vdmaccm)
         vdepa(ii,jj, vnh4aim)   =  vdep(j, vdmnucm)
         vdepa(ii,jj, vno3ajm)   =  vdep(j, vdmaccm)
         vdepa(ii,jj, vno3aim)   =  vdep(j, vdmnucm)
         vdepa(ii,jj, vorg1jm)   =  vdep(j, vdmaccm) !eathana  !vdepa did not exist in the previous version, for mixed mode organics
         vdepa(ii,jj, vorg1im)   =  vdep(j, vdmnucm) !eathana
         vdepa(ii,jj, vorg10jm)  =  vdep(j, vdmaccm) !eathana
         vdepa(ii,jj, vorg10im)  =  vdep(j, vdmnucm) !eathana
         vdepa(ii,jj, vorg100jm) =  vdep(j, vdmaccm) !eathana
         vdepa(ii,jj, vorg100im) =  vdep(j, vdmnucm) !eathana
         vdepa(ii,jj, vorg1000jm)=  vdep(j, vdmaccm) !eathana
         vdepa(ii,jj, vorg1000im)=  vdep(j, vdmnucm) !eathana
         vdepa(ii,jj, vorgpajm)  =  vdep(j, vdmaccm)
         vdepa(ii,jj, vorgpaim)  =  vdep(j, vdmnucm)  
         vdepa(ii,jj, vsootj)    =  vdep(j, vdmaccm)
         vdepa(ii,jj, vsooti)    =  vdep(j, vdmnucm)
         vdepa(ii,jj, vsoot)     =  vdep(j, vdmsoot)
         vdepa(ii,jj, vso4as)    =  vdep(j, vdmsoot)
         vdepa(ii,jj, vnh4as)    =  vdep(j, vdmsoot)
         vdepa(ii,jj, vno3as)    =  vdep(j, vdmsoot)
         vdepa(ii,jj, vecj   )   =  vdep(j, vdmacc )
         vdepa(ii,jj, veci   )   =  vdep(j, vdmnuc )
         vdepa(ii,jj, vp25aj )   =  vdep(j, vdmacc )
         vdepa(ii,jj, vp25ai )   =  vdep(j, vdmnuc )
         vdepa(ii,jj, vantha )   =   vdep(j, vdmcor )
         vdepa(ii,jj, vseasa )   =   vdep(j, vdmseasa )
         vdepa(ii,jj, vseasb )   =   vdep(j, vdmseasb )
         vdepa(ii,jj, vseasc )   =   vdep(j, vdmseasc )
         vdepa(ii,jj, vso4seasa )=  vdep(j, vdmseasa )
         vdepa(ii,jj, vso4seasb )=  vdep(j, vdmseasb )
         vdepa(ii,jj, vso4seasc )=  vdep(j, vdmseasc )
         vdepa(ii,jj, vsoila )   =   vdep(j, vdmsoila )
         vdepa(ii,jj, vsoilb )   =   vdep(j, vdmsoilb )
         vdepa(ii,jj, vsoilc )   =   vdep(j, vdmsoilc )
         vdepa(ii,jj,  vnu0  )   =   vdep(j, vdnnuc )
         vdepa(ii,jj,  vac0  )   =   vdep(j, vdnacc )
         vdepa(ii,jj,  vnu0m )   =   vdep(j, vdnnucm)
         vdepa(ii,jj,  vac0m )   =   vdep(j, vdnaccm)
         vdepa(ii,jj,  vsoot0)   =   vdep(j, vdnsoot )
         vdepa(ii,jj ,  vcorn)   =   vdep(j, vdncor )
         vdepa(ii,jj, vseasa0)   =   vdep(j, vdnseasa )
         vdepa(ii,jj, vseasb0)   =   vdep(j, vdnseasb )
         vdepa(ii,jj, vseasc0)   =   vdep(j, vdnseasc )
         vdepa(ii,jj, vsoila0)   =   vdep(j, vdnsoila )
         vdepa(ii,jj, vsoilb0)   =   vdep(j, vdnsoilb )
         vdepa(ii,jj, vsoilc0)   =   vdep(j, vdnsoilc )
       END DO
     END IF    !k = ke

     DO j = 1,(iend-nboundlines)*(jend-nboundlines)
       jj = j+nboundlines-(INT((j-1)/(jend-nboundlines))*(jend-nboundlines))
       ii = INT((j-1)/(jend-nboundlines))+jstart
       !h       sedimentation
       vseda(ii,jj,kkz, vso4aj )     =  - vsed(j, vsmacc )
       vseda(ii,jj,kkz, vso4ai )     =  - vsed(j, vsmnuc )
       vseda(ii,jj,kkz, vnh4aj )     =  - vsed(j, vsmacc )
       vseda(ii,jj,kkz, vnh4ai )     =  - vsed(j, vsmnuc )
       vseda(ii,jj,kkz, vno3aj )     =  - vsed(j, vsmacc )
       vseda(ii,jj,kkz, vno3ai )     =  - vsed(j, vsmnuc )
       vseda(ii,jj,kkz, vorg1j )     =  - vsed(j, vsmacc )   !eathana
       vseda(ii,jj,kkz, vorg1i )     =  - vsed(j, vsmnuc )   !eathana
       vseda(ii,jj,kkz, vorg10j )    =  - vsed(j, vsmacc )   !eathana
       vseda(ii,jj,kkz, vorg10i )    =  - vsed(j, vsmnuc )   !eathana
       vseda(ii,jj,kkz, vorg100j )   =  - vsed(j, vsmacc )   !eathana
       vseda(ii,jj,kkz, vorg100i )   =  - vsed(j, vsmnuc )   !eathana
       vseda(ii,jj,kkz, vorg1000j )  =  - vsed(j, vsmacc )   !eathana
       vseda(ii,jj,kkz, vorg1000i )  =  - vsed(j, vsmnuc )   !eathana
       vseda(ii,jj,kkz, vorgpaj   )  =  - vsed(j, vsmacc )
       vseda(ii,jj,kkz, vorgpai   )  =  - vsed(j, vsmnuc )
       vseda(ii,jj,kkz, vso4ajm)     =  - vsed(j,vsmaccm )
       vseda(ii,jj,kkz, vso4aim)     =  - vsed(j,vsmnucm )
       vseda(ii,jj,kkz, vnh4ajm)     =  - vsed(j,vsmaccm )
       vseda(ii,jj,kkz, vnh4aim)     =  - vsed(j,vsmnucm )
       vseda(ii,jj,kkz, vno3ajm)     =  - vsed(j,vsmaccm )
       vseda(ii,jj,kkz, vno3aim)     =  - vsed(j,vsmnucm )
       vseda(ii,jj,kkz, vorg1jm)     =  - vsed(j,vsmaccm )   !eathana 
       vseda(ii,jj,kkz, vorg1im)     =  - vsed(j,vsmnucm )   !eathana
       vseda(ii,jj,kkz, vorg10jm)    =  - vsed(j,vsmaccm )   !eathana
       vseda(ii,jj,kkz, vorg10im)    =  - vsed(j,vsmnucm )   !eathana
       vseda(ii,jj,kkz, vorg100jm)   =  - vsed(j,vsmaccm )   !eathana
       vseda(ii,jj,kkz, vorg100im)   =  - vsed(j,vsmnucm )   !eathana
       vseda(ii,jj,kkz, vorg1000jm)  =  - vsed(j,vsmaccm )   !eathana
       vseda(ii,jj,kkz, vorg1000im)  =  - vsed(j,vsmnucm )   !eathana     
       vseda(ii,jj,kkz, vorgpajm )   =  - vsed(j,vsmaccm )
       vseda(ii,jj,kkz, vorgpaim )   =  - vsed(j,vsmnucm )
       vseda(ii,jj,kkz, vsootj )     =  - vsed(j,vsmaccm )
       vseda(ii,jj,kkz, vsooti )     =  - vsed(j,vsmnucm )
       vseda(ii,jj,kkz, vsoot  )     =  - vsed(j,vsmsoot)
       vseda(ii,jj,kkz, vso4as  )    =  - vsed(j,vsmsoot)
       vseda(ii,jj,kkz, vnh4as  )    =  - vsed(j,vsmsoot)
       vseda(ii,jj,kkz, vno3as  )    =  - vsed(j,vsmsoot)
       vseda(ii,jj,kkz, vecj   )     =  - vsed(j, vsmnuc )
       vseda(ii,jj,kkz, veci   )     =  - vsed(j, vsmacc )
       vseda(ii,jj,kkz, vp25aj )     =  - vsed(j, vsmnuc )
       vseda(ii,jj,kkz, vp25ai )     =  - vsed(j, vsmnuc )
       vseda(ii,jj,kkz, vantha )     =  - vsed(j, vsmcor)
       vseda(ii,jj,kkz, vseasa )     =  - vsed(j, vsmseasa )
       vseda(ii,jj,kkz, vseasb )     =  - vsed(j, vsmseasb )
       vseda(ii,jj,kkz, vseasc )     =  - vsed(j, vsmseasc )
       vseda(ii,jj,kkz, vso4seasa )  =  - vsed(j, vsmseasa )
       vseda(ii,jj,kkz, vso4seasb )  =  - vsed(j, vsmseasb )
       vseda(ii,jj,kkz, vso4seasc )  =  - vsed(j, vsmseasc )     
       vseda(ii,jj,kkz, vsoila )     =  - vsed(j, vsmsoila )
       vseda(ii,jj,kkz, vsoilb )     =  - vsed(j, vsmsoilb )
       vseda(ii,jj,kkz, vsoilc )     =  - vsed(j, vsmsoilc )
       vseda(ii,jj,kkz, vnu0   )     =  - vsed(j, vsnnuc)
       vseda(ii,jj,kkz, vac0   )     =  - vsed(j, vsnacc)
       vseda(ii,jj,kkz, vnu0m  )     =  - vsed(j, vsnnucm)
       vseda(ii,jj,kkz, vac0m  )     =  - vsed(j, vsnaccm)
       vseda(ii,jj,kkz, vsoot0 )     =  - vsed(j, vsnsoot)
       vseda(ii,jj,kkz, vcorn  )     =  - vsed(j, vsncor)
       vseda(ii,jj,kkz, vseasa0 )    =  - vsed(j, vsnseasa )
       vseda(ii,jj,kkz, vseasb0 )    =  - vsed(j, vsnseasb )
       vseda(ii,jj,kkz, vseasc0 )    =  - vsed(j, vsnseasc )
       vseda(ii,jj,kkz, vsoila0 )    =  - vsed(j, vsnsoila )
       vseda(ii,jj,kkz, vsoilb0 )    =  - vsed(j, vsnsoilb )
       vseda(ii,jj,kkz, vsoilc0 )    =  - vsed(j, vsnsoilc )
     ENDDO

   END IF

 END IF  ! laero

END SUBROUTINE cchemkpp

!//////////////////////////////////////////////////////////////////////////
!/////////////////////////////////////////////////////////////////////////

SUBROUTINE org_chem (kkz,dtmod, zeit, errmark)


USE art_mademod, ONLY: rpmmod3

 IMPLICIT NONE

 INTEGER (KIND=iintegers)                 ::    &
    kkz,                                        &
    errmark,                                    &
    ii,jj,ir,irdum,k, stat_var,                 &
    lk,l,istep,le,ik,j,lst,ll

 REAL (KIND=ireals)                       ::    &
    dtmod,                                      &
    zeit,                                       &
    f,t_in,                                     &
    f_nuc,f_acc,f_nucm,f_accm,                  &
    gamma_in_nuc,                               &
    gamma_in_acc,                               &
    gamma_in_nucm,                              &
    gamma_in_accm,                              &
    gamma_nuc,                                  &
    gamma_acc,                                  &
    gamma_nucm,                                 &
    gamma_accm,                                 &
    gamma_nuc_inv,                              &
    gamma_acc_inv,                              &
    gamma_nucm_inv,                             &
    gamma_accm_inv,                             &
    gamma_orgi_inv,                             &
    gamma_orgj_inv,                             &
    gamma_orgim_inv,                            &
    gamma_orgjm_inv,                            &
    org_cont_i,                                 &
    org_cont_j,                                 &
    org_cont_im,                                &
    org_cont_jm,                                &
    inorg_cont_i,                               &
    inorg_cont_j,                               &
    inorg_cont_im,                              &
    inorg_cont_jm,                              &
    rat_nuc,                                    &
    rat_acc,                                    &
    rat_nucm,                                   &
    rat_accm,                                   &
    l_nuc,                                      &
    l_acc,                                      &
    l_nucm,                                     &
    l_accm,                                     &
    rc_nuc,                                     &
    rc_acc,                                     &
    rc_nucm,                                    &
    rc_accm,                                    &
    pot

 REAL (KIND=ireals)                       ::    &
    v_n2o5(imax*jmax),                          &
    gamma_n2o5(imax*jmax),                      &
    sulfold(ie,je),                             &
    crorg(imax*jmax,22)

 REAL (KIND=ireals), ALLOCATABLE ::            &
    convfac(:),                                &
    s_ges(:),                                  &
    nitrate_in(:),                             &
    nh3_in(:),                                 &
    vsulf_in(:),                               &
    so4rat_in(:),                              &
    ess(:),                                    &
    s_nuc(:),                                  &
    s_acc(:),                                  &
    s_nucm(:),                                 &
    s_accm(:),                                 &
    tsoot(:),                                  &
    tsoot_coag(:)

 REAL (KIND=ireals),  ALLOCATABLE ::                  &
    condvap_in(:,:),                           &
    drog_in(:,:),                              &
    vdrog(:,:)

   ! Tracer pointers:
 REAL (KIND=ireals), POINTER ::                          &
     qv_new       (:,:,:)  => NULL()                      ! QV at nnew


 ALLOCATE( convfac(imax*jmax), s_ges(imax*jmax),           &
           s_nuc(imax*jmax),                               &
           s_acc(imax*jmax),                               &
           s_nucm(imax*jmax),                              &
           s_accm(imax*jmax),                              &
           nitrate_in(imax*jmax),                          &
           nh3_in(imax*jmax),                              &
           vsulf_in(imax*jmax),                            &
           so4rat_in(imax*jmax),                           &
           drog_in(imax*jmax,ldrog),                       &
           ustar(imax*jmax),                               &
           wstar(imax*jmax),                               &
           raerody(imax*jmax),                             &
           blkprs(imax*jmax),                              &
           blkta(imax*jmax),                               &
           blkrh(imax*jmax),                               &
           ess(imax*jmax),                                 &
           tsoot(imax*jmax),                               &
           tsoot_coag(imax*jmax),                          &
           condvap_in(imax*jmax,ncv),                      &
           vdrog(imax*jmax,ldrog),                         &
           stat=stat_var)


  ! retrieve the required microphysics tracers
   CALL trcr_get(izerror, 'QV', ptr_tlev = nnew, ptr = qv_new)
   IF (izerror /= 0) THEN
     yzerrmsg = trcr_errorstr(izerror)
     CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
   ENDIF

 IF (lgas) THEN
   CALL trcr_get_block(izerror, idx_start=trcr_idx_gas(1), idx_end=trcr_idx_gas(isp_gas), &
                       ptr_tlev = nnew, ptr = cgas_new)
   IF (izerror /= 0) THEN
     yzerrmsg = trcr_errorstr(izerror)
     CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
   ENDIF
 ENDIF



! CK initialize arrays correctly

  convfac(:)      = 0.0_ireals
  s_ges(:)        = 0.0_ireals
  s_nuc(:)        = 0.0_ireals
  s_acc(:)        = 0.0_ireals
  s_nucm(:)       = 0.0_ireals
  s_accm(:)       = 0.0_ireals
  nitrate_in(:)   = 0.0_ireals
  nh3_in(:)       = 0.0_ireals
  vsulf_in(:)     = 0.0_ireals
  so4rat_in(:)    = 0.0_ireals
  drog_in(:,:)    = 0.0_ireals
  ustar(:)        = 0.0_ireals
  wstar(:)        = 0.0_ireals
  raerody(:)      = 0.0_ireals
  blkprs(:)       = 0.0_ireals
  blkta(:)        = 0.0_ireals
  blkrh(:)        = 0.0_ireals
  ess(:)          = 0.0_ireals
  tsoot(:)        = 0.0_ireals
  tsoot_coag(:)   = 0.0_ireals
  condvap_in(:,:) = 0.0_ireals
  vdrog(:,:)      = 0.0_ireals

   
   wstern(:,:) = 1._ireals

 IF (lgas) THEN
    DO l = 1,ldrog
      DO j = 1,jmax*imax
        vdrog(j,l) = 0._ireals
      ENDDO
    ENDDO

     CALL get_timings (i_art_vorher, ntstep, dt, izerror)
    DO jj = jstart,jend
      DO ii = istart,iend
        sulfold(ii,jj) = cgas_new(ii,jj,kkz,lsulf)
        CALL get_data_for_kpp(ii, jj, kkz)
        CALL kpp_step(0.0d0, dtmod, kpp_temp, kpp_m, kpp_photo, kpp_het, kpp_conc)
        CALL set_conc_from_kpp(ii, jj, kkz)
      END DO
    END DO


   DO j = 1,(iend-nboundlines)*(jend-nboundlines)
     jj = j+nboundlines-(INT((j-1)/(jend-nboundlines))*(jend-nboundlines))
     ii = INT((j-1)/(jend-nboundlines))+jstart
     pot     = (pp(ii,jj,kkz,nnew)+p0(ii,jj,kkz)) / t(ii,jj,kkz,nnew)/101300._ireals

     !crk     = rk                                           * const(2)    *pot(j)*cgas                             *cgas
     crorg(j,1)  = 1.89e-11_ireals*EXP(116._ireals/t(ii,jj,kkz,nnew))                                      &
                   *4.4e17_ireals*pot*cgas_new(ii,jj,kkz,lxyl)*cgas_new(ii,jj,kkz,lho)
     crorg(j,2)  = 2.10e-12_ireals*EXP(332._ireals/t(ii,jj,kkz,nnew))                                      &
                   *4.4e17_ireals*pot*cgas_new(ii,jj,kkz,ltol)*cgas_new(ii,jj,kkz,lho)
     crorg(j,3)  = 4.00e-11_ireals*4.4e17_ireals*pot*cgas_new(ii,jj,kkz,lcsl)*cgas_new(ii,jj,kkz,lho)
     crorg(j,4)  = 2.20e-11_ireals*4.4e17_ireals*pot*cgas_new(ii,jj,kkz,lcsl)*cgas_new(ii,jj,kkz,lno3)
     crorg(j,5)  = 3.64e-11_ireals*EXP(-380._ireals/t(ii,jj,kkz,nnew))                                     &
                   *4.4e17_ireals*pot*cgas_new(ii,jj,kkz,lhc8)*cgas_new(ii,jj,kkz,lho)
     crorg(j,6)  = 1.07e-11_ireals*EXP(549._ireals/t(ii,jj,kkz,nnew))                                      &
                   *4.4e17_ireals*pot*cgas_new(ii,jj,kkz,loli)*cgas_new(ii,jj,kkz,lho)
     crorg(j,7)  = 3.23e-11_ireals*EXP(-975._ireals/t(ii,jj,kkz,nnew))                                     &
                   *4.4e17_ireals*pot*cgas_new(ii,jj,kkz,loli)*cgas_new(ii,jj,kkz,lno3)
     crorg(j,8)  = 7.29e-15_ireals*EXP(-1136._ireals/t(ii,jj,kkz,nnew))                                    &
                   *4.4e17_ireals*pot*cgas_new(ii,jj,kkz,loli)*cgas_new(ii,jj,kkz,lo3)
     crorg(j,9)  = 5.32e-12_ireals*EXP(504._ireals/t(ii,jj,kkz,nnew))                                      &
                   *4.4e17_ireals*pot*cgas_new(ii,jj,kkz,lolt)*cgas_new(ii,jj,kkz,lho)
     crorg(j,10) = 1.00e-11_ireals*EXP(-1895._ireals/t(ii,jj,kkz,nnew))                                    &
                   *4.4e17_ireals*pot*cgas_new(ii,jj,kkz,lolt)*cgas_new(ii,jj,kkz,lno3)
     crorg(j,11) = 1.32e-14_ireals*EXP(-2105._ireals/t(ii,jj,kkz,nnew))                                    &
                   *4.4e17_ireals*pot*cgas_new(ii,jj,kkz,lolt)*cgas_new(ii,jj,kkz,lo3)
     crorg(j,12) = 1.21e-11_ireals*EXP(444._ireals/t(ii,jj,kkz,nnew))                                      &
                   *4.4e17_ireals*pot*cgas_new(ii,jj,kkz,lapi)*cgas_new(ii,jj,kkz,lho)
     crorg(j,13) = 1.19e-12_ireals*EXP(490._ireals/t(ii,jj,kkz,nnew))                                      &
                   *4.4e17_ireals*pot*cgas_new(ii,jj,kkz,lapi)*cgas_new(ii,jj,kkz,lno3)
     crorg(j,14) = 1.01e-15_ireals*EXP(-736._ireals/t(ii,jj,kkz,nnew))                                     &
                   *4.4e17_ireals*pot*cgas_new(ii,jj,kkz,lapi)*cgas_new(ii,jj,kkz,lo3)
     crorg(j,15) = 1.70e-10_ireals*4.4e17_ireals*pot*cgas_new(ii,jj,kkz,llim)*cgas_new(ii,jj,kkz,lho)
     crorg(j,16) = 1.22e-11_ireals*4.4e17_ireals*pot*cgas_new(ii,jj,kkz,llim)*cgas_new(ii,jj,kkz,lno3)
     crorg(j,17) = 2.00e-16_ireals*4.4e17_ireals*pot*cgas_new(ii,jj,kkz,llim)*cgas_new(ii,jj,kkz,lo3)
     crorg(j,18) = 2.55e-11_ireals*EXP(409._ireals/t(ii,jj,kkz,nnew))                                      &
                   *4.4e17_ireals*pot*cgas_new(ii,jj,kkz,liso)*cgas_new(ii,jj,kkz,lho)                       !eathana
     crorg(j,19) = 1.23e-14_ireals*EXP(-2013._ireals/t(ii,jj,kkz,nnew))                                    &
                   *4.4e17_ireals*pot*cgas_new(ii,jj,kkz,liso)*cgas_new(ii,jj,kkz,lo3)                       !eathana
     crorg(j,20) = 2.50e-12_ireals*4.4e17_ireals*pot*cgas_new(ii,jj,kkz,lcs10)*cgas_new(ii,jj,kkz,lho)       !eathana
     crorg(j,21) = 2.50e-12_ireals*4.4e17_ireals*pot*cgas_new(ii,jj,kkz,lcs100)*cgas_new(ii,jj,kkz,lho)      !eathana
     crorg(j,22) = 2.50e-12_ireals*4.4e17_ireals*pot*cgas_new(ii,jj,kkz,lcs1000)*cgas_new(ii,jj,kkz,lho)     !eathana
   ENDDO
   
     DO j = 1,imax*jmax           !dtc durch dtmod
       vdrog(j,dxyl ) = vdrog(j,dxyl )+crorg(j,1) * dtmod
       vdrog(j,dtol ) = vdrog(j,dtol )+crorg(j,2) * dtmod
       vdrog(j,dcsl1) = vdrog(j,dcsl1)+crorg(j,3) * dtmod
       vdrog(j,dcsl2) = vdrog(j,dcsl2)+crorg(j,4) * dtmod
       vdrog(j,dhc8 ) = vdrog(j,dhc8 )+crorg(j,5) * dtmod
       vdrog(j,doli1) = vdrog(j,doli1)+crorg(j,6) * dtmod
       vdrog(j,doli2) = vdrog(j,doli2)+crorg(j,7) * dtmod
       vdrog(j,doli3) = vdrog(j,doli3)+crorg(j,8) * dtmod
       vdrog(j,dolt1) = vdrog(j,dolt1)+crorg(j,9) * dtmod
       vdrog(j,dolt2) = vdrog(j,dolt2)+crorg(j,10) * dtmod
       vdrog(j,dolt3) = vdrog(j,dolt3)+crorg(j,11) * dtmod
       vdrog(j,dapi1) = vdrog(j,dapi1)+crorg(j,12) * dtmod
       vdrog(j,dapi2) = vdrog(j,dapi2)+crorg(j,13) * dtmod
       vdrog(j,dapi3) = vdrog(j,dapi3)+crorg(j,14) * dtmod
       vdrog(j,dlim1) = vdrog(j,dlim1)+crorg(j,15) * dtmod
       vdrog(j,dlim2) = vdrog(j,dlim2)+crorg(j,16) * dtmod
       vdrog(j,dlim3) = vdrog(j,dlim3)+crorg(j,17) * dtmod
       vdrog(j,diso1) = vdrog(j,diso1)+crorg(j,18) * dtmod !eathana
       vdrog(j,diso2) = vdrog(j,diso2)+crorg(j,19) * dtmod !eathana
       vdrog(j,dcs10) = vdrog(j,dcs10)+crorg(j,20) * dtmod !eathana
       vdrog(j,dcs100)= vdrog(j,dcs100)+crorg(j,21)* dtmod !eathana
       vdrog(j,dcs1000)= vdrog(j,dcs1000)+crorg(j,22)* dtmod !eathana
     ENDDO

 END IF  !   lgas

!**************************

1     CONTINUE

   CALL get_timings (i_art_chemie, ntstep, dt, izerror)

!**************************
! **** switch for aerosols
   IF(laero) THEN
!**************************


!     CALL AEROSOL DYNAMICS ROUTINE AS FUNCTION OF
!     k= loop over model layer
!     DT= TIME STEP IN SECONDS (currently DT=600)


   IF ((ntstep*dt/3600.) >= aerostart ) THEN

!... SET UP METEOROLOGICAL INPUT

     DO j = 1,(iend-nboundlines)*(jend-nboundlines)
       jj = j+nboundlines-(INT((j-1)/(jend-nboundlines))*(jend-nboundlines))
       ii =INT((j-1)/(jend-nboundlines))+jstart

       blkprs(j)  = (pp(ii,jj,kkz,nnew)+p0(ii,jj,kkz))                 ! pressure in Pa
       blkta(j)   = t(ii,jj,kkz,nnew)                  ! temperature in K
       ess(j)     = 6.1_ireals*10**(8.26_ireals*(blkta(j)-273._ireals)/blkta(j))
       blkrh(j)   = blkprs(j)*1.e-02_ireals*qv_new(ii,jj,kkz)/(0.622_ireals*ess(j)) ! rel. humidity
       !CK 20100627 avoid zero RH as it causes a FP exception in ISORROPIA
       blkrh(j)   = MAX(0.01_ireals, blkrh(j))

       IF (kkz == kmax) THEN
         raerody(j) = ra(ii,jj)                  ! aerodynamic resistance [s/m]
         ustar(j)   = ustern(ii,jj)              ! friction velocity      [m/s]
         wstar(j)   = wstern(ii,jj)              ! convective velocity    [m/s]
       END IF

       convfac(j) = blkprs(j)/rgasuniv/blkta(j)  !factor to convert ppm to ug/m*3


 IF (lgas) THEN

!... Calculate sulfate production during the last
!... time step from SO2+OH. Since the chemical time
!... (gas phase) is smaller than the RPM time step
!... this rate has to be calculated from the diff.
!... between old and new concentrations of LSULF.

       so4rat_in(j) = ( cgas_new(ii,jj,kkz,lsulf) - sulfold(ii,jj) ) /dtmod * convfac(j) * mwso4

!... CONVERT GAS PHASE CONCENTRATIONS FROM
!... ppm to ug / m*3 for use in dyn.calcs
!... 1 ug/m*3= ppm * p [hPa] * Mol.Wei./RGAS/TEMP

       nitrate_in(j) = cgas_new(ii,jj,kkz,lhno3) * convfac(j) * mwhno3
       nh3_in(j)     = cgas_new(ii,jj,kkz,lnh3)  * convfac(j) * mwnh3

!ia... vsulf_in has to be the sulfate vapor already updated for the
!ia... actual time-step!!!

       vsulf_in(j)   = cgas_new(ii,jj,kkz,lsulf) * convfac(j) * mwso4

       drog_in(j,pxyl ) = vdrog(j,dxyl )
       drog_in(j,ptol ) = vdrog(j,dtol )
       drog_in(j,pcsl1) = vdrog(j,dcsl1)
       drog_in(j,pcsl2) = vdrog(j,dcsl2)
       drog_in(j,phc8 ) = vdrog(j,dhc8 )
       drog_in(j,poli1) = vdrog(j,doli1)
       drog_in(j,poli2) = vdrog(j,doli2)
       drog_in(j,poli3) = vdrog(j,doli3)
       drog_in(j,polt1) = vdrog(j,dolt1)
       drog_in(j,polt2) = vdrog(j,dolt2)
       drog_in(j,polt3) = vdrog(j,dolt3)
!bs * biogenic organics DeltaROG
       drog_in(j,papi1) = vdrog(j,dapi1)
       drog_in(j,papi2) = vdrog(j,dapi2)
       drog_in(j,papi3) = vdrog(j,dapi3)
       drog_in(j,plim1) = vdrog(j,dlim1)
       drog_in(j,plim2) = vdrog(j,dlim2)
       drog_in(j,plim3) = vdrog(j,dlim3)
       drog_in(j,piso1) = vdrog(j,diso1)  !eathana
       drog_in(j,piso2) = vdrog(j,diso2)  !eathana
       drog_in(j,pcs10) = vdrog(j,dcs10)  !eathana
       drog_in(j,pcs100)= vdrog(j,dcs100) !eathana
      drog_in(j,pcs1000)= vdrog(j,dcs1000)!eathana
!bs
       condvap_in(j,psoa1)    = cgas_new(ii,jj,kkz,lcs1)    * convfac(j) * var_gas(lcs1)%wtm         !eathana
       condvap_in(j,psoa10)   = cgas_new(ii,jj,kkz,lcs10)   * convfac(j) * var_gas(lcs10)%wtm         !eathana
       condvap_in(j,psoa100)  = cgas_new(ii,jj,kkz,lcs100)  * convfac(j) * var_gas(lcs100)%wtm         !eathana
       condvap_in(j,psoa1000) = cgas_new(ii,jj,kkz,lcs1000) * convfac(j) * var_gas(lcs1000)%wtm         !eathana

ENDIF   !lgas

     END DO

!US  write(6,*)'drog',vdrog(25,dapi2), condvap_in(25,psoaaro1),condvap_in(25,psoaaro2)


     CALL rpmmod3(kkz,dtmod,                                           &
                  nitrate_in, nh3_in, vsulf_in, so4rat_in,          &
                  drog_in, condvap_in,                              &
                  tsoot, tsoot_coag                                 &
                  )

     CALL get_timings (i_art_aerosol, ntstep, dt, izerror)

IF (lgas) THEN

     DO j = 1,(iend-nboundlines)*(jend-nboundlines)
       jj = j+nboundlines-(INT((j-1)/(jend-nboundlines))*(jend-nboundlines))
       ii = INT((j-1)/(jend-nboundlines))+jstart

!...and gas-phase concs to VC-Array
       cgas_new(ii,jj,kkz,lnh3)     = cblk(j,vnh3 ) / convfac(j) / mwnh3
       cgas_new(ii,jj,kkz,lhno3)    = cblk(j,vhno3) / convfac(j) / mwhno3
       cgas_new(ii,jj,kkz,lsulf)    = cblk(j,vsulf) / convfac(j) / mwso4
       cgas_new(ii,jj,kkz,lcs1)     = cblk(j,vcv1)    / convfac(j) / var_gas(lcs1)%wtm                  !eathana
       cgas_new(ii,jj,kkz,lcs10)    = cblk(j,vcv10)   / convfac(j) / var_gas(lcs10)%wtm                  !eathana
       cgas_new(ii,jj,kkz,lcs100)   = cblk(j,vcv100)  / convfac(j) / var_gas(lcs100)%wtm                  !eathana
       cgas_new(ii,jj,kkz,lcs1000)  = cblk(j,vcv1000) / convfac(j) / var_gas(lcs1000)%wtm

!...ensure non-zero concentrations
       cgas_new(ii,jj,kkz,lnh3)    = MAX( 1.e-19_ireals, cgas_new(ii,jj,kkz,lnh3) )
       cgas_new(ii,jj,kkz,lhno3)   = MAX( 1.e-19_ireals, cgas_new(ii,jj,kkz,lhno3) )
       cgas_new(ii,jj,kkz,lsulf)   = MAX( 1.e-19_ireals, cgas_new(ii,jj,kkz,lsulf) )
       cgas_new(ii,jj,kkz,lcs1)    = max( 1.e-19_ireals, cgas_new(ii,jj,kkz,lcs1)    )              !eathana
       cgas_new(ii,jj,kkz,lcs10)   = max( 1.e-19_ireals, cgas_new(ii,jj,kkz,lcs10)   )              !eathana
       cgas_new(ii,jj,kkz,lcs100)  = max( 1.e-19_ireals, cgas_new(ii,jj,kkz,lcs100)  )              !eathana
       cgas_new(ii,jj,kkz,lcs1000) = max( 1.e-19_ireals, cgas_new(ii,jj,kkz,lcs1000) )

     ENDDO


! *** calculataion of reaction rate for hydrolysis of N2O5

      DO j = 1,(iend-nboundlines)*(jend-nboundlines)
        jj = j+nboundlines-(INT((j-1)/(jend-nboundlines))*(jend-nboundlines))
        ii = INT((j-1)/(jend-nboundlines))+jstart

! *** s in m2/m3
        s_nuc(j)  =  pi * cblk(j,vnu0)*(dgnuc(J))**2*en1**16
        s_acc(j)  =  pi * cblk(j,vac0)*(dgacc(J))**2*ea1**16
        s_nucm(j) =  pi * cblk(j,vnu0m)*(dgnuc(J))**2*en1**16
        s_accm(j) =  pi * cblk(j,vac0m)*(dgacc(J))**2*ea1**16
        s_ges(j)  =  s_nuc(j) + s_acc(j) + s_nucm(j) + s_accm(j)

! *** v_n2o5 in m/s
        v_n2o5(j)=SQRT(3*rgasuniv*blkta(j)/(mwn2o5*1.d-03))

! *** weighting factor for nitrate effect
        f_nuc  = cblk(j,vso4ai)/                     &
                (cblk(j,vso4ai)+cblk(j,vno3ai))
        f_acc  = cblk(j,vso4aj)/                     &
                (cblk(j,vso4aj)+cblk(j,vno3aj))
        f_nucm = cblk(j,vso4aim)/                   &
                (cblk(j,vso4aim)+cblk(j,vno3aim))
        f_accm = cblk(j,vso4ajm)/                   &
                (cblk(j,vso4ajm)+cblk(j,vno3ajm))


!       F = (cblk(j,vso4aj)+CBLK(J,vso4ai)           &
!              +cblk(j,vso4ajm)+CBLK(J,vso4aim))/    &
!           (cblk(j,vso4aj)+cblk(j,vso4ai)           &
!               +cblk(j,vno3aj)+cblk(j,vno3ai)+      &
!            cblk(j,vso4ajm)+cblk(j,vso4aim)+        &
!              cblk(j,vno3ajm)+cblk(j,vno3aim))
!
! *** weigthing of inorganic gammas depending on NO3 content
        gamma_in_nuc  = f_nuc * 0.02_ireals + (1 - f_nuc) * 0.002_ireals
        gamma_in_acc  = f_acc * 0.02_ireals + (1 - f_acc) * 0.002_ireals
        gamma_in_nucm = f_nucm * 0.02_ireals + (1 - f_nucm) * 0.002_ireals
        gamma_in_accm = f_accm * 0.02_ireals + (1 - f_accm) * 0.002_ireals

! *** calculate volume of SOA in individual modes
       org_cont_i  = cblk(j,vorg1i) + cblk(j,vorg10i) +          &      !eathana
                      cblk(j,vorg100i) + cblk(j,vorg1000i)               !eathana
        org_cont_i  = org_cont_i / rhoorg
        org_cont_j  = cblk(j,vorg1j)+cblk(j,vorg10j) +            &      !eathana
                      cblk(j,vorg100j)+ cblk(j,vorg1000j)                !eathana
        org_cont_j  = org_cont_j/rhoorg
        org_cont_im = cblk(j,vorg1im)+cblk(j,vorg10im) +          &      !eathana
                      cblk(j,vorg100im)+ cblk(j,vorg1000im)              !eathana
        org_cont_im = org_cont_im/rhoorg
        org_cont_jm = cblk(j,vorg1jm)+ cblk(j,vorg10jm) +         &      !eathana
                      cblk(j,vorg100jm)+ cblk(j,vorg1000jm) 
        org_cont_jm = org_cont_jm / rhoorg

! *** calculate volume of inorganic substances in individual modes
        inorg_cont_i  = cblk(j,vso4ai)/rhoso4                             &
                        + cblk(j,vno3ai)/rhono3 +                         &
                        cblk(j,vnh4ai)/rhonh4 + cblk(j,vh2oai)/rhoh2o
        inorg_cont_j  =  cblk(j,vso4aj)/rhoso4                            &
                        + cblk(j,vno3aj)/rhono3 +                         &
                        cblk(j,vnh4aj)/rhonh4 + cblk(j,vh2oaj)/rhoh2o
        inorg_cont_im = cblk(j,vso4aim)/rhoso4                            &
                        + cblk(j,vno3aim)/rhono3 +                        &
                        cblk(j,vnh4aim)/rhonh4 + cblk(j,vh2oaim)/rhoh2o   &
                        + cblk(j,vsooti)/rhosoot
        inorg_cont_jm = cblk(j,vso4ajm)/rhoso4                            &
                        + cblk(j,vno3ajm)/rhono3 +                        &
                        cblk(j,vnh4ajm)/rhonh4 + cblk(j,vh2oajm)/rhoh2o   &
                        + cblk(j,vsootj)/rhosoot

! *** calculate ratio of inorganic core (including water) and total volume
        rat_nuc  = inorg_cont_i                             &
                   / (org_cont_i+inorg_cont_i)
        rat_acc  = inorg_cont_j                             &
                   / (org_cont_j+inorg_cont_j)
        rat_nucM = inorg_cont_im                            &
                   / (org_cont_im+inorg_cont_im)
        rat_accm = inorg_cont_jm                            &
                   / (org_cont_jm+inorg_cont_jm)

! *** calculate thickness of organic layer
! *** taking the mean diameter of the surface distribution
        l_nuc  = dgnuc(j)  * esn16/2._ireals * (1-(rat_nuc)**(1._ireals/3._ireals))
        l_acc  = dgacc(j)  * esa16/2._ireals * (1-(rat_acc)**(1._ireals/3._ireals))
        l_nucm = dgnucm(j) * esn16/2._ireals * (1-(rat_nucm)**(1._ireals/3._ireals))
        l_accm = dgaccm(j) * esa16/2._ireals * (1-(rat_accm)**(1._ireals/3._ireals))

! *** calculate radius of inorganic core: radius of particle minus organic layer
! *** thickness
        rc_nuc  = dgnuc(j) * esn16/2._ireals  - l_nuc
        rc_acc  = dgacc(j) * esn16/2._ireals  - l_acc
        rc_nucm = dgnucm(j) * esn16/2._ireals - l_nucm
        rc_accm = dgaccm(j) * esn16/2._ireals - l_accm
        t_in = t(ii,jj,kkz,nnew)

        call gamma_calc (rc_nuc,l_nuc,v_n2o5(j),t_in,gamma_orgi_inv)
        call gamma_calc (rc_acc,l_acc,v_n2o5(j),t_in,gamma_orgj_inv)
        call gamma_calc (rc_nucm,l_nucm,v_n2o5(j),t_in,gamma_orgim_inv)
        call gamma_calc (rc_accm,l_accm,v_n2o5(j),t_in,gamma_orgjm_inv)

         gamma_nuc_inv = 1._ireals/gamma_in_nuc + gamma_orgi_inv
         gamma_acc_inv = 1._ireals/gamma_in_acc  + gamma_orgj_inv
         gamma_nucm_inv = 1._ireals/gamma_in_nucm + gamma_orgim_inv
         gamma_accm_inv = 1._ireals/gamma_in_accm + gamma_orgjm_inv

!        gamma_nuc_inv  = 1._ireals/gamma_in_nuc
!        gamma_acc_inv  = 1._ireals/gamma_in_acc
!        gamma_nucm_inv = 1._ireals/gamma_in_nucm
!        gamma_accm_inv = 1._ireals/gamma_in_accm

         gamma_nuc  = 1._ireals/gamma_nuc_inv
         gamma_acc  = 1._ireals/gamma_acc_inv
         gamma_nucm = 1._ireals/gamma_nucm_inv
         gamma_accm = 1._ireals/gamma_accm_inv

         gamma_n2o5(j) = gamma_nuc * s_nuc(j)/s_ges(j) +              &
                         gamma_acc * s_acc(j)/s_ges(j) +              &
                         gamma_nucm * s_nucm(j)/s_ges(j) +            &
                         gamma_accm * s_accm(j)/s_ges(j)
!
   END DO

   DO j = 1,(iend-nboundlines)*(jend-nboundlines)
     jj = j+nboundlines-(INT((j-1)/(jend-nboundlines))*(jend-nboundlines))
     ii = INT((j-1)/(jend-nboundlines))+jstart
     k_het(ii,jj,kkz) = 0._ireals
     IF(cblk(j,vh2oaj) /= 1.e-30_ireals) THEN
        k_het(ii,jj,kkz) = 0.25_ireals * v_n2o5(j) * gamma_n2o5(j) * s_ges(j) * 60._ireals
     ENDIF

   END DO


  END IF ! End of lgas
 END IF ! switch aerosols

!hhh**********************************************************************


END IF   ! laero


     DEALLOCATE (   convfac,s_ges,                                   &
                    s_nuc,s_acc,s_nucm,s_accm,                       &
                    nitrate_in,nh3_in,vsulf_in,so4rat_in,            &
                    drog_in,vdrog,condvap_in,                        &
                    ustar, wstar,raerody,                            &
                    blkprs,blkta,blkrh,ess,                          &
                    tsoot, tsoot_coag)

END SUBROUTINE org_chem




! //////////////////////////////////////////////////////////////////////////
!//////////////////////////////////////////////////////////////////////////





SUBROUTINE gamma_calc(r_c, ell, c_ave, t, gamma_inv)

! *** calculates term 4 in equation 11 in Anttila et al., JPC 2006
! *** 1/Gamma_coat
!
   INTEGER (KIND=iintegers)                 ::         &
      i,j

   REAL (KIND=ireals)                       ::         &
      r_p, r_c, ell, c_ave,                            &
      t, q,                                            &
      q_aq,                                            &
      gamma, gamma_inv,                                &
      term4
   REAL (KIND=ireals),PARAMETER             ::              &
      m_w = 108.e-3,                & !kg/mol
      r_gas = 8.314510,             & !J/(K*mol)
!n * Standard
!     h_org = 5000./101300.,        &
!n * Sensitivity
      h_org = 500./101300.,         &
      h_aq = 5000./101300.,         &
      d_aq = 1.e-9,                 & !m2/s
!n * Standard
!     d_org = 1.e-9,                &
!n * Sensitivity
      d_org = 1.e-10,               &
      d_gas = 1.e-5,                & !m2/s
      alpha = 0.04,                 &
      k_aq = 5.e6                     ! s-1

      r_p = r_c  +  ell

      term4 = c_ave * ell * R_p /                   &
              (4e0 * R_gas * T * H_org * D_org * R_c)

      gamma_inv =  term4

END SUBROUTINE gamma_calc
! *****************************************************************************

SUBROUTINE get_data_for_kpp(ind_i, ind_j, ind_k)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Get concentrations of the species needed for KPP !!
!! and store them in the array "kpp_conc".          !!
!! This subroutine needs to be adjusted to the      !!
!! chemistry-model!                                 !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  INTEGER (KIND=iintegers),  intent(in) :: ind_i, ind_j, ind_k
  INTEGER (KIND=iintegers)              :: lauf

   ! Tracer pointers:
   REAL (KIND=ireals), POINTER ::                        &
     qv_new       (:,:,:)  => NULL()                      ! QV at nnew

  ! retrieve the required microphysics tracers
   CALL trcr_get(izerror, 'QV', ptr_tlev = nnew, ptr = qv_new)
   IF (izerror /= 0) THEN
     yzerrmsg = trcr_errorstr(izerror)
     CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
   ENDIF

   CALL trcr_get_block(izerror, idx_start=trcr_idx_gas(1), idx_end=trcr_idx_gas(isp_gas), ptr_tlev = nnew, ptr = cgas_new)
   IF (izerror /= 0) THEN
     yzerrmsg = trcr_errorstr(izerror)
     CALL model_abort(my_cart_id, izerror, yzerrmsg, yzroutine)
   ENDIF

  ! temperature
  kpp_temp = t(ind_i, ind_j, ind_k, nnew)
  !! Zeitscheibe nnew richtig? kpp_temp = NaN ab STEP 1 !!!!!

#ifdef DEBUG_KPP
  write(123,*) 'my_cart_id =', my_cart_id, 'i =', ind_i, 'j =', ind_j, &
  'k =', ind_k, 'nnew =', nnew, 't(...) =', t(ind_i,ind_j,ind_k,nnew)
#endif

  ! collision parameter
  kpp_m = ( pp(ind_i, ind_j, ind_k, nnew) + p0(ind_i, ind_j, ind_k) ) * &
            avo / (kpp_temp*rgasuniv*1.e6_ireals)

  ! photolysis rates
  DO lauf = 1,jphot
    kpp_photo(lauf) = mjval(ind_i,ind_j,lauf,ind_k) !! 1/s
  END DO
  
  ! heterogeneous rate
  kpp_het(1)=k_het(ind_i, ind_j, ind_k)/60._ireals  !! 1/s

  ! concentrations
  DO lauf = 1,isp_gas
    kpp_conc(lauf) = cgas_new(ind_i,ind_j,ind_k,lauf) !! ppm
  END DO

! DO lauf = 16,52          !vorher: 48
!   kpp_conc(lauf-1) = cgas_new(ind_i,ind_j,ind_k,lauf) !! ppm
! END DO

!   kpp_conc(52) = cgas_new(ind_i,ind_j,ind_k,ldms) !! ppm   !DMS
!   kpp_conc(53) = cgas_new(ind_i,ind_j,ind_k,ldmso) !! ppm   !DMSO
!   kpp_conc(54) = cgas_new(ind_i,ind_j,ind_k,lho) !! ppm   !vorher: 48
!   kpp_conc(55) = cgas_new(ind_i,ind_j,ind_k,lho2) !! ppm   !vorher: 49

  DO lauf = 1,21
!   kpp_conc(lauf+55) = cradicals(ind_i,ind_j,ind_k,lauf,nnew) !! radicals !vorher: 49
    kpp_conc(lauf+isp_gas) = cradicals(ind_i,ind_j,ind_k,lauf) !! radicals !vorher: 49
  END DO

  kpp_conc(82) = qv_new(ind_i,ind_j,ind_k)*1.604e6_ireals !! ppm    !vorher: 75

!  kpp_conc(83) = 0.0 !! Dummy    !vorher: 77

END SUBROUTINE get_data_for_kpp
! *****************************************************************************

SUBROUTINE set_conc_from_kpp(ind_i, ind_j, ind_k)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Store the calculated concentrations from KPP.  !!
!! This subroutine needs to be adjusted to the    !!
!! chemistry-model!                               !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  implicit none

  INTEGER (KIND=iintegers),  intent(in) :: ind_i, ind_j, ind_k
  INTEGER (KIND=iintegers)              :: lauf

  ! concentrations
  DO lauf =1,isp_gas
    cgas_new(ind_i,ind_j,ind_k,lauf) = kpp_conc(lauf) !! ppm
  END DO

! DO lauf = 16,52          !vorher: 48
!   cgas_new(ind_i,ind_j,ind_k,lauf) = kpp_conc(lauf-1) !! ppm
! END DO

! cgas_new(ind_i,ind_j,ind_k,ldms) = kpp_conc(52)   !DMS
! cgas_new(ind_i,ind_j,ind_k,ldmso) = kpp_conc(53)   !DMSO
! cgas_new(ind_i,ind_j,ind_k,lho) = kpp_conc(54) !! ppm  !vorher: 48
! cgas_new(ind_i,ind_j,ind_k,lho2) = kpp_conc(55) !! ppm  !vorher: 49

  DO lauf = 1,21
    cradicals(ind_i,ind_j,ind_k,lauf) = kpp_conc(lauf+isp_gas) !! radicals    !vorher: 49
  END DO

END SUBROUTINE set_conc_from_kpp
! *****************************************************************************
END MODULE art_chemkpp
