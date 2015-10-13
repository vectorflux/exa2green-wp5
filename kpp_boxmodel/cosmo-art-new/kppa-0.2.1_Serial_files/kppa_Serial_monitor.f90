!-------------------- BEGIN kppa_Serial_monitor.f90 BEGIN --------------------
! @file kppa_Serial_monitor.f90                                               
! @author charlesj                                                            
! @date 2015-02-10 15:56:51.121614                                            
! @brief Mechanism monitoring                                                 
!                                                                             
! Species names, equations, etc.                                              
!                                                                             
! This file was generated by Kppa: http://www.paratools.com/Kppa              
!-----------------------------------------------------------------------------


MODULE kppa_Serial_monitor

  USE kppa_Serial_parameters

  IMPLICIT NONE



!-----------------------------------------------------------------------------
! Array dimensions                                                            
!-----------------------------------------------------------------------------

  ! Number of species on the LOOKAT list 
  INTEGER, PARAMETER :: NLOOKAT = 0

  ! Number of species on the MONITOR list 
  INTEGER, PARAMETER :: NMONITOR = 0

!-----------------------------------------------------------------------------
! Species indices arrays                                                      
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Mechanism information                                                       
!-----------------------------------------------------------------------------

  ! Species names 
  CHARACTER(LEN=6), PARAMETER :: SPC_NAMES(NSPEC) = (/ &
      'NH3   ', &
      'N2    ', &
      'SULF  ', &
      'ORA1  ', &
      'ORA2  ', &
      'CS1   ', &
      'CO2   ', &
      'H2    ', &
      'ETH   ', &
      'ISHP  ', &
      'HC5   ', &
      'TPAN  ', &
      'HONO  ', &
      'DMSO  ', &
      'HC8   ', &
      'SO2   ', &
      'N2O5  ', &
      'MAHP  ', &
      'DMS   ', &
      'TOL   ', &
      'XYL   ', &
      'HC3   ', &
      'NALD  ', &
      'CS10  ', &
      'CS100 ', &
      'CS1000', &
      'O1D   ', &
      'PAA   ', &
      'MPAN  ', &
      'OP1   ', &
      'CH4   ', &
      'CSL   ', &
      'HACE  ', &
      'HNO4  ', &
      'PAN   ', &
      'O3P   ', &
      'H2O2  ', &
      'OL2   ', &
      'ISOP  ', &
      'ISON  ', &
      'O2    ', &
      'HNO3  ', &
      'API   ', &
      'CO    ', &
      'LIM   ', &
      'OLT   ', &
      'ISO   ', &
      'XNO2  ', &
      'GLY   ', &
      'XYLP  ', &
      'MACR  ', &
      'DCB   ', &
      'HC5P  ', &
      'HC8P  ', &
      'OLIP  ', &
      'KET   ', &
      'OL2P  ', &
      'OLTP  ', &
      'XO2   ', &
      'TOLP  ', &
      'OP2   ', &
      'MACP  ', &
      'OLN   ', &
      'MGLY  ', &
      'HCHO  ', &
      'TCO3  ', &
      'H2O   ', &
      'LIMP  ', &
      'APIP  ', &
      'OLI   ', &
      'KETP  ', &
      'ALD   ', &
      'ETHP  ', &
      'MO2   ', &
      'NO2   ', &
      'NO3   ', &
      'HO2   ', &
      'NO    ', &
      'O3    ', &
      'HO    ', &
      'HC3P  ', &
      'ACO3  ', &
      'ONIT  '  /)

  ! Equations 
  CHARACTER(LEN=111), PARAMETER :: EQN_NAMES(NREACT) = (/ &
      'NO2 --> O3P + NO                                         &
      &                                                      ', &
      'O3 --> O1D + O2                                          &
      &                                                      ', &
      'O3 --> O3P + O2                                          &
      &                                                      ', &
      'HONO --> HO + NO                                         &
      &                                                      ', &
      'HNO3 --> HO + NO2                                        &
      &                                                      ', &
      'HNO4 --> HO2 + NO2                                       &
      &                                                      ', &
      'NO3 --> NO + O2                                          &
      &                                                      ', &
      'NO3 --> NO2 + O3P                                        &
      &                                                      ', &
      'H2O2 --> 2 HO                                            &
      &                                                      ', &
      'HCHO --> H2 + CO                                         &
      &                                                      ', &
      'HCHO --> 2 HO2 + CO                                      &
      &                                                      ', &
      'ALD --> MO2 + HO2 + CO                                   &
      &                                                      ', &
      'OP1 --> HCHO + HO2 + HO                                  &
      &                                                      ', &
      'OP2 --> ALD + HO2 + HO                                   &
      &                                                      ', &
      'PAA --> MO2 + CO2 + HO                                   &
      &                                                      ', &
      'KET --> ACO3 + ETHP                                      &
      &                                                      ', &
      'GLY --> HCHO + 1.87 CO                                   &
      &                                                      ', &
      'GLY --> HCHO + 1.55 CO + HO2                             &
      &                                                      ', &
      'MGLY --> ACO3 + HO2 + CO                                 &
      &                                                      ', &
      'DCB --> HO2 + TCO3 + ACO3                                &
      &                                                      ', &
      'ONIT --> ALD + KET + HO2 + NO2                           &
      &                                                      ', &
      'O3P + O2 --> O3                                          &
      &                                                      ', &
      'O3P + NO2 --> NO + O2                                    &
      &                                                      ', &
      'O1D + N2 --> O3P + N2                                    &
      &                                                      ', &
      'O1D + O2 --> O3P + O2                                    &
      &                                                      ', &
      'O1D + H2O --> 2 HO                                       &
      &                                                      ', &
      'O3 + NO --> NO2 + O2                                     &
      &                                                      ', &
      'O3 + HO --> HO2 + O2                                     &
      &                                                      ', &
      'O3 + HO2 --> HO + 2 O2                                   &
      &                                                      ', &
      'HO2 + NO --> NO2 + HO                                    &
      &                                                      ', &
      'HO2 + NO2 --> HNO4                                       &
      &                                                      ', &
      'HO2 + HO2 --> H2O2                                       &
      &                                                      ', &
      'HO2 + HO2 + H2O --> H2O2 + H2O                           &
      &                                                      ', &
      'H2O2 + HO --> HO2 + H2O                                  &
      &                                                      ', &
      'NO + HO --> HONO                                         &
      &                                                      ', &
      'NO + NO + O2 --> NO2 + NO2                               &
      &                                                      ', &
      'O3 + NO2 --> NO3                                         &
      &                                                      ', &
      'NO3 + NO --> NO2 + NO2                                   &
      &                                                      ', &
      'NO3 + NO2 --> NO + NO2 + O2                              &
      &                                                      ', &
      'NO3 + HO2 --> HNO3 + O2                                  &
      &                                                      ', &
      'NO3 + NO2 --> N2O5                                       &
      &                                                      ', &
      'N2O5 --> NO2 + NO3                                       &
      &                                                      ', &
      'N2O5 --> 2 HNO3                                          &
      &                                                      ', &
      'HO + NO2 --> HNO3                                        &
      &                                                      ', &
      'HO + HNO3 --> NO3 + H2O                                  &
      &                                                      ', &
      'HO + HNO4 --> NO2 + H2O + O2                             &
      &                                                      ', &
      'HO + HO2 --> H2O + O2                                    &
      &                                                      ', &
      'HO + SO2 --> SULF + HO2                                  &
      &                                                      ', &
      'CO + HO --> HO2                                          &
      &                                                      ', &
      'CH4 + HO --> MO2 + H2O                                   &
      &                                                      ', &
      'ETH + HO --> ETHP + H2O                                  &
      &                                                      ', &
      'HC3 + HO --> HC3P + HO2 + HCHO + ALD + KET + H2O         &
      &                                                      ', &
      'HC5 + HO --> HC5P + XO2 + H2O                            &
      &                                                      ', &
      'HC8 + HO --> HC8P + XO2 + H2O + CS10                     &
      &                                                      ', &
      'OL2 + HO --> OL2P                                        &
      &                                                      ', &
      'OLT + HO --> OLTP + CS1 + CS10 + CS100 + CS1000          &
      &                                                      ', &
      'OLI + HO --> OLIP + CS1 + CS10 + CS100 + CS1000          &
      &                                                      ', &
      'TOL + HO --> TOLP + CSL + HO2 + CS1 + CS10 + CS100 + CS10&
      &00                                                    ', &
      'XYL + HO --> XYLP + CSL + HO2 + CS1 + CS10 + CS100 + CS10&
      &00                                                    ', &
      'CSL + HO --> HO2 + XO2 + TCO3 - HO                       &
      &                                                      ', &
      'HCHO + HO --> HO2 + CO + H2O                             &
      &                                                      ', &
      'ALD + HO --> ACO3 + H2O                                  &
      &                                                      ', &
      'KET + HO --> KETP + H2O                                  &
      &                                                      ', &
      'GLY + HO --> HO2 + 2 CO + H2O                            &
      &                                                      ', &
      'MGLY + HO --> ACO3 + CO + H2O                            &
      &                                                      ', &
      'DCB + HO --> TCO3 + H2O                                  &
      &                                                      ', &
      'OP1 + HO --> MO2 + HCHO + HO                             &
      &                                                      ', &
      'OP2 + HO --> HC3P + ALD + HO                             &
      &                                                      ', &
      'PAA + HO --> ACO3 + H2O                                  &
      &                                                      ', &
      'PAN + HO --> HCHO + NO3 + XO2                            &
      &                                                      ', &
      'ONIT + HO --> HC3P + NO2                                 &
      &                                                      ', &
      'ISO + HO --> ISOP + CS1 + CS10 + CS100                   &
      &                                                      ', &
      'ACO3 + NO2 --> PAN                                       &
      &                                                      ', &
      'PAN --> ACO3 + NO2                                       &
      &                                                      ', &
      'TCO3 + NO2 --> TPAN                                      &
      &                                                      ', &
      'TPAN --> TCO3 + NO2                                      &
      &                                                      ', &
      'MO2 + NO --> HCHO + HO2 + NO2                            &
      &                                                      ', &
      'HC3P + NO --> ALD + KET + HCHO + ONIT + NO2 + HO2        &
      &                                                      ', &
      'HC5P + NO --> ALD + KET + ONIT + NO2 + HO2               &
      &                                                      ', &
      'HC8P + NO --> ALD + 1.06 KET + HCHO + ONIT + NO2 + HO2   &
      &                                                      ', &
      'OL2P + NO --> 1.6 HCHO + HO2 + NO2 + ALD                 &
      &                                                      ', &
      'OLTP + NO --> ALD + HCHO + HO2 + NO2                     &
      &                                                      ', &
      'OLIP + NO --> HO2 + 1.45 ALD + HCHO + KET + NO2          &
      &                                                      ', &
      'ACO3 + NO --> MO2 + NO2                                  &
      &                                                      ', &
      'TCO3 + NO --> NO2 + HO2 + GLY + MGLY + ACO3 + CO + 2 XO2 &
      &                                                      ', &
      'TOLP + NO --> NO2 + HO2 + MGLY + GLY + DCB               &
      &                                                      ', &
      'XYLP + NO --> NO2 + HO2 + MGLY + DCB                     &
      &                                                      ', &
      'ETHP + NO --> ALD + HO2 + NO2                            &
      &                                                      ', &
      'KETP + NO --> MGLY + NO2 + HO2                           &
      &                                                      ', &
      'OLN + NO --> HCHO + ALD + 2 NO2                          &
      &                                                      ', &
      'HCHO + NO3 --> HO2 + HNO3 + CO                           &
      &                                                      ', &
      'ALD + NO3 --> ACO3 + HNO3                                &
      &                                                      ', &
      'GLY + NO3 --> HNO3 + HO2 + 2 CO                          &
      &                                                      ', &
      'MGLY + NO3 --> HNO3 + ACO3 + CO                          &
      &                                                      ', &
      'DCB + NO3 --> HNO3 + TCO3                                &
      &                                                      ', &
      'CSL + NO3 --> HNO3 + XNO2 + CSL                          &
      &                                                      ', &
      'OL2 + NO3 --> OLN                                        &
      &                                                      ', &
      'OLT + NO3 --> OLN + CS1 + CS10 + CS100 + CS1000          &
      &                                                      ', &
      'OLI + NO3 --> OLN + CS1 + CS10 + CS100 + CS1000          &
      &                                                      ', &
      'ISO + NO3 --> ISON                                       &
      &                                                      ', &
      'OL2 + O3 --> HCHO + CO + ORA1 + HO2                      &
      &                                                      ', &
      'OLT + O3 --> HCHO + ALD + CO + ORA1 + ORA2 + HO2 + MO2 + &
      &HO + CH4 + CS1 + CS10 + CS100 + CS1000                ', &
      'OLI + O3 --> HCHO + ALD + KET + CO + ORA1 + ORA2 + CH4 + &
      &HO2 + HO + MO2 + CS1 + CS10 + CS100 + CS1000          ', &
      'ISO + O3 --> HCHO + MACR + CO + ORA1 + MACP + ACO3 + H2O2&
      & + HO2 + MO2 + HO + CS1 + CS10 + CS100                ', &
      'HO2 + MO2 --> OP1                                        &
      &                                                      ', &
      'HO2 + ETHP --> OP2                                       &
      &                                                      ', &
      'HO2 + HC3P --> OP2                                       &
      &                                                      ', &
      'HO2 + HC5P --> OP2                                       &
      &                                                      ', &
      'HO2 + HC8P --> OP2                                       &
      &                                                      ', &
      'HO2 + OL2P --> OP2                                       &
      &                                                      ', &
      'HO2 + OLTP --> OP2                                       &
      &                                                      ', &
      'HO2 + OLIP --> OP2                                       &
      &                                                      ', &
      'HO2 + KETP --> OP2                                       &
      &                                                      ', &
      'HO2 + ACO3 --> PAA                                       &
      &                                                      ', &
      'HO2 + TOLP --> OP2                                       &
      &                                                      ', &
      'HO2 + XYLP --> OP2                                       &
      &                                                      ', &
      'HO2 + TCO3 --> OP2                                       &
      &                                                      ', &
      'HO2 + OLN --> ONIT                                       &
      &                                                      ', &
      'MO2 + MO2 --> 1.5 HCHO + HO2                             &
      &                                                      ', &
      'MO2 + ETHP --> HCHO + HO2 + ALD                          &
      &                                                      ', &
      'MO2 + HC3P --> HCHO + HO2 + ALD + KET                    &
      &                                                      ', &
      'MO2 + HC5P --> HCHO + HO2 + ALD + KET                    &
      &                                                      ', &
      'MO2 + HC8P --> HCHO + HO2 + ALD + 1.39 KET               &
      &                                                      ', &
      'MO2 + OL2P --> 1.55 HCHO + HO2 + ALD                     &
      &                                                      ', &
      'MO2 + OLTP --> 1.25 HCHO + HO2 + ALD                     &
      &                                                      ', &
      'MO2 + OLIP --> HCHO + HO2 + ALD + KET                    &
      &                                                      ', &
      'MO2 + KETP --> HCHO + HO2 + MGLY                         &
      &                                                      ', &
      'MO2 + ACO3 --> HCHO + HO2 + MO2 + ORA2                   &
      &                                                      ', &
      'MO2 + TOLP --> HCHO + 2 HO2 + MGLY + GLY + DCB           &
      &                                                      ', &
      'MO2 + XYLP --> HCHO + 2 HO2 + MGLY + DCB                 &
      &                                                      ', &
      'MO2 + TCO3 --> HCHO + ORA2 + HO2 + GLY + MGLY + ACO3 + CO&
      & + XO2                                                ', &
      'ETHP + ACO3 --> ALD + HO2 + MO2 + ORA2                   &
      &                                                      ', &
      'HC3P + ACO3 --> ALD + KET + HO2 + MO2 + ORA2             &
      &                                                      ', &
      'HC5P + ACO3 --> ALD + KET + HO2 + MO2 + ORA2             &
      &                                                      ', &
      'HC8P + ACO3 --> ALD + 1.39 KET + HO2 + MO2 + ORA2        &
      &                                                      ', &
      'OL2P + ACO3 --> HCHO + ALD + HO2 + MO2 + ORA2            &
      &                                                      ', &
      'OLTP + ACO3 --> ALD + HCHO + HO2 + MO2 + ORA2            &
      &                                                      ', &
      'OLIP + ACO3 --> ALD + KET + HCHO + HO2 + MO2 + ORA2      &
      &                                                      ', &
      'KETP + ACO3 --> MGLY + HO2 + MO2 + ORA2                  &
      &                                                      ', &
      'ACO3 + ACO3 --> 2 MO2                                    &
      &                                                      ', &
      'ACO3 + TOLP --> MO2 + MGLY + GLY + DCB + HO2             &
      &                                                      ', &
      'ACO3 + XYLP --> MO2 + MGLY + DCB + HO2                   &
      &                                                      ', &
      'ACO3 + TCO3 --> MO2 + HO2 + GLY + MGLY + ACO3 + CO + 2 XO&
      &2                                                     ', &
      'XO2 + HO2 --> OP2                                        &
      &                                                      ', &
      'XO2 + MO2 --> HCHO + HO2                                 &
      &                                                      ', &
      'XO2 + ACO3 --> MO2                                       &
      &                                                      ', &
      'XO2 + NO --> NO2                                         &
      &                                                      ', &
      'XNO2 + NO2 --> ONIT                                      &
      &                                                      ', &
      'XNO2 + HO2 --> OP2                                       &
      &                                                      ', &
      'XNO2 + MO2 --> HCHO + HO2                                &
      &                                                      ', &
      'XNO2 + ACO3 --> MO2                                      &
      &                                                      ', &
      'MO2 + OLN --> 1.75 HCHO + HO2 + ALD + NO2                &
      &                                                      ', &
      'ACO3 + OLN --> HCHO + ALD + ORA2 + NO2 + MO2             &
      &                                                      ', &
      'OLN + OLN --> 2 HCHO + 2 ALD + 2 NO2                     &
      &                                                      ', &
      'API + HO --> APIP + CS1 + CS10 + CS100 + CS1000          &
      &                                                      ', &
      'API + NO3 --> OLN + CS1 + CS10 + CS100 + CS1000          &
      &                                                      ', &
      'API + O3 --> ALD + KET + CO + ETHP + KETP + HO + HO2 + H2&
      &O2 + CS1 + CS1 + CS10 + CS100 + CS1000                ', &
      'APIP + NO --> HO2 + ALD + KET + ONIT + NO2               &
      &                                                      ', &
      'APIP + HO2 --> OP2                                       &
      &                                                      ', &
      'APIP + MO2 --> HCHO + ALD + KET + 2 HO2                  &
      &                                                      ', &
      'APIP + ACO3 --> ALD + KET + HO2 + MO2                    &
      &                                                      ', &
      'APIP + NO3 --> ALD + KET + HO2 + NO2                     &
      &                                                      ', &
      'LIM + HO --> LIMP + CS1 + CS10 + CS100 + CS1000          &
      &                                                      ', &
      'LIM + NO3 --> OLN + CS1 + CS10 + CS100 + CS1000          &
      &                                                      ', &
      'LIM + O3 --> HCHO + OLT + CO + ETHP + KETP + HO + HO2 + H&
      &2O2 + MACR + ORA1 + ORA2 + CS1 + CS10 + CS100 + CS1000', &
      'LIMP + NO --> HO2 + MACR + OLI + HCHO + ONIT + NO2       &
      &                                                      ', &
      'LIMP + HO2 --> OP2                                       &
      &                                                      ', &
      'LIMP + MO2 --> 1.40 HCHO + MACR + OLI + 2.00 HO2         &
      &                                                      ', &
      'LIMP + ACO3 --> MACR + OLI + HCHO + HO2 + MO2            &
      &                                                      ', &
      'LIMP + NO3 --> MACR + OLI + HCHO + HO2 + NO2             &
      &                                                      ', &
      'ISOP + NO --> MACR + NO2 + HCHO + HO2 + ISON             &
      &                                                      ', &
      'ISOP + HO2 --> ISHP                                      &
      &                                                      ', &
      'ISOP + ISOP --> 2 MACR + HCHO + HO2                      &
      &                                                      ', &
      'ISHP + HO --> MACR + HO                                  &
      &                                                      ', &
      'ISON + HO --> HACE + NALD                                &
      &                                                      ', &
      'MACR + HO --> MACP                                       &
      &                                                      ', &
      'MACR + O3 --> MGLY + ORA1 + HO2 + CO + HO + ACO3         &
      &                                                      ', &
      'MACP + NO --> NO2 + HACE + CO + ACO3 + MGLY + HCHO + HO2 &
      &                                                      ', &
      'MACP + HO2 --> MAHP                                      &
      &                                                      ', &
      'MACP + MACP --> HACE + MGLY + HCHO + CO + HO2            &
      &                                                      ', &
      'MACP + NO2 --> MPAN                                      &
      &                                                      ', &
      'MPAN --> MACP + NO2                                      &
      &                                                      ', &
      'MPAN + HO --> HACE + NO2                                 &
      &                                                      ', &
      'MAHP + HO --> MACP                                       &
      &                                                      ', &
      'HACE + HO --> MGLY + HO2                                 &
      &                                                      ', &
      'NALD + HO --> HCHO + CO + NO2                            &
      &                                                      ', &
      'DMS + NO3 --> SO2                                        &
      &                                                      ', &
      'DMS + HO --> SO2                                         &
      &                                                      ', &
      'DMS + HO --> SO2 + DMSO                                  &
      &                                                      ', &
      'DMSO + HO --> SO2                                        &
      &                                                      ', &
      'CS10 + HO --> 1.075 CS1                                  &
      &                                                      ', &
      'CS100 + HO --> 1.075 CS10                                &
      &                                                      ', &
      'CS1000 + HO --> 1.075 CS100                              &
      &                                                      ', &
      'NH3 --> NH3                                              &
      &                                                      '  /)


END MODULE kppa_Serial_monitor
!---------------------- END kppa_Serial_monitor.f90 END ----------------------