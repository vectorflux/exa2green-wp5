! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Utility Data Module File
! 
! Generated by KPP-2.2.3 symbolic chemistry Kinetics PreProcessor
!       (http://www.cs.vt.edu/~asandu/Software/KPP)
! KPP is distributed under GPL, the general public licence
!       (http://www.gnu.org/copyleft/gpl.html)
! (C) 1995-1997, V. Damian & A. Sandu, CGRER, Univ. Iowa
! (C) 1997-2005, A. Sandu, Michigan Tech, Virginia Tech
!     With important contributions from:
!        M. Damian, Villanova University, USA
!        R. Sander, Max-Planck Institute for Chemistry, Mainz, Germany
! 
! File                 : kpp_Monitor.f90
! Time                 : Tue Mar 10 15:23:26 2015
! Working directory    : /users/charlesj/KPP_BOXMODEL/cosmo-art-new/kpp-2.2.3_files_with_Prace_Integrator
! Equation file        : kpp.kpp
! Output root filename : kpp
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE kpp_Monitor


  CHARACTER(LEN=15), PARAMETER, DIMENSION(83) :: SPC_NAMES = (/ &
     'NH3            ','N2             ','SULF           ', &
     'ORA1           ','ORA2           ','CS1            ', &
     'CO2            ','H2             ','ETH            ', &
     'ISHP           ','HC5            ','TPAN           ', &
     'HONO           ','DMSO           ','HC8            ', &
     'SO2            ','N2O5           ','MAHP           ', &
     'DMS            ','TOL            ','XYL            ', &
     'HC3            ','NALD           ','CS10           ', &
     'CS100          ','CS1000         ','O1D            ', &
     'PAA            ','MPAN           ','OP1            ', &
     'CH4            ','CSL            ','HACE           ', &
     'HNO4           ','PAN            ','O3P            ', &
     'H2O2           ','OL2            ','ISOP           ', &
     'ISON           ','O2             ','HNO3           ', &
     'API            ','CO             ','LIM            ', &
     'OLT            ','ISO            ','XNO2           ', &
     'GLY            ','XYLP           ','MACR           ', &
     'DCB            ','HC5P           ','HC8P           ', &
     'OLIP           ','KET            ','OL2P           ', &
     'OLTP           ','XO2            ','TOLP           ', &
     'OP2            ','MACP           ','OLN            ', &
     'MGLY           ','HCHO           ','TCO3           ', &
     'H2O            ','LIMP           ','APIP           ', &
     'OLI            ','KETP           ','ALD            ', &
     'ETHP           ','MO2            ','NO2            ', &
     'NO3            ','HO2            ','NO             ', &
     'O3             ','HO             ','HC3P           ', &
     'ACO3           ','ONIT           ' /)

  INTEGER, DIMENSION(1) :: LOOKAT
  INTEGER, DIMENSION(1) :: MONITOR
  CHARACTER(LEN=15), DIMENSION(1) :: SMASS
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_0 = (/ &
     '        NO2 --> O3P + NO                                                                            ', &
     '         O3 --> O1D + O2                                                                            ', &
     '         O3 --> O3P + O2                                                                            ', &
     '       HONO --> NO + HO                                                                             ', &
     '       HNO3 --> NO2 + HO                                                                            ', &
     '       HNO4 --> NO2 + HO2                                                                           ', &
     '        NO3 --> O2 + NO                                                                             ', &
     '        NO3 --> O3P + NO2                                                                           ', &
     '       H2O2 --> 2 HO                                                                                ', &
     '       HCHO --> H2 + CO                                                                             ', &
     '       HCHO --> CO + 2 HO2                                                                          ', &
     '        ALD --> CO + MO2 + HO2                                                                      ', &
     '        OP1 --> HCHO + HO2 + HO                                                                     ', &
     '        OP2 --> ALD + HO2 + HO                                                                      ', &
     '        PAA --> CO2 + MO2 + HO                                                                      ', &
     '        KET --> ETHP + ACO3                                                                         ', &
     '        GLY --> 1.87 CO + 0.13 HCHO                                                                 ', &
     '        GLY --> 1.55 CO + 0.45 HCHO + 0.8 HO2                                                       ', &
     '       MGLY --> CO + HO2 + ACO3                                                                     ', &
     '        DCB --> TCO3 + 0.98 HO2 + 0.02 ACO3                                                         ', &
     '       ONIT --> 0.8 KET + 0.2 ALD + NO2 + HO2                                                       ', &
     '   O3P + O2 --> O3                                                                                  ', &
     '  O3P + NO2 --> O2 + NO                                                                             ', &
     '   N2 + O1D --> N2 + O3P                                                                            ', &
     '   O1D + O2 --> O3P + O2                                                                            ', &
     '  O1D + H2O --> 2 HO                                                                                ', &
     '    NO + O3 --> O2 + NO2                                                                            ', &
     '    O3 + HO --> O2 + HO2                                                                            ', &
     '   HO2 + O3 --> 2 O2 + HO                                                                           ', &
     '   HO2 + NO --> NO2 + HO                                                                            ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_1 = (/ &
     '  NO2 + HO2 --> HNO4                                                                                ', &
     '      2 HO2 --> H2O2                                                                                ', &
     'H2O + 2 HO2 --> H2O2 + H2O                                                                          ', &
     '  H2O2 + HO --> H2O + HO2                                                                           ', &
     '    NO + HO --> HONO                                                                                ', &
     '  O2 + 2 NO --> 2 NO2                                                                               ', &
     '   NO2 + O3 --> NO3                                                                                 ', &
     '   NO3 + NO --> 2 NO2                                                                               ', &
     '  NO2 + NO3 --> O2 + NO2 + NO                                                                       ', &
     '  NO3 + HO2 --> O2 + HNO3                                                                           ', &
     '  NO2 + NO3 --> N2O5                                                                                ', &
     '       N2O5 --> NO2 + NO3                                                                           ', &
     '       N2O5 --> 2 HNO3                                                                              ', &
     '   NO2 + HO --> HNO3                                                                                ', &
     '  HNO3 + HO --> H2O + NO3                                                                           ', &
     '  HNO4 + HO --> O2 + H2O + NO2                                                                      ', &
     '   HO2 + HO --> O2 + H2O                                                                            ', &
     '   SO2 + HO --> SULF + HO2                                                                          ', &
     '    CO + HO --> HO2                                                                                 ', &
     '   CH4 + HO --> H2O + MO2                                                                           ', &
     '   ETH + HO --> H2O + ETHP                                                                          ', &
     '   HC3 + HO --> 0.025 KET + 0.009 HCHO + H2O + 0.075 ALD + 0.17 HO2 ... etc.                        ', &
     '   HC5 + HO --> HC5P + 0.25 XO2 + H2O                                                               ', &
     '   HC8 + HO --> 0.2033 CS10 + HC8P + 0.75 XO2 + H2O                                                 ', &
     '   OL2 + HO --> OL2P                                                                                ', &
     '   OLT + HO --> 0.00105 CS1 + 0.0021 CS10 + 0.014 CS100 + 0.0525 CS1000 ... etc.                    ', &
     '   OLI + HO --> 0.007 CS1 + 0.01353 CS10 + 0.04013 CS100 + 0.11667 CS1000 ... etc.                  ', &
     '   TOL + HO --> 0.0054433 CS1 + 0.1314 CS10 + 0.3833 CS100 + 0.49289 CS1000 ... etc.                ', &
     '   XYL + HO --> 0.04417 CS1 + 0.17667 CS10 + 0.22083 CS100 + 0.30917 CS1000 ... etc.                ', &
     '   CSL + HO --> 0.9 XO2 + 0.9 TCO3 + 0.1 HO2 - -0.9 HO                                              ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_2 = (/ &
     '  HCHO + HO --> CO + H2O + HO2                                                                      ', &
     '   ALD + HO --> H2O + ACO3                                                                          ', &
     '   KET + HO --> H2O + KETP                                                                          ', &
     '   GLY + HO --> 2 CO + H2O + HO2                                                                    ', &
     '  MGLY + HO --> CO + H2O + ACO3                                                                     ', &
     '   DCB + HO --> TCO3 + H2O                                                                          ', &
     '   OP1 + HO --> 0.5 HCHO + 0.5 MO2 + 0.5 HO                                                         ', &
     '   OP2 + HO --> 0.5 ALD + 0.5 HO + 0.5 HC3P                                                         ', &
     '   PAA + HO --> H2O + ACO3                                                                          ', &
     '   PAN + HO --> XO2 + HCHO + NO3                                                                    ', &
     '  HO + ONIT --> NO2 + HC3P                                                                          ', &
     '   ISO + HO --> 0.0034 CS1 + 0.01133 CS10 + 0.0056667 CS100 + ISOP                                  ', &
     ' NO2 + ACO3 --> PAN                                                                                 ', &
     '        PAN --> NO2 + ACO3                                                                          ', &
     ' TCO3 + NO2 --> TPAN                                                                                ', &
     '       TPAN --> TCO3 + NO2                                                                          ', &
     '   MO2 + NO --> HCHO + NO2 + HO2                                                                    ', &
     '  NO + HC3P --> 0.25 KET + 0.09 HCHO + 0.75 ALD + 0.964 NO2 + 0.964 HO2 ... etc.                    ', &
     '  HC5P + NO --> 0.69 KET + 0.38 ALD + 0.92 NO2 + 0.92 HO2 + 0.08 ONIT ... etc.                      ', &
     '  HC8P + NO --> 1.06 KET + 0.04 HCHO + 0.35 ALD + 0.76 NO2 + 0.76 HO2 ... etc.                      ', &
     '  OL2P + NO --> 1.6 HCHO + 0.2 ALD + NO2 + HO2                                                      ', &
     '  OLTP + NO --> HCHO + ALD + NO2 + HO2                                                              ', &
     '  OLIP + NO --> 0.1 KET + 0.28 HCHO + 1.45 ALD + NO2 + HO2                                          ', &
     '  NO + ACO3 --> MO2 + NO2                                                                           ', &
     '  TCO3 + NO --> 0.95 CO + 0.89 GLY + 2 XO2 + 0.11 MGLY + NO2 + 0.92 HO2 ... etc.                    ', &
     '  TOLP + NO --> 0.16 GLY + 0.7 DCB + 0.17 MGLY + NO2 + HO2                                          ', &
     '  XYLP + NO --> 0.806 DCB + 0.45 MGLY + NO2 + HO2                                                   ', &
     '  ETHP + NO --> ALD + NO2 + HO2                                                                     ', &
     '  KETP + NO --> MGLY + NO2 + HO2                                                                    ', &
     '   OLN + NO --> HCHO + ALD + 2 NO2                                                                  ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_3 = (/ &
     ' HCHO + NO3 --> HNO3 + CO + HO2                                                                     ', &
     '  ALD + NO3 --> HNO3 + ACO3                                                                         ', &
     '  GLY + NO3 --> HNO3 + 2 CO + HO2                                                                   ', &
     ' MGLY + NO3 --> HNO3 + CO + ACO3                                                                    ', &
     '  DCB + NO3 --> HNO3 + TCO3                                                                         ', &
     '  CSL + NO3 --> 0.5 CSL + HNO3 + XNO2                                                               ', &
     '  OL2 + NO3 --> OLN                                                                                 ', &
     '  OLT + NO3 --> 0.00105 CS1 + 0.0021 CS10 + 0.014 CS100 + 0.0525 CS1000 ... etc.                    ', &
     '  OLI + NO3 --> 0.007 CS1 + 0.01353 CS10 + 0.04013 CS100 + 0.11667 CS1000 ... etc.                  ', &
     '  ISO + NO3 --> ISON                                                                                ', &
     '   OL2 + O3 --> 0.4 ORA1 + 0.42 CO + HCHO + 0.12 HO2                                                ', &
     '   OLT + O3 --> 0.2 ORA1 + 0.2 ORA2 + 0.00105 CS1 + 0.0021 CS10 + 0.014 CS100 ... etc.              ', &
     '   OLI + O3 --> 0.06 ORA1 + 0.29 ORA2 + 0.007 CS1 + 0.01353 CS10 + 0.04013 CS100 ... etc.           ', &
     '   ISO + O3 --> 0.28 ORA1 + 0.0034 CS1 + 0.01133 CS10 + 0.0056667 CS100 ... etc.                    ', &
     '  MO2 + HO2 --> OP1                                                                                 ', &
     ' ETHP + HO2 --> OP2                                                                                 ', &
     ' HO2 + HC3P --> OP2                                                                                 ', &
     ' HC5P + HO2 --> OP2                                                                                 ', &
     ' HC8P + HO2 --> OP2                                                                                 ', &
     ' OL2P + HO2 --> OP2                                                                                 ', &
     ' OLTP + HO2 --> OP2                                                                                 ', &
     ' OLIP + HO2 --> OP2                                                                                 ', &
     ' KETP + HO2 --> OP2                                                                                 ', &
     ' HO2 + ACO3 --> PAA                                                                                 ', &
     ' TOLP + HO2 --> OP2                                                                                 ', &
     ' XYLP + HO2 --> OP2                                                                                 ', &
     ' TCO3 + HO2 --> OP2                                                                                 ', &
     '  OLN + HO2 --> ONIT                                                                                ', &
     '      2 MO2 --> 1.5 HCHO + HO2                                                                      ', &
     ' ETHP + MO2 --> 0.75 HCHO + 0.75 ALD + HO2                                                          ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_4 = (/ &
     ' MO2 + HC3P --> 0.26 KET + 0.84 HCHO + 0.77 ALD + HO2                                               ', &
     ' HC5P + MO2 --> 0.75 KET + 0.77 HCHO + 0.41 ALD + HO2                                               ', &
     ' HC8P + MO2 --> 1.39 KET + 0.8 HCHO + 0.46 ALD + HO2                                                ', &
     ' OL2P + MO2 --> 1.55 HCHO + 0.35 ALD + HO2                                                          ', &
     ' OLTP + MO2 --> 1.25 HCHO + 0.75 ALD + HO2                                                          ', &
     ' OLIP + MO2 --> 0.55 KET + 0.89 HCHO + 0.725 ALD + HO2                                              ', &
     ' KETP + MO2 --> 0.75 MGLY + 0.75 HCHO + HO2                                                         ', &
     ' MO2 + ACO3 --> 0.5 ORA2 + HCHO + 0.5 MO2 + 0.5 HO2                                                 ', &
     ' TOLP + MO2 --> 0.16 GLY + 0.7 DCB + 0.17 MGLY + HCHO + 2 HO2                                       ', &
     ' XYLP + MO2 --> 0.806 DCB + 0.45 MGLY + HCHO + 2 HO2                                                ', &
     ' TCO3 + MO2 --> 0.5 ORA2 + 0.475 CO + 0.445 GLY + XO2 + 0.055 MGLY + 0.5 HCHO ... etc.              ', &
     'ETHP + ACO3 --> 0.5 ORA2 + ALD + 0.5 MO2 + 0.5 HO2                                                  ', &
     'HC3P + ACO3 --> 0.5 ORA2 + 0.26 KET + 0.77 ALD + 0.5 MO2 + 0.5 HO2                                  ', &
     'HC5P + ACO3 --> 0.5 ORA2 + 0.75 KET + 0.41 ALD + 0.5 MO2 + 0.5 HO2                                  ', &
     'HC8P + ACO3 --> 0.5 ORA2 + 1.39 KET + 0.46 ALD + 0.5 MO2 + 0.5 HO2                                  ', &
     'OL2P + ACO3 --> 0.5 ORA2 + 0.8 HCHO + 0.6 ALD + 0.5 MO2 + 0.5 HO2                                   ', &
     'OLTP + ACO3 --> 0.5 ORA2 + 0.5 HCHO + ALD + 0.5 MO2 + 0.5 HO2                                       ', &
     'OLIP + ACO3 --> 0.5 ORA2 + 0.55 KET + 0.14 HCHO + 0.725 ALD + 0.5 MO2 ... etc.                      ', &
     'KETP + ACO3 --> 0.5 ORA2 + MGLY + 0.5 MO2 + 0.5 HO2                                                 ', &
     '     2 ACO3 --> 2 MO2                                                                               ', &
     'TOLP + ACO3 --> 0.16 GLY + 0.7 DCB + 0.17 MGLY + MO2 + HO2                                          ', &
     'XYLP + ACO3 --> 0.806 DCB + 0.45 MGLY + MO2 + HO2                                                   ', &
     'TCO3 + ACO3 --> 0.95 CO + 0.89 GLY + 2 XO2 + 0.11 MGLY + MO2 + 0.92 HO2 ... etc.                    ', &
     '  XO2 + HO2 --> OP2                                                                                 ', &
     '  XO2 + MO2 --> HCHO + HO2                                                                          ', &
     ' XO2 + ACO3 --> MO2                                                                                 ', &
     '   XO2 + NO --> NO2                                                                                 ', &
     ' XNO2 + NO2 --> ONIT                                                                                ', &
     ' XNO2 + HO2 --> OP2                                                                                 ', &
     ' XNO2 + MO2 --> HCHO + HO2                                                                          ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(30) :: EQN_NAMES_5 = (/ &
     'XNO2 + ACO3 --> MO2                                                                                 ', &
     '  OLN + MO2 --> 1.75 HCHO + ALD + NO2 + 0.5 HO2                                                     ', &
     ' OLN + ACO3 --> 0.5 ORA2 + HCHO + ALD + 0.5 MO2 + NO2                                               ', &
     '      2 OLN --> 2 HCHO + 2 ALD + 2 NO2                                                              ', &
     '   API + HO --> 0.08103 CS1 + 0.06936 CS10 + 0.27087 CS100 + 0.459 CS1000 ... etc.                  ', &
     '  API + NO3 --> 0.08103 CS1 + 0.06936 CS10 + 0.27087 CS100 + 0.459 CS1000 ... etc.                  ', &
     '   API + O3 --> 0.83663 CS1 + 0.06936 CS10 + 0.27087 CS100 + 0.459 CS1000 ... etc.                  ', &
     '  APIP + NO --> 0.8 KET + 0.8 ALD + 0.8 NO2 + 0.8 HO2 + 0.2 ONIT                                    ', &
     ' APIP + HO2 --> OP2                                                                                 ', &
     ' APIP + MO2 --> KET + HCHO + ALD + 2 HO2                                                            ', &
     'APIP + ACO3 --> KET + ALD + MO2 + HO2                                                               ', &
     ' APIP + NO3 --> KET + ALD + NO2 + HO2                                                               ', &
     '   LIM + HO --> 0.08103 CS1 + 0.06936 CS10 + 0.27087 CS100 + 0.459 CS1000 ... etc.                  ', &
     '  LIM + NO3 --> 0.08103 CS1 + 0.06936 CS10 + 0.27087 CS100 + 0.459 CS1000 ... etc.                  ', &
     '   LIM + O3 --> 0.01 ORA1 + 0.07 ORA2 + 0.08103 CS1 + 0.06936 CS10 + 0.27087 CS100 ... etc.         ', &
     '  LIMP + NO --> 0.4 MACR + 0.25 HCHO + 0.25 OLI + 0.65 NO2 + 0.65 HO2 ... etc.                      ', &
     ' LIMP + HO2 --> OP2                                                                                 ', &
     ' LIMP + MO2 --> 0.6 MACR + 1.4 HCHO + 0.4 OLI + 2 HO2                                               ', &
     'LIMP + ACO3 --> 0.6 MACR + 0.4 HCHO + 0.4 OLI + MO2 + HO2                                           ', &
     ' LIMP + NO3 --> 0.6 MACR + 0.4 HCHO + 0.4 OLI + NO2 + HO2                                           ', &
     '  ISOP + NO --> 0.046 ISON + MACR + HCHO + NO2 + HO2                                                ', &
     ' ISOP + HO2 --> ISHP                                                                                ', &
     '     2 ISOP --> 2 MACR + HCHO + HO2                                                                 ', &
     '  ISHP + HO --> MACR + HO                                                                           ', &
     '  ISON + HO --> NALD + HACE                                                                         ', &
     '  MACR + HO --> MACP                                                                                ', &
     '  MACR + O3 --> 0.45 ORA1 + 0.22 CO + 0.9 MGLY + 0.32 HO2 + 0.19 HO ... etc.                        ', &
     '  MACP + NO --> 0.25 HACE + 0.25 CO + 0.5 MGLY + 0.75 HCHO + NO2 + 0.75 HO2 ... etc.                ', &
     ' MACP + HO2 --> MAHP                                                                                ', &
     '     2 MACP --> HACE + 0.5 CO + MGLY + 0.5 HCHO + HO2                                               ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(14) :: EQN_NAMES_6 = (/ &
     ' MACP + NO2 --> MPAN                                                                                ', &
     '       MPAN --> MACP + NO2                                                                          ', &
     '  MPAN + HO --> HACE + NO2                                                                          ', &
     '  MAHP + HO --> MACP                                                                                ', &
     '  HACE + HO --> MGLY + HO2                                                                          ', &
     '  NALD + HO --> CO + HCHO + NO2                                                                     ', &
     '  DMS + NO3 --> SO2                                                                                 ', &
     '   DMS + HO --> SO2                                                                                 ', &
     '   DMS + HO --> 0.4 DMSO + 0.6 SO2                                                                  ', &
     '  DMSO + HO --> 0.6 SO2                                                                             ', &
     '  CS10 + HO --> 1.075 CS1                                                                           ', &
     ' CS100 + HO --> 1.075 CS10                                                                          ', &
     'CS1000 + HO --> 1.075 CS100                                                                         ', &
     '        NH3 --> NH3                                                                                 ' /)
  CHARACTER(LEN=100), PARAMETER, DIMENSION(194) :: EQN_NAMES = (/&
    EQN_NAMES_0, EQN_NAMES_1, EQN_NAMES_2, EQN_NAMES_3, EQN_NAMES_4, &
    EQN_NAMES_5, EQN_NAMES_6 /)

! INLINED global variables

! End INLINED global variables


END MODULE kpp_Monitor
