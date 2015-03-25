! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! The Stoichiometric Chemical Model File
! 
! Generated by KPP-2.2 symbolic chemistry Kinetics PreProcessor
!       (http://www.cs.vt.edu/~asandu/Software/KPP)
! KPP is distributed under GPL, the general public licence
!       (http://www.gnu.org/copyleft/gpl.html)
! (C) 1995-1997, V. Damian & A. Sandu, CGRER, Univ. Iowa
! (C) 1997-2005, A. Sandu, Michigan Tech, Virginia Tech
!     With important contributions from:
!        M. Damian, Villanova University, USA
!        R. Sander, Max-Planck Institute for Chemistry, Mainz, Germany
! 
! File                 : box_model_Stoichiom.f90
! Time                 : Tue Jun  3 15:01:53 2014
! Working directory    : /Users/jlinford/workspace/kppa/verify/box_model_kpp221
! Equation file        : box_model.kpp
! Output root filename : box_model
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE box_model_Stoichiom

  USE box_model_Parameters
  USE box_model_StoichiomSP

  IMPLICIT NONE

CONTAINS


! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! ReactantProd - Reactant Products in each equation
!   Arguments :
!      V         - Concentrations of variable species (local)
!      F         - Concentrations of fixed species (local)
!      ARP       - Reactant product in each equation
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE ReactantProd ( V, F, ARP )

! V - Concentrations of variable species (local)
  REAL(kind=dp) :: V(NVAR)
! F - Concentrations of fixed species (local)
  REAL(kind=dp) :: F(NFIX)
! ARP - Reactant product in each equation
  REAL(kind=dp) :: ARP(NREACT)


! Reactant Products in each equation are useful in the
!     stoichiometric formulation of mass action law
  ARP(1) = V(76)
  ARP(2) = V(78)
  ARP(3) = V(78)
  ARP(4) = V(13)
  ARP(5) = V(44)
  ARP(6) = V(32)
  ARP(7) = V(80)
  ARP(8) = V(80)
  ARP(9) = V(35)
  ARP(10) = V(63)
  ARP(11) = V(63)
  ARP(12) = V(70)
  ARP(13) = V(28)
  ARP(14) = V(59)
  ARP(15) = V(29)
  ARP(16) = V(54)
  ARP(17) = V(46)
  ARP(18) = V(46)
  ARP(19) = V(62)
  ARP(20) = V(50)
  ARP(21) = V(74)
  ARP(22) = V(34)*V(39)
  ARP(23) = V(34)*V(76)
  ARP(24) = V(1)*V(25)
  ARP(25) = V(25)*V(39)
  ARP(26) = V(25)*V(66)
  ARP(27) = V(77)*V(78)
  ARP(28) = V(72)*V(78)
  ARP(29) = V(75)*V(78)
  ARP(30) = V(75)*V(77)
  ARP(31) = V(75)*V(76)
  ARP(32) = V(75)*V(75)
  ARP(33) = V(66)*V(75)*V(75)
  ARP(34) = V(35)*V(72)
  ARP(35) = V(72)*V(77)
  ARP(36) = V(39)*V(77)*V(77)
  ARP(37) = V(76)*V(78)
  ARP(38) = V(77)*V(80)
  ARP(39) = V(76)*V(80)
  ARP(40) = V(75)*V(80)
  ARP(41) = V(76)*V(80)
  ARP(42) = V(26)
  ARP(43) = V(26)*V(66)
  ARP(44) = V(72)*V(76)
  ARP(45) = V(44)*V(72)
  ARP(46) = V(32)*V(72)
  ARP(47) = V(72)*V(75)
  ARP(48) = V(17)*V(72)
  ARP(49) = V(41)*V(72)
  ARP(50) = V(72)*F(1)
  ARP(51) = V(9)*V(72)
  ARP(52) = V(20)*V(72)
  ARP(53) = V(11)*V(72)
  ARP(54) = V(14)*V(72)
  ARP(55) = V(36)*V(72)
  ARP(56) = V(42)*V(72)
  ARP(57) = V(68)*V(72)
  ARP(58) = V(18)*V(72)
  ARP(59) = V(19)*V(72)
  ARP(60) = V(30)*V(72)
  ARP(61) = V(63)*V(72)
  ARP(62) = V(70)*V(72)
  ARP(63) = V(54)*V(72)
  ARP(64) = V(46)*V(72)
  ARP(65) = V(62)*V(72)
  ARP(66) = V(50)*V(72)
  ARP(67) = V(28)*V(72)
  ARP(68) = V(59)*V(72)
  ARP(69) = V(29)*V(72)
  ARP(70) = V(33)*V(72)
  ARP(71) = V(72)*V(74)
  ARP(72) = V(43)*V(72)
  ARP(73) = V(76)*V(81)
  ARP(74) = V(33)
  ARP(75) = V(64)*V(76)
  ARP(76) = V(12)
  ARP(77) = V(77)*V(79)
  ARP(78) = V(73)*V(77)
  ARP(79) = V(51)*V(77)
  ARP(80) = V(52)*V(77)
  ARP(81) = V(55)*V(77)
  ARP(82) = V(56)*V(77)
  ARP(83) = V(53)*V(77)
  ARP(84) = V(77)*V(81)
  ARP(85) = V(64)*V(77)
  ARP(86) = V(58)*V(77)
  ARP(87) = V(48)*V(77)
  ARP(88) = V(69)*V(77)
  ARP(89) = V(71)*V(77)
  ARP(90) = V(61)*V(77)
  ARP(91) = V(63)*V(80)
  ARP(92) = V(70)*V(80)
  ARP(93) = V(46)*V(80)
  ARP(94) = V(62)*V(80)
  ARP(95) = V(50)*V(80)
  ARP(96) = V(30)*V(80)
  ARP(97) = V(36)*V(80)
  ARP(98) = V(42)*V(80)
  ARP(99) = V(68)*V(80)
  ARP(100) = V(43)*V(80)
  ARP(101) = V(36)*V(78)
  ARP(102) = V(42)*V(78)
  ARP(103) = V(68)*V(78)
  ARP(104) = V(43)*V(78)
  ARP(105) = V(75)*V(79)
  ARP(106) = V(69)*V(75)
  ARP(107) = V(73)*V(75)
  ARP(108) = V(51)*V(75)
  ARP(109) = V(52)*V(75)
  ARP(110) = V(55)*V(75)
  ARP(111) = V(56)*V(75)
  ARP(112) = V(53)*V(75)
  ARP(113) = V(71)*V(75)
  ARP(114) = V(75)*V(81)
  ARP(115) = V(58)*V(75)
  ARP(116) = V(48)*V(75)
  ARP(117) = V(64)*V(75)
  ARP(118) = V(61)*V(75)
  ARP(119) = V(79)*V(79)
  ARP(120) = V(69)*V(79)
  ARP(121) = V(73)*V(79)
  ARP(122) = V(51)*V(79)
  ARP(123) = V(52)*V(79)
  ARP(124) = V(55)*V(79)
  ARP(125) = V(56)*V(79)
  ARP(126) = V(53)*V(79)
  ARP(127) = V(71)*V(79)
  ARP(128) = V(79)*V(81)
  ARP(129) = V(58)*V(79)
  ARP(130) = V(48)*V(79)
  ARP(131) = V(64)*V(79)
  ARP(132) = V(69)*V(81)
  ARP(133) = V(73)*V(81)
  ARP(134) = V(51)*V(81)
  ARP(135) = V(52)*V(81)
  ARP(136) = V(55)*V(81)
  ARP(137) = V(56)*V(81)
  ARP(138) = V(53)*V(81)
  ARP(139) = V(71)*V(81)
  ARP(140) = V(81)*V(81)
  ARP(141) = V(58)*V(81)
  ARP(142) = V(48)*V(81)
  ARP(143) = V(64)*V(81)
  ARP(144) = V(57)*V(75)
  ARP(145) = V(57)*V(79)
  ARP(146) = V(57)*V(81)
  ARP(147) = V(57)*V(77)
  ARP(148) = V(47)*V(76)
  ARP(149) = V(47)*V(75)
  ARP(150) = V(47)*V(79)
  ARP(151) = V(47)*V(81)
  ARP(152) = V(61)*V(79)
  ARP(153) = V(61)*V(81)
  ARP(154) = V(61)*V(61)
  ARP(155) = V(40)*V(72)
  ARP(156) = V(40)*V(80)
  ARP(157) = V(40)*V(78)
  ARP(158) = V(65)*V(77)
  ARP(159) = V(65)*V(75)
  ARP(160) = V(65)*V(79)
  ARP(161) = V(65)*V(81)
  ARP(162) = V(65)*V(80)
  ARP(163) = V(45)*V(72)
  ARP(164) = V(45)*V(80)
  ARP(165) = V(45)*V(78)
  ARP(166) = V(67)*V(77)
  ARP(167) = V(67)*V(75)
  ARP(168) = V(67)*V(79)
  ARP(169) = V(67)*V(81)
  ARP(170) = V(67)*V(80)
  ARP(171) = V(37)*V(77)
  ARP(172) = V(37)*V(75)
  ARP(173) = V(37)*V(37)
  ARP(174) = V(10)*V(72)
  ARP(175) = V(38)*V(72)
  ARP(176) = V(49)*V(72)
  ARP(177) = V(49)*V(78)
  ARP(178) = V(60)*V(77)
  ARP(179) = V(60)*V(75)
  ARP(180) = V(60)*V(60)
  ARP(181) = V(60)*V(76)
  ARP(182) = V(27)
  ARP(183) = V(27)*V(72)
  ARP(184) = V(15)*V(72)
  ARP(185) = V(31)*V(72)
  ARP(186) = V(21)*V(72)
  ARP(187) = V(16)*V(80)
  ARP(188) = V(16)*V(72)
  ARP(189) = V(8)*V(72)
  ARP(190) = V(22)*V(72)
  ARP(191) = V(23)*V(72)
  ARP(192) = V(24)*V(72)
      
END SUBROUTINE ReactantProd

! End of ReactantProd function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! JacReactantProd - Jacobian of Reactant Products vector
!   Arguments :
!      V         - Concentrations of variable species (local)
!      F         - Concentrations of fixed species (local)
!      JVRP      - d ARP(1:NREACT)/d VAR (1:NVAR)
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE JacReactantProd ( V, F, JVRP )

! V - Concentrations of variable species (local)
  REAL(kind=dp) :: V(NVAR)
! F - Concentrations of fixed species (local)
  REAL(kind=dp) :: F(NFIX)
! JVRP - d ARP(1:NREACT)/d VAR (1:NVAR)
  REAL(kind=dp) :: JVRP(NJVRP)


! Reactant Products in each equation are useful in the
!    stoichiometric formulation of mass action law
! Below we compute the Jacobian of the Reactant Products vector
!    w.r.t. variable species: d ARP(1:NREACT) / d Var(1:NVAR)

! JVRP(1) = dARP(1)/dV(76)
  JVRP(1) = 1
! JVRP(2) = dARP(2)/dV(78)
  JVRP(2) = 1
! JVRP(3) = dARP(3)/dV(78)
  JVRP(3) = 1
! JVRP(4) = dARP(4)/dV(13)
  JVRP(4) = 1
! JVRP(5) = dARP(5)/dV(44)
  JVRP(5) = 1
! JVRP(6) = dARP(6)/dV(32)
  JVRP(6) = 1
! JVRP(7) = dARP(7)/dV(80)
  JVRP(7) = 1
! JVRP(8) = dARP(8)/dV(80)
  JVRP(8) = 1
! JVRP(9) = dARP(9)/dV(35)
  JVRP(9) = 1
! JVRP(10) = dARP(10)/dV(63)
  JVRP(10) = 1
! JVRP(11) = dARP(11)/dV(63)
  JVRP(11) = 1
! JVRP(12) = dARP(12)/dV(70)
  JVRP(12) = 1
! JVRP(13) = dARP(13)/dV(28)
  JVRP(13) = 1
! JVRP(14) = dARP(14)/dV(59)
  JVRP(14) = 1
! JVRP(15) = dARP(15)/dV(29)
  JVRP(15) = 1
! JVRP(16) = dARP(16)/dV(54)
  JVRP(16) = 1
! JVRP(17) = dARP(17)/dV(46)
  JVRP(17) = 1
! JVRP(18) = dARP(18)/dV(46)
  JVRP(18) = 1
! JVRP(19) = dARP(19)/dV(62)
  JVRP(19) = 1
! JVRP(20) = dARP(20)/dV(50)
  JVRP(20) = 1
! JVRP(21) = dARP(21)/dV(74)
  JVRP(21) = 1
! JVRP(22) = dARP(22)/dV(34)
  JVRP(22) = V(39)
! JVRP(23) = dARP(22)/dV(39)
  JVRP(23) = V(34)
! JVRP(24) = dARP(23)/dV(34)
  JVRP(24) = V(76)
! JVRP(25) = dARP(23)/dV(76)
  JVRP(25) = V(34)
! JVRP(26) = dARP(24)/dV(1)
  JVRP(26) = V(25)
! JVRP(27) = dARP(24)/dV(25)
  JVRP(27) = V(1)
! JVRP(28) = dARP(25)/dV(25)
  JVRP(28) = V(39)
! JVRP(29) = dARP(25)/dV(39)
  JVRP(29) = V(25)
! JVRP(30) = dARP(26)/dV(25)
  JVRP(30) = V(66)
! JVRP(31) = dARP(26)/dV(66)
  JVRP(31) = V(25)
! JVRP(32) = dARP(27)/dV(77)
  JVRP(32) = V(78)
! JVRP(33) = dARP(27)/dV(78)
  JVRP(33) = V(77)
! JVRP(34) = dARP(28)/dV(72)
  JVRP(34) = V(78)
! JVRP(35) = dARP(28)/dV(78)
  JVRP(35) = V(72)
! JVRP(36) = dARP(29)/dV(75)
  JVRP(36) = V(78)
! JVRP(37) = dARP(29)/dV(78)
  JVRP(37) = V(75)
! JVRP(38) = dARP(30)/dV(75)
  JVRP(38) = V(77)
! JVRP(39) = dARP(30)/dV(77)
  JVRP(39) = V(75)
! JVRP(40) = dARP(31)/dV(75)
  JVRP(40) = V(76)
! JVRP(41) = dARP(31)/dV(76)
  JVRP(41) = V(75)
! JVRP(42) = dARP(32)/dV(75)
  JVRP(42) = 2*V(75)
! JVRP(43) = dARP(33)/dV(66)
  JVRP(43) = V(75)*V(75)
! JVRP(44) = dARP(33)/dV(75)
  JVRP(44) = 2*V(66)*V(75)
! JVRP(45) = dARP(34)/dV(35)
  JVRP(45) = V(72)
! JVRP(46) = dARP(34)/dV(72)
  JVRP(46) = V(35)
! JVRP(47) = dARP(35)/dV(72)
  JVRP(47) = V(77)
! JVRP(48) = dARP(35)/dV(77)
  JVRP(48) = V(72)
! JVRP(49) = dARP(36)/dV(39)
  JVRP(49) = V(77)*V(77)
! JVRP(50) = dARP(36)/dV(77)
  JVRP(50) = 2*V(39)*V(77)
! JVRP(51) = dARP(37)/dV(76)
  JVRP(51) = V(78)
! JVRP(52) = dARP(37)/dV(78)
  JVRP(52) = V(76)
! JVRP(53) = dARP(38)/dV(77)
  JVRP(53) = V(80)
! JVRP(54) = dARP(38)/dV(80)
  JVRP(54) = V(77)
! JVRP(55) = dARP(39)/dV(76)
  JVRP(55) = V(80)
! JVRP(56) = dARP(39)/dV(80)
  JVRP(56) = V(76)
! JVRP(57) = dARP(40)/dV(75)
  JVRP(57) = V(80)
! JVRP(58) = dARP(40)/dV(80)
  JVRP(58) = V(75)
! JVRP(59) = dARP(41)/dV(76)
  JVRP(59) = V(80)
! JVRP(60) = dARP(41)/dV(80)
  JVRP(60) = V(76)
! JVRP(61) = dARP(42)/dV(26)
  JVRP(61) = 1
! JVRP(62) = dARP(43)/dV(26)
  JVRP(62) = V(66)
! JVRP(63) = dARP(43)/dV(66)
  JVRP(63) = V(26)
! JVRP(64) = dARP(44)/dV(72)
  JVRP(64) = V(76)
! JVRP(65) = dARP(44)/dV(76)
  JVRP(65) = V(72)
! JVRP(66) = dARP(45)/dV(44)
  JVRP(66) = V(72)
! JVRP(67) = dARP(45)/dV(72)
  JVRP(67) = V(44)
! JVRP(68) = dARP(46)/dV(32)
  JVRP(68) = V(72)
! JVRP(69) = dARP(46)/dV(72)
  JVRP(69) = V(32)
! JVRP(70) = dARP(47)/dV(72)
  JVRP(70) = V(75)
! JVRP(71) = dARP(47)/dV(75)
  JVRP(71) = V(72)
! JVRP(72) = dARP(48)/dV(17)
  JVRP(72) = V(72)
! JVRP(73) = dARP(48)/dV(72)
  JVRP(73) = V(17)
! JVRP(74) = dARP(49)/dV(41)
  JVRP(74) = V(72)
! JVRP(75) = dARP(49)/dV(72)
  JVRP(75) = V(41)
! JVRP(76) = dARP(50)/dV(72)
  JVRP(76) = F(1)
! JVRP(77) = dARP(51)/dV(9)
  JVRP(77) = V(72)
! JVRP(78) = dARP(51)/dV(72)
  JVRP(78) = V(9)
! JVRP(79) = dARP(52)/dV(20)
  JVRP(79) = V(72)
! JVRP(80) = dARP(52)/dV(72)
  JVRP(80) = V(20)
! JVRP(81) = dARP(53)/dV(11)
  JVRP(81) = V(72)
! JVRP(82) = dARP(53)/dV(72)
  JVRP(82) = V(11)
! JVRP(83) = dARP(54)/dV(14)
  JVRP(83) = V(72)
! JVRP(84) = dARP(54)/dV(72)
  JVRP(84) = V(14)
! JVRP(85) = dARP(55)/dV(36)
  JVRP(85) = V(72)
! JVRP(86) = dARP(55)/dV(72)
  JVRP(86) = V(36)
! JVRP(87) = dARP(56)/dV(42)
  JVRP(87) = V(72)
! JVRP(88) = dARP(56)/dV(72)
  JVRP(88) = V(42)
! JVRP(89) = dARP(57)/dV(68)
  JVRP(89) = V(72)
! JVRP(90) = dARP(57)/dV(72)
  JVRP(90) = V(68)
! JVRP(91) = dARP(58)/dV(18)
  JVRP(91) = V(72)
! JVRP(92) = dARP(58)/dV(72)
  JVRP(92) = V(18)
! JVRP(93) = dARP(59)/dV(19)
  JVRP(93) = V(72)
! JVRP(94) = dARP(59)/dV(72)
  JVRP(94) = V(19)
! JVRP(95) = dARP(60)/dV(30)
  JVRP(95) = V(72)
! JVRP(96) = dARP(60)/dV(72)
  JVRP(96) = V(30)
! JVRP(97) = dARP(61)/dV(63)
  JVRP(97) = V(72)
! JVRP(98) = dARP(61)/dV(72)
  JVRP(98) = V(63)
! JVRP(99) = dARP(62)/dV(70)
  JVRP(99) = V(72)
! JVRP(100) = dARP(62)/dV(72)
  JVRP(100) = V(70)
! JVRP(101) = dARP(63)/dV(54)
  JVRP(101) = V(72)
! JVRP(102) = dARP(63)/dV(72)
  JVRP(102) = V(54)
! JVRP(103) = dARP(64)/dV(46)
  JVRP(103) = V(72)
! JVRP(104) = dARP(64)/dV(72)
  JVRP(104) = V(46)
! JVRP(105) = dARP(65)/dV(62)
  JVRP(105) = V(72)
! JVRP(106) = dARP(65)/dV(72)
  JVRP(106) = V(62)
! JVRP(107) = dARP(66)/dV(50)
  JVRP(107) = V(72)
! JVRP(108) = dARP(66)/dV(72)
  JVRP(108) = V(50)
! JVRP(109) = dARP(67)/dV(28)
  JVRP(109) = V(72)
! JVRP(110) = dARP(67)/dV(72)
  JVRP(110) = V(28)
! JVRP(111) = dARP(68)/dV(59)
  JVRP(111) = V(72)
! JVRP(112) = dARP(68)/dV(72)
  JVRP(112) = V(59)
! JVRP(113) = dARP(69)/dV(29)
  JVRP(113) = V(72)
! JVRP(114) = dARP(69)/dV(72)
  JVRP(114) = V(29)
! JVRP(115) = dARP(70)/dV(33)
  JVRP(115) = V(72)
! JVRP(116) = dARP(70)/dV(72)
  JVRP(116) = V(33)
! JVRP(117) = dARP(71)/dV(72)
  JVRP(117) = V(74)
! JVRP(118) = dARP(71)/dV(74)
  JVRP(118) = V(72)
! JVRP(119) = dARP(72)/dV(43)
  JVRP(119) = V(72)
! JVRP(120) = dARP(72)/dV(72)
  JVRP(120) = V(43)
! JVRP(121) = dARP(73)/dV(76)
  JVRP(121) = V(81)
! JVRP(122) = dARP(73)/dV(81)
  JVRP(122) = V(76)
! JVRP(123) = dARP(74)/dV(33)
  JVRP(123) = 1
! JVRP(124) = dARP(75)/dV(64)
  JVRP(124) = V(76)
! JVRP(125) = dARP(75)/dV(76)
  JVRP(125) = V(64)
! JVRP(126) = dARP(76)/dV(12)
  JVRP(126) = 1
! JVRP(127) = dARP(77)/dV(77)
  JVRP(127) = V(79)
! JVRP(128) = dARP(77)/dV(79)
  JVRP(128) = V(77)
! JVRP(129) = dARP(78)/dV(73)
  JVRP(129) = V(77)
! JVRP(130) = dARP(78)/dV(77)
  JVRP(130) = V(73)
! JVRP(131) = dARP(79)/dV(51)
  JVRP(131) = V(77)
! JVRP(132) = dARP(79)/dV(77)
  JVRP(132) = V(51)
! JVRP(133) = dARP(80)/dV(52)
  JVRP(133) = V(77)
! JVRP(134) = dARP(80)/dV(77)
  JVRP(134) = V(52)
! JVRP(135) = dARP(81)/dV(55)
  JVRP(135) = V(77)
! JVRP(136) = dARP(81)/dV(77)
  JVRP(136) = V(55)
! JVRP(137) = dARP(82)/dV(56)
  JVRP(137) = V(77)
! JVRP(138) = dARP(82)/dV(77)
  JVRP(138) = V(56)
! JVRP(139) = dARP(83)/dV(53)
  JVRP(139) = V(77)
! JVRP(140) = dARP(83)/dV(77)
  JVRP(140) = V(53)
! JVRP(141) = dARP(84)/dV(77)
  JVRP(141) = V(81)
! JVRP(142) = dARP(84)/dV(81)
  JVRP(142) = V(77)
! JVRP(143) = dARP(85)/dV(64)
  JVRP(143) = V(77)
! JVRP(144) = dARP(85)/dV(77)
  JVRP(144) = V(64)
! JVRP(145) = dARP(86)/dV(58)
  JVRP(145) = V(77)
! JVRP(146) = dARP(86)/dV(77)
  JVRP(146) = V(58)
! JVRP(147) = dARP(87)/dV(48)
  JVRP(147) = V(77)
! JVRP(148) = dARP(87)/dV(77)
  JVRP(148) = V(48)
! JVRP(149) = dARP(88)/dV(69)
  JVRP(149) = V(77)
! JVRP(150) = dARP(88)/dV(77)
  JVRP(150) = V(69)
! JVRP(151) = dARP(89)/dV(71)
  JVRP(151) = V(77)
! JVRP(152) = dARP(89)/dV(77)
  JVRP(152) = V(71)
! JVRP(153) = dARP(90)/dV(61)
  JVRP(153) = V(77)
! JVRP(154) = dARP(90)/dV(77)
  JVRP(154) = V(61)
! JVRP(155) = dARP(91)/dV(63)
  JVRP(155) = V(80)
! JVRP(156) = dARP(91)/dV(80)
  JVRP(156) = V(63)
! JVRP(157) = dARP(92)/dV(70)
  JVRP(157) = V(80)
! JVRP(158) = dARP(92)/dV(80)
  JVRP(158) = V(70)
! JVRP(159) = dARP(93)/dV(46)
  JVRP(159) = V(80)
! JVRP(160) = dARP(93)/dV(80)
  JVRP(160) = V(46)
! JVRP(161) = dARP(94)/dV(62)
  JVRP(161) = V(80)
! JVRP(162) = dARP(94)/dV(80)
  JVRP(162) = V(62)
! JVRP(163) = dARP(95)/dV(50)
  JVRP(163) = V(80)
! JVRP(164) = dARP(95)/dV(80)
  JVRP(164) = V(50)
! JVRP(165) = dARP(96)/dV(30)
  JVRP(165) = V(80)
! JVRP(166) = dARP(96)/dV(80)
  JVRP(166) = V(30)
! JVRP(167) = dARP(97)/dV(36)
  JVRP(167) = V(80)
! JVRP(168) = dARP(97)/dV(80)
  JVRP(168) = V(36)
! JVRP(169) = dARP(98)/dV(42)
  JVRP(169) = V(80)
! JVRP(170) = dARP(98)/dV(80)
  JVRP(170) = V(42)
! JVRP(171) = dARP(99)/dV(68)
  JVRP(171) = V(80)
! JVRP(172) = dARP(99)/dV(80)
  JVRP(172) = V(68)
! JVRP(173) = dARP(100)/dV(43)
  JVRP(173) = V(80)
! JVRP(174) = dARP(100)/dV(80)
  JVRP(174) = V(43)
! JVRP(175) = dARP(101)/dV(36)
  JVRP(175) = V(78)
! JVRP(176) = dARP(101)/dV(78)
  JVRP(176) = V(36)
! JVRP(177) = dARP(102)/dV(42)
  JVRP(177) = V(78)
! JVRP(178) = dARP(102)/dV(78)
  JVRP(178) = V(42)
! JVRP(179) = dARP(103)/dV(68)
  JVRP(179) = V(78)
! JVRP(180) = dARP(103)/dV(78)
  JVRP(180) = V(68)
! JVRP(181) = dARP(104)/dV(43)
  JVRP(181) = V(78)
! JVRP(182) = dARP(104)/dV(78)
  JVRP(182) = V(43)
! JVRP(183) = dARP(105)/dV(75)
  JVRP(183) = V(79)
! JVRP(184) = dARP(105)/dV(79)
  JVRP(184) = V(75)
! JVRP(185) = dARP(106)/dV(69)
  JVRP(185) = V(75)
! JVRP(186) = dARP(106)/dV(75)
  JVRP(186) = V(69)
! JVRP(187) = dARP(107)/dV(73)
  JVRP(187) = V(75)
! JVRP(188) = dARP(107)/dV(75)
  JVRP(188) = V(73)
! JVRP(189) = dARP(108)/dV(51)
  JVRP(189) = V(75)
! JVRP(190) = dARP(108)/dV(75)
  JVRP(190) = V(51)
! JVRP(191) = dARP(109)/dV(52)
  JVRP(191) = V(75)
! JVRP(192) = dARP(109)/dV(75)
  JVRP(192) = V(52)
! JVRP(193) = dARP(110)/dV(55)
  JVRP(193) = V(75)
! JVRP(194) = dARP(110)/dV(75)
  JVRP(194) = V(55)
! JVRP(195) = dARP(111)/dV(56)
  JVRP(195) = V(75)
! JVRP(196) = dARP(111)/dV(75)
  JVRP(196) = V(56)
! JVRP(197) = dARP(112)/dV(53)
  JVRP(197) = V(75)
! JVRP(198) = dARP(112)/dV(75)
  JVRP(198) = V(53)
! JVRP(199) = dARP(113)/dV(71)
  JVRP(199) = V(75)
! JVRP(200) = dARP(113)/dV(75)
  JVRP(200) = V(71)
! JVRP(201) = dARP(114)/dV(75)
  JVRP(201) = V(81)
! JVRP(202) = dARP(114)/dV(81)
  JVRP(202) = V(75)
! JVRP(203) = dARP(115)/dV(58)
  JVRP(203) = V(75)
! JVRP(204) = dARP(115)/dV(75)
  JVRP(204) = V(58)
! JVRP(205) = dARP(116)/dV(48)
  JVRP(205) = V(75)
! JVRP(206) = dARP(116)/dV(75)
  JVRP(206) = V(48)
! JVRP(207) = dARP(117)/dV(64)
  JVRP(207) = V(75)
! JVRP(208) = dARP(117)/dV(75)
  JVRP(208) = V(64)
! JVRP(209) = dARP(118)/dV(61)
  JVRP(209) = V(75)
! JVRP(210) = dARP(118)/dV(75)
  JVRP(210) = V(61)
! JVRP(211) = dARP(119)/dV(79)
  JVRP(211) = 2*V(79)
! JVRP(212) = dARP(120)/dV(69)
  JVRP(212) = V(79)
! JVRP(213) = dARP(120)/dV(79)
  JVRP(213) = V(69)
! JVRP(214) = dARP(121)/dV(73)
  JVRP(214) = V(79)
! JVRP(215) = dARP(121)/dV(79)
  JVRP(215) = V(73)
! JVRP(216) = dARP(122)/dV(51)
  JVRP(216) = V(79)
! JVRP(217) = dARP(122)/dV(79)
  JVRP(217) = V(51)
! JVRP(218) = dARP(123)/dV(52)
  JVRP(218) = V(79)
! JVRP(219) = dARP(123)/dV(79)
  JVRP(219) = V(52)
! JVRP(220) = dARP(124)/dV(55)
  JVRP(220) = V(79)
! JVRP(221) = dARP(124)/dV(79)
  JVRP(221) = V(55)
! JVRP(222) = dARP(125)/dV(56)
  JVRP(222) = V(79)
! JVRP(223) = dARP(125)/dV(79)
  JVRP(223) = V(56)
! JVRP(224) = dARP(126)/dV(53)
  JVRP(224) = V(79)
! JVRP(225) = dARP(126)/dV(79)
  JVRP(225) = V(53)
! JVRP(226) = dARP(127)/dV(71)
  JVRP(226) = V(79)
! JVRP(227) = dARP(127)/dV(79)
  JVRP(227) = V(71)
! JVRP(228) = dARP(128)/dV(79)
  JVRP(228) = V(81)
! JVRP(229) = dARP(128)/dV(81)
  JVRP(229) = V(79)
! JVRP(230) = dARP(129)/dV(58)
  JVRP(230) = V(79)
! JVRP(231) = dARP(129)/dV(79)
  JVRP(231) = V(58)
! JVRP(232) = dARP(130)/dV(48)
  JVRP(232) = V(79)
! JVRP(233) = dARP(130)/dV(79)
  JVRP(233) = V(48)
! JVRP(234) = dARP(131)/dV(64)
  JVRP(234) = V(79)
! JVRP(235) = dARP(131)/dV(79)
  JVRP(235) = V(64)
! JVRP(236) = dARP(132)/dV(69)
  JVRP(236) = V(81)
! JVRP(237) = dARP(132)/dV(81)
  JVRP(237) = V(69)
! JVRP(238) = dARP(133)/dV(73)
  JVRP(238) = V(81)
! JVRP(239) = dARP(133)/dV(81)
  JVRP(239) = V(73)
! JVRP(240) = dARP(134)/dV(51)
  JVRP(240) = V(81)
! JVRP(241) = dARP(134)/dV(81)
  JVRP(241) = V(51)
! JVRP(242) = dARP(135)/dV(52)
  JVRP(242) = V(81)
! JVRP(243) = dARP(135)/dV(81)
  JVRP(243) = V(52)
! JVRP(244) = dARP(136)/dV(55)
  JVRP(244) = V(81)
! JVRP(245) = dARP(136)/dV(81)
  JVRP(245) = V(55)
! JVRP(246) = dARP(137)/dV(56)
  JVRP(246) = V(81)
! JVRP(247) = dARP(137)/dV(81)
  JVRP(247) = V(56)
! JVRP(248) = dARP(138)/dV(53)
  JVRP(248) = V(81)
! JVRP(249) = dARP(138)/dV(81)
  JVRP(249) = V(53)
! JVRP(250) = dARP(139)/dV(71)
  JVRP(250) = V(81)
! JVRP(251) = dARP(139)/dV(81)
  JVRP(251) = V(71)
! JVRP(252) = dARP(140)/dV(81)
  JVRP(252) = 2*V(81)
! JVRP(253) = dARP(141)/dV(58)
  JVRP(253) = V(81)
! JVRP(254) = dARP(141)/dV(81)
  JVRP(254) = V(58)
! JVRP(255) = dARP(142)/dV(48)
  JVRP(255) = V(81)
! JVRP(256) = dARP(142)/dV(81)
  JVRP(256) = V(48)
! JVRP(257) = dARP(143)/dV(64)
  JVRP(257) = V(81)
! JVRP(258) = dARP(143)/dV(81)
  JVRP(258) = V(64)
! JVRP(259) = dARP(144)/dV(57)
  JVRP(259) = V(75)
! JVRP(260) = dARP(144)/dV(75)
  JVRP(260) = V(57)
! JVRP(261) = dARP(145)/dV(57)
  JVRP(261) = V(79)
! JVRP(262) = dARP(145)/dV(79)
  JVRP(262) = V(57)
! JVRP(263) = dARP(146)/dV(57)
  JVRP(263) = V(81)
! JVRP(264) = dARP(146)/dV(81)
  JVRP(264) = V(57)
! JVRP(265) = dARP(147)/dV(57)
  JVRP(265) = V(77)
! JVRP(266) = dARP(147)/dV(77)
  JVRP(266) = V(57)
! JVRP(267) = dARP(148)/dV(47)
  JVRP(267) = V(76)
! JVRP(268) = dARP(148)/dV(76)
  JVRP(268) = V(47)
! JVRP(269) = dARP(149)/dV(47)
  JVRP(269) = V(75)
! JVRP(270) = dARP(149)/dV(75)
  JVRP(270) = V(47)
! JVRP(271) = dARP(150)/dV(47)
  JVRP(271) = V(79)
! JVRP(272) = dARP(150)/dV(79)
  JVRP(272) = V(47)
! JVRP(273) = dARP(151)/dV(47)
  JVRP(273) = V(81)
! JVRP(274) = dARP(151)/dV(81)
  JVRP(274) = V(47)
! JVRP(275) = dARP(152)/dV(61)
  JVRP(275) = V(79)
! JVRP(276) = dARP(152)/dV(79)
  JVRP(276) = V(61)
! JVRP(277) = dARP(153)/dV(61)
  JVRP(277) = V(81)
! JVRP(278) = dARP(153)/dV(81)
  JVRP(278) = V(61)
! JVRP(279) = dARP(154)/dV(61)
  JVRP(279) = 2*V(61)
! JVRP(280) = dARP(155)/dV(40)
  JVRP(280) = V(72)
! JVRP(281) = dARP(155)/dV(72)
  JVRP(281) = V(40)
! JVRP(282) = dARP(156)/dV(40)
  JVRP(282) = V(80)
! JVRP(283) = dARP(156)/dV(80)
  JVRP(283) = V(40)
! JVRP(284) = dARP(157)/dV(40)
  JVRP(284) = V(78)
! JVRP(285) = dARP(157)/dV(78)
  JVRP(285) = V(40)
! JVRP(286) = dARP(158)/dV(65)
  JVRP(286) = V(77)
! JVRP(287) = dARP(158)/dV(77)
  JVRP(287) = V(65)
! JVRP(288) = dARP(159)/dV(65)
  JVRP(288) = V(75)
! JVRP(289) = dARP(159)/dV(75)
  JVRP(289) = V(65)
! JVRP(290) = dARP(160)/dV(65)
  JVRP(290) = V(79)
! JVRP(291) = dARP(160)/dV(79)
  JVRP(291) = V(65)
! JVRP(292) = dARP(161)/dV(65)
  JVRP(292) = V(81)
! JVRP(293) = dARP(161)/dV(81)
  JVRP(293) = V(65)
! JVRP(294) = dARP(162)/dV(65)
  JVRP(294) = V(80)
! JVRP(295) = dARP(162)/dV(80)
  JVRP(295) = V(65)
! JVRP(296) = dARP(163)/dV(45)
  JVRP(296) = V(72)
! JVRP(297) = dARP(163)/dV(72)
  JVRP(297) = V(45)
! JVRP(298) = dARP(164)/dV(45)
  JVRP(298) = V(80)
! JVRP(299) = dARP(164)/dV(80)
  JVRP(299) = V(45)
! JVRP(300) = dARP(165)/dV(45)
  JVRP(300) = V(78)
! JVRP(301) = dARP(165)/dV(78)
  JVRP(301) = V(45)
! JVRP(302) = dARP(166)/dV(67)
  JVRP(302) = V(77)
! JVRP(303) = dARP(166)/dV(77)
  JVRP(303) = V(67)
! JVRP(304) = dARP(167)/dV(67)
  JVRP(304) = V(75)
! JVRP(305) = dARP(167)/dV(75)
  JVRP(305) = V(67)
! JVRP(306) = dARP(168)/dV(67)
  JVRP(306) = V(79)
! JVRP(307) = dARP(168)/dV(79)
  JVRP(307) = V(67)
! JVRP(308) = dARP(169)/dV(67)
  JVRP(308) = V(81)
! JVRP(309) = dARP(169)/dV(81)
  JVRP(309) = V(67)
! JVRP(310) = dARP(170)/dV(67)
  JVRP(310) = V(80)
! JVRP(311) = dARP(170)/dV(80)
  JVRP(311) = V(67)
! JVRP(312) = dARP(171)/dV(37)
  JVRP(312) = V(77)
! JVRP(313) = dARP(171)/dV(77)
  JVRP(313) = V(37)
! JVRP(314) = dARP(172)/dV(37)
  JVRP(314) = V(75)
! JVRP(315) = dARP(172)/dV(75)
  JVRP(315) = V(37)
! JVRP(316) = dARP(173)/dV(37)
  JVRP(316) = 2*V(37)
! JVRP(317) = dARP(174)/dV(10)
  JVRP(317) = V(72)
! JVRP(318) = dARP(174)/dV(72)
  JVRP(318) = V(10)
! JVRP(319) = dARP(175)/dV(38)
  JVRP(319) = V(72)
! JVRP(320) = dARP(175)/dV(72)
  JVRP(320) = V(38)
! JVRP(321) = dARP(176)/dV(49)
  JVRP(321) = V(72)
! JVRP(322) = dARP(176)/dV(72)
  JVRP(322) = V(49)
! JVRP(323) = dARP(177)/dV(49)
  JVRP(323) = V(78)
! JVRP(324) = dARP(177)/dV(78)
  JVRP(324) = V(49)
! JVRP(325) = dARP(178)/dV(60)
  JVRP(325) = V(77)
! JVRP(326) = dARP(178)/dV(77)
  JVRP(326) = V(60)
! JVRP(327) = dARP(179)/dV(60)
  JVRP(327) = V(75)
! JVRP(328) = dARP(179)/dV(75)
  JVRP(328) = V(60)
! JVRP(329) = dARP(180)/dV(60)
  JVRP(329) = 2*V(60)
! JVRP(330) = dARP(181)/dV(60)
  JVRP(330) = V(76)
! JVRP(331) = dARP(181)/dV(76)
  JVRP(331) = V(60)
! JVRP(332) = dARP(182)/dV(27)
  JVRP(332) = 1
! JVRP(333) = dARP(183)/dV(27)
  JVRP(333) = V(72)
! JVRP(334) = dARP(183)/dV(72)
  JVRP(334) = V(27)
! JVRP(335) = dARP(184)/dV(15)
  JVRP(335) = V(72)
! JVRP(336) = dARP(184)/dV(72)
  JVRP(336) = V(15)
! JVRP(337) = dARP(185)/dV(31)
  JVRP(337) = V(72)
! JVRP(338) = dARP(185)/dV(72)
  JVRP(338) = V(31)
! JVRP(339) = dARP(186)/dV(21)
  JVRP(339) = V(72)
! JVRP(340) = dARP(186)/dV(72)
  JVRP(340) = V(21)
! JVRP(341) = dARP(187)/dV(16)
  JVRP(341) = V(80)
! JVRP(342) = dARP(187)/dV(80)
  JVRP(342) = V(16)
! JVRP(343) = dARP(188)/dV(16)
  JVRP(343) = V(72)
! JVRP(344) = dARP(188)/dV(72)
  JVRP(344) = V(16)
! JVRP(345) = dARP(189)/dV(8)
  JVRP(345) = V(72)
! JVRP(346) = dARP(189)/dV(72)
  JVRP(346) = V(8)
! JVRP(347) = dARP(190)/dV(22)
  JVRP(347) = V(72)
! JVRP(348) = dARP(190)/dV(72)
  JVRP(348) = V(22)
! JVRP(349) = dARP(191)/dV(23)
  JVRP(349) = V(72)
! JVRP(350) = dARP(191)/dV(72)
  JVRP(350) = V(23)
! JVRP(351) = dARP(192)/dV(24)
  JVRP(351) = V(72)
! JVRP(352) = dARP(192)/dV(72)
  JVRP(352) = V(24)
      
END SUBROUTINE JacReactantProd

! End of JacReactantProd function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



! Begin Derivative w.r.t. Rate Coefficients

! ------------------------------------------------------------------------------
! Subroutine for the derivative of Fun with respect to rate coefficients
! -----------------------------------------------------------------------------

      SUBROUTINE  dFun_dRcoeff( V, F, NCOEFF, JCOEFF, DFDR )
       
      USE box_model_Parameters
      USE box_model_StoichiomSP
      IMPLICIT NONE 

! V - Concentrations of variable/radical/fixed species            
      REAL(kind=dp) V(NVAR), F(NFIX)
! NCOEFF - the number of rate coefficients with respect to which we differentiate
      INTEGER NCOEFF       
! JCOEFF - a vector of integers containing the indices of reactions (rate
!          coefficients) with respect to which we differentiate
      INTEGER JCOEFF(NCOEFF)       
! DFDR  - a matrix containg derivative values; specifically, 
!         column j contains d Fun(1:NVAR) / d RCT( JCOEFF(j) )
!         for each 1 <= j <= NCOEFF
!         This matrix is stored in a column-wise linearized format
      REAL(kind=dp) DFDR(NVAR*NCOEFF)

! Local vector with reactant products
      REAL(kind=dp) A_RPROD(NREACT)
      REAL(kind=dp) aj
      INTEGER i,j,k
      
! Compute the reactant products of all reactions     
      CALL ReactantProd ( V, F, A_RPROD )

! Compute the derivatives by multiplying column JCOEFF(j) of the stoichiometric matrix with A_RPROD       
      DO j=1,NCOEFF
!                  Initialize the j-th column of derivative matrix to zero       
         DO i=1,NVAR
           DFDR(i+NVAR*(j-1)) = 0.0_dp 
         END DO
!                  Column JCOEFF(j) in the stoichiometric matrix times the
!                  reactant product  of the JCOEFF(j)-th reaction      
!                  give the j-th column of the derivative matrix   
         aj = A_RPROD(JCOEFF(j))
         DO k=CCOL_STOICM(JCOEFF(j)),CCOL_STOICM(JCOEFF(j)+1)-1
           DFDR(IROW_STOICM(k)+NVAR*(j-1)) = STOICM(k)*aj
         END DO
      END DO
      
      END SUBROUTINE  dFun_dRcoeff

! End Derivative w.r.t. Rate Coefficients


! Begin Jacobian Derivative w.r.t. Rate Coefficients

! ------------------------------------------------------------------------------
! Subroutine for the derivative of Jac with respect to rate coefficients
! Times a user vector
! -----------------------------------------------------------------------------

      SUBROUTINE  dJac_dRcoeff( V, F, U, NCOEFF, JCOEFF, DJDR )
       
      USE box_model_Parameters
      USE box_model_StoichiomSP
      IMPLICIT NONE 

! V - Concentrations of variable/fixed species            
      REAL(kind=dp) V(NVAR), F(NFIX)
! U - User-supplied Vector           
      REAL(kind=dp) U(NVAR)
! NCOEFF - the number of rate coefficients with respect to which we differentiate
      INTEGER NCOEFF       
! JCOEFF - a vector of integers containing the indices of reactions (rate
!          coefficients) with respect to which we differentiate
      INTEGER JCOEFF(NCOEFF)       
! DFDR  - a matrix containg derivative values; specifically, 
!         column j contains d Jac(1:NVAR) / d RCT( JCOEFF(j) ) * U
!                     for each 1 <= j <= NCOEFF
!         This matrix is stored in a column-wise linearized format
      REAL(kind=dp) DJDR(NVAR*NCOEFF)

! Local vector for Jacobian of reactant products
      REAL(kind=dp) JV_RPROD(NJVRP)
      REAL(kind=dp) aj
      INTEGER i,j,k
      
! Compute the Jacobian of all reactant products   
      CALL JacReactantProd( V, F, JV_RPROD )

! Compute the derivatives by multiplying column JCOEFF(j) of the stoichiometric matrix with A_PROD       
      DO j=1,NCOEFF
!                  Initialize the j-th column of derivative matrix to zero       
         DO i=1,NVAR
           DJDR(i+NVAR*(j-1)) = 0.0_dp
         END DO
!                  Column JCOEFF(j) in the stoichiometric matrix times the
!                  ( Gradient of reactant product of the JCOEFF(j)-th reaction X user vector )    
!                  give the j-th column of the derivative matrix   
!
!          Row JCOEFF(j) of JV_RPROD times the user vector
         aj = 0.0_dp
         DO k=CROW_JVRP(JCOEFF(j)),CROW_JVRP(JCOEFF(j)+1)-1
             aj = aj + JV_RPROD(k)*U(ICOL_JVRP(k))
         END DO
!          Column JCOEFF(j) of Stoichiom. matrix times aj         
         DO k=CCOL_STOICM(JCOEFF(j)),CCOL_STOICM(JCOEFF(j)+1)-1
           DJDR(IROW_STOICM(k)+NVAR*(j-1)) = STOICM(k)*aj
         END DO
      END DO
      
      END SUBROUTINE  dJac_dRcoeff

! End Jacobian Derivative w.r.t. Rate Coefficients


END MODULE box_model_Stoichiom
