\ Model Least Squares e
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  quad_aux_6681 + quad_aux_6685 + quad_aux_6689 + quad_aux_6693
   + quad_aux_6697 + quad_aux_6701 + quad_aux_6705 + quad_aux_6709
   + quad_aux_6713 + 10000 quad_aux_6715 + 10000 quad_aux_6717
   + 10000 quad_aux_6719 + 22500 quad_aux_6721 + 22500 quad_aux_6723
   + 22500 quad_aux_6725 + 40000 quad_aux_6731 + 40000 quad_aux_6737
   + 40000 quad_aux_6743
Subject To
 R0: qq1 - ab1 >= 0
 R1: ab1 >= 0
 R2: qq2 - ab2 >= 0
 R3: ab2 >= 0
 R4: qq3 - ab3 >= 0
 R5: ab3 >= 0
 R6: qq4 - ab4 >= 0
 R7: ab4 >= 0
 R8: qq5 - ab5 >= 0
 R9: ab5 >= 0
 R10: qq6 - ab6 >= 0
 R11: ab6 >= 0
 R12: qq7 - ab7 >= 0
 R13: ab7 >= 0
 R14: qq8 - ab8 >= 0
 R15: ab8 >= 0
 R16: qq9 - ab9 >= 0
 R17: ab9 >= 0
 R18: qq1 - ab1 + qq2 - ab2 + qq3 - ab3 + qq4 - ab4 + qq5 - ab5 + qq6 - ab6
   + qq7 - ab7 + qq8 - ab8 + qq9 - ab9 = 1784.68072606113
 R19: - ab5 + pow_base_aux_6678 = 0
 R20: 12 ab5 - ppp + 6 quad_aux_6679 + pow_base_aux_6680 = -1
 R21: - ab3 + pow_base_aux_6682 = 0
 R22: 6 ab3 - ppp + 6 quad_aux_6683 + pow_base_aux_6684 = -1
 R23: - ab4 + pow_base_aux_6686 = 0
 R24: 10 ab4 - ppp + 9 quad_aux_6687 + pow_base_aux_6688 = -7
 R25: - ab9 + pow_base_aux_6690 = 0
 R26: 20 ab9 - ppp + 33 quad_aux_6691 + pow_base_aux_6692 = -5
 R27: - ab8 + pow_base_aux_6694 = 0
 R28: 18 ab8 - ppp + 30 quad_aux_6695 + pow_base_aux_6696 = -4
 R29: - ab7 + pow_base_aux_6698 = 0
 R30: 16 ab7 - ppp + 12 quad_aux_6699 + pow_base_aux_6700 = -3
 R31: - ab2 + pow_base_aux_6702 = 0
 R32: 6 ab2 - ppp + 6 quad_aux_6703 + pow_base_aux_6704 = -3
 R33: - ab6 + pow_base_aux_6706 = 0
 R34: 14 ab6 - ppp + 9 quad_aux_6707 + pow_base_aux_6708 = -2
 R35: - ab1 + pow_base_aux_6710 = 0
 R36: 4 ab1 - ppp + 6 quad_aux_6711 + pow_base_aux_6712 = -2
 R37: 0.001 qq1 + 0.001 qq2 + 0.002 qq3 + 0.01 ppp + pow_base_aux_6714 = 1
 R38: 0.001 qq1 + 0.002 qq2 + 0.001 qq3 + 0.01 ppp + pow_base_aux_6716 = 1
 R39: 0.002 qq1 + 0.001 qq2 + 0.001 qq3 + 0.01 ppp + pow_base_aux_6718 = 1
 R40: 6.6666666666666664e-04 qq4 + 6.6666666666666664e-04 qq5
   + 0.00133333333333333 qq6 + 0.00666666666666667 ppp + pow_base_aux_6720
   = 1
 R41: 6.6666666666666664e-04 qq4 + 0.00133333333333333 qq5
   + 6.6666666666666664e-04 qq6 + 0.00666666666666667 ppp
   + pow_base_aux_6722 = 1
 R42: 0.00133333333333333 qq4 + 6.6666666666666664e-04 qq5
   + 6.6666666666666664e-04 qq6 + 0.00666666666666667 ppp
   + pow_base_aux_6724 = 1
 R43: - qq7 - qq8 - qq9 + pow_base_aux_6726 = 0
 R44: - qq7 - qq8 - qq9 + pow_base_aux_6728 = 0
 R45: - qq7 - qq8 - qq9 + pow_base_aux_6732 = 0
 R46: - qq7 - qq8 - qq9 + pow_base_aux_6734 = 0
 R47: - qq7 - qq8 - qq9 + pow_base_aux_6738 = 0
 R48: - qq7 - qq8 - qq9 + pow_base_aux_6740 = 0
 qc0: quad_aux_6679 + [ - pow_base_aux_6678 ^2 ] = 0
 qc1: quad_aux_6681 + [ - pow_base_aux_6680 ^2 ] = 0
 qc2: quad_aux_6683 + [ - pow_base_aux_6682 ^2 ] = 0
 qc3: quad_aux_6685 + [ - pow_base_aux_6684 ^2 ] = 0
 qc4: quad_aux_6687 + [ - pow_base_aux_6686 ^2 ] = 0
 qc5: quad_aux_6689 + [ - pow_base_aux_6688 ^2 ] = 0
 qc6: quad_aux_6691 + [ - pow_base_aux_6690 ^2 ] = 0
 qc7: quad_aux_6693 + [ - pow_base_aux_6692 ^2 ] = 0
 qc8: quad_aux_6695 + [ - pow_base_aux_6694 ^2 ] = 0
 qc9: quad_aux_6697 + [ - pow_base_aux_6696 ^2 ] = 0
 qc10: quad_aux_6699 + [ - pow_base_aux_6698 ^2 ] = 0
 qc11: quad_aux_6701 + [ - pow_base_aux_6700 ^2 ] = 0
 qc12: quad_aux_6703 + [ - pow_base_aux_6702 ^2 ] = 0
 qc13: quad_aux_6705 + [ - pow_base_aux_6704 ^2 ] = 0
 qc14: quad_aux_6707 + [ - pow_base_aux_6706 ^2 ] = 0
 qc15: quad_aux_6709 + [ - pow_base_aux_6708 ^2 ] = 0
 qc16: quad_aux_6711 + [ - pow_base_aux_6710 ^2 ] = 0
 qc17: quad_aux_6713 + [ - pow_base_aux_6712 ^2 ] = 0
 qc18: quad_aux_6715 + [ - pow_base_aux_6714 ^2 ] = 0
 qc19: quad_aux_6717 + [ - pow_base_aux_6716 ^2 ] = 0
 qc20: quad_aux_6719 + [ - pow_base_aux_6718 ^2 ] = 0
 qc21: quad_aux_6721 + [ - pow_base_aux_6720 ^2 ] = 0
 qc22: quad_aux_6723 + [ - pow_base_aux_6722 ^2 ] = 0
 qc23: quad_aux_6725 + [ - pow_base_aux_6724 ^2 ] = 0
 qc24: 0.005 ppp + 0.0001 pow_aux_6727 + pow_base_aux_6730 + [
   0.00015 qq7 * pow_aux_6729 ] = 1
 qc25: quad_aux_6731 + [ - pow_base_aux_6730 ^2 ] = 0
 qc26: 0.005 ppp + 0.0001 pow_aux_6733 + pow_base_aux_6736 + [
   0.00015 qq8 * pow_aux_6735 ] = 1
 qc27: quad_aux_6737 + [ - pow_base_aux_6736 ^2 ] = 0
 qc28: 0.005 ppp + 0.0001 pow_aux_6739 + pow_base_aux_6742 + [
   0.00015 qq9 * pow_aux_6741 ] = 1
 qc29: quad_aux_6743 + [ - pow_base_aux_6742 ^2 ] = 0
Bounds
General Constraints
 GC0: pow_aux_6727 = POW ( pow_base_aux_6726 ^ 1.5 )
 GC1: pow_aux_6729 = POW ( pow_base_aux_6728 ^ 0.5 )
 GC2: pow_aux_6733 = POW ( pow_base_aux_6732 ^ 1.5 )
 GC3: pow_aux_6735 = POW ( pow_base_aux_6734 ^ 0.5 )
 GC4: pow_aux_6739 = POW ( pow_base_aux_6738 ^ 1.5 )
 GC5: pow_aux_6741 = POW ( pow_base_aux_6740 ^ 0.5 )
End
