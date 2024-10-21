\ Model Least Squares e
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  quad_aux_741 + quad_aux_745 + quad_aux_749 + quad_aux_753 + quad_aux_757
   + quad_aux_761 + quad_aux_765 + quad_aux_769 + quad_aux_773
   + 10000 quad_aux_775 + 10000 quad_aux_777 + 10000 quad_aux_779
   + 22500 quad_aux_781 + 22500 quad_aux_783 + 22500 quad_aux_785
   + 40000 quad_aux_791 + 40000 quad_aux_797 + 40000 quad_aux_803
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
 R19: - ab5 + pow_base_aux_738 = 0
 R20: 12 ab5 - ppp + 6 quad_aux_739 + pow_base_aux_740 = -1
 R21: - ab2 + pow_base_aux_742 = 0
 R22: 6 ab2 - ppp + 6 quad_aux_743 + pow_base_aux_744 = -11
 R23: - ab1 + pow_base_aux_746 = 0
 R24: 4 ab1 - ppp + 6 quad_aux_747 + pow_base_aux_748 = -10
 R25: - ab4 + pow_base_aux_750 = 0
 R26: 10 ab4 - ppp + 9 quad_aux_751 + pow_base_aux_752 = -7
 R27: - ab9 + pow_base_aux_754 = 0
 R28: 20 ab9 - ppp + 33 quad_aux_755 + pow_base_aux_756 = -5
 R29: - ab3 + pow_base_aux_758 = 0
 R30: 8 ab3 - ppp + 15 quad_aux_759 + pow_base_aux_760 = -5
 R31: - ab8 + pow_base_aux_762 = 0
 R32: 18 ab8 - ppp + 30 quad_aux_763 + pow_base_aux_764 = -4
 R33: - ab7 + pow_base_aux_766 = 0
 R34: 16 ab7 - ppp + 12 quad_aux_767 + pow_base_aux_768 = -3
 R35: - ab6 + pow_base_aux_770 = 0
 R36: 14 ab6 - ppp + 9 quad_aux_771 + pow_base_aux_772 = -2
 R37: 0.001 qq1 + 0.001 qq2 + 0.002 qq3 + 0.00929292929292929 ppp
   + pow_base_aux_774 = 1
 R38: 0.001 qq1 + 0.002 qq2 + 0.001 qq3 + 0.00929292929292929 ppp
   + pow_base_aux_776 = 1
 R39: 0.002 qq1 + 0.001 qq2 + 0.001 qq3 + 0.00929292929292929 ppp
   + pow_base_aux_778 = 1
 R40: 6.6666666666666664e-04 qq4 + 6.6666666666666664e-04 qq5
   + 0.00133333333333333 qq6 + 0.00666666666666667 ppp + pow_base_aux_780
   = 1
 R41: 6.6666666666666664e-04 qq4 + 0.00133333333333333 qq5
   + 6.6666666666666664e-04 qq6 + 0.00666666666666667 ppp
   + pow_base_aux_782 = 1
 R42: 0.00133333333333333 qq4 + 6.6666666666666664e-04 qq5
   + 6.6666666666666664e-04 qq6 + 0.00666666666666667 ppp
   + pow_base_aux_784 = 1
 R43: - qq7 - qq8 - qq9 + pow_base_aux_786 = 0
 R44: - qq7 - qq8 - qq9 + pow_base_aux_788 = 0
 R45: - qq7 - qq8 - qq9 + pow_base_aux_792 = 0
 R46: - qq7 - qq8 - qq9 + pow_base_aux_794 = 0
 R47: - qq7 - qq8 - qq9 + pow_base_aux_798 = 0
 R48: - qq7 - qq8 - qq9 + pow_base_aux_800 = 0
 qc0: quad_aux_739 + [ - pow_base_aux_738 ^2 ] = 0
 qc1: quad_aux_741 + [ - pow_base_aux_740 ^2 ] = 0
 qc2: quad_aux_743 + [ - pow_base_aux_742 ^2 ] = 0
 qc3: quad_aux_745 + [ - pow_base_aux_744 ^2 ] = 0
 qc4: quad_aux_747 + [ - pow_base_aux_746 ^2 ] = 0
 qc5: quad_aux_749 + [ - pow_base_aux_748 ^2 ] = 0
 qc6: quad_aux_751 + [ - pow_base_aux_750 ^2 ] = 0
 qc7: quad_aux_753 + [ - pow_base_aux_752 ^2 ] = 0
 qc8: quad_aux_755 + [ - pow_base_aux_754 ^2 ] = 0
 qc9: quad_aux_757 + [ - pow_base_aux_756 ^2 ] = 0
 qc10: quad_aux_759 + [ - pow_base_aux_758 ^2 ] = 0
 qc11: quad_aux_761 + [ - pow_base_aux_760 ^2 ] = 0
 qc12: quad_aux_763 + [ - pow_base_aux_762 ^2 ] = 0
 qc13: quad_aux_765 + [ - pow_base_aux_764 ^2 ] = 0
 qc14: quad_aux_767 + [ - pow_base_aux_766 ^2 ] = 0
 qc15: quad_aux_769 + [ - pow_base_aux_768 ^2 ] = 0
 qc16: quad_aux_771 + [ - pow_base_aux_770 ^2 ] = 0
 qc17: quad_aux_773 + [ - pow_base_aux_772 ^2 ] = 0
 qc18: quad_aux_775 + [ - pow_base_aux_774 ^2 ] = 0
 qc19: quad_aux_777 + [ - pow_base_aux_776 ^2 ] = 0
 qc20: quad_aux_779 + [ - pow_base_aux_778 ^2 ] = 0
 qc21: quad_aux_781 + [ - pow_base_aux_780 ^2 ] = 0
 qc22: quad_aux_783 + [ - pow_base_aux_782 ^2 ] = 0
 qc23: quad_aux_785 + [ - pow_base_aux_784 ^2 ] = 0
 qc24: 0.005 ppp + 0.0001 pow_aux_787 + pow_base_aux_790 + [
   0.00015 qq7 * pow_aux_789 ] = 1
 qc25: quad_aux_791 + [ - pow_base_aux_790 ^2 ] = 0
 qc26: 0.005 ppp + 0.0001 pow_aux_793 + pow_base_aux_796 + [
   0.00015 qq8 * pow_aux_795 ] = 1
 qc27: quad_aux_797 + [ - pow_base_aux_796 ^2 ] = 0
 qc28: 0.005 ppp + 0.0001 pow_aux_799 + pow_base_aux_802 + [
   0.00015 qq9 * pow_aux_801 ] = 1
 qc29: quad_aux_803 + [ - pow_base_aux_802 ^2 ] = 0
Bounds
General Constraints
 GC0: pow_aux_787 = POW ( pow_base_aux_786 ^ 1.5 )
 GC1: pow_aux_789 = POW ( pow_base_aux_788 ^ 0.5 )
 GC2: pow_aux_793 = POW ( pow_base_aux_792 ^ 1.5 )
 GC3: pow_aux_795 = POW ( pow_base_aux_794 ^ 0.5 )
 GC4: pow_aux_799 = POW ( pow_base_aux_798 ^ 1.5 )
 GC5: pow_aux_801 = POW ( pow_base_aux_800 ^ 0.5 )
End
