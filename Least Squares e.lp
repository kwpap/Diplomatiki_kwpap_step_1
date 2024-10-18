\ Model Least Squares e
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  quad_aux_6799 + quad_aux_6803 + quad_aux_6807 + quad_aux_6811
   + quad_aux_6815 + quad_aux_6819 + quad_aux_6823 + 100 quad_aux_6827
   + 121 quad_aux_6831 + 10000 quad_aux_6833 + 10000 quad_aux_6835
   + 10000 quad_aux_6837 + 22500 quad_aux_6839 + 22500 quad_aux_6841
   + 22500 quad_aux_6843 + 40000 quad_aux_6849 + 40000 quad_aux_6855
   + 40000 quad_aux_6861
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
   + qq7 - ab7 + qq8 - ab8 + qq9 - ab9 = 117.3289016365619
 R19: - ab5 + pow_base_aux_6796 = 0
 R20: 12 ab5 - ppp + 6 quad_aux_6797 + pow_base_aux_6798 = -1
 R21: - ab4 + pow_base_aux_6800 = 0
 R22: 10 ab4 - ppp + 9 quad_aux_6801 + pow_base_aux_6802 = -7
 R23: - ab9 + pow_base_aux_6804 = 0
 R24: 20 ab9 - ppp + 33 quad_aux_6805 + pow_base_aux_6806 = -5
 R25: - ab3 + pow_base_aux_6808 = 0
 R26: 8 ab3 - ppp + 15 quad_aux_6809 + pow_base_aux_6810 = -5
 R27: - ab8 + pow_base_aux_6812 = 0
 R28: 18 ab8 - ppp + 30 quad_aux_6813 + pow_base_aux_6814 = -4
 R29: - ab7 + pow_base_aux_6816 = 0
 R30: 16 ab7 - ppp + 12 quad_aux_6817 + pow_base_aux_6818 = -3
 R31: - ab6 + pow_base_aux_6820 = 0
 R32: 14 ab6 - ppp + 9 quad_aux_6821 + pow_base_aux_6822 = -2
 R33: - ab1 + pow_base_aux_6824 = 0
 R34: 0.4 ab1 - 0.1 ppp + 0.03 quad_aux_6825 + pow_base_aux_6826 = -1
 R35: - ab2 + pow_base_aux_6828 = 0
 R36: 0.5454545454545454 ab2 - 0.0909090909090909 ppp
   + 0.0545454545454545 quad_aux_6829 + pow_base_aux_6830 = -1
 R37: 0.001 qq1 + 0.001 qq2 + 0.002 qq3 + 0.01 ppp + pow_base_aux_6832 = 1
 R38: 0.001 qq1 + 0.002 qq2 + 0.001 qq3 + 0.01 ppp + pow_base_aux_6834 = 1
 R39: 0.002 qq1 + 0.001 qq2 + 0.001 qq3 + 0.01 ppp + pow_base_aux_6836 = 1
 R40: 6.6666666666666664e-04 qq4 + 6.6666666666666664e-04 qq5
   + 0.00133333333333333 qq6 + 0.00666666666666667 ppp + pow_base_aux_6838
   = 1
 R41: 6.6666666666666664e-04 qq4 + 0.00133333333333333 qq5
   + 6.6666666666666664e-04 qq6 + 0.00666666666666667 ppp
   + pow_base_aux_6840 = 1
 R42: 0.00133333333333333 qq4 + 6.6666666666666664e-04 qq5
   + 6.6666666666666664e-04 qq6 + 0.00666666666666667 ppp
   + pow_base_aux_6842 = 1
 R43: - qq7 - qq8 - qq9 + pow_base_aux_6844 = 0
 R44: - qq7 - qq8 - qq9 + pow_base_aux_6846 = 0
 R45: - qq7 - qq8 - qq9 + pow_base_aux_6850 = 0
 R46: - qq7 - qq8 - qq9 + pow_base_aux_6852 = 0
 R47: - qq7 - qq8 - qq9 + pow_base_aux_6856 = 0
 R48: - qq7 - qq8 - qq9 + pow_base_aux_6858 = 0
 qc0: quad_aux_6797 + [ - pow_base_aux_6796 ^2 ] = 0
 qc1: quad_aux_6799 + [ - pow_base_aux_6798 ^2 ] = 0
 qc2: quad_aux_6801 + [ - pow_base_aux_6800 ^2 ] = 0
 qc3: quad_aux_6803 + [ - pow_base_aux_6802 ^2 ] = 0
 qc4: quad_aux_6805 + [ - pow_base_aux_6804 ^2 ] = 0
 qc5: quad_aux_6807 + [ - pow_base_aux_6806 ^2 ] = 0
 qc6: quad_aux_6809 + [ - pow_base_aux_6808 ^2 ] = 0
 qc7: quad_aux_6811 + [ - pow_base_aux_6810 ^2 ] = 0
 qc8: quad_aux_6813 + [ - pow_base_aux_6812 ^2 ] = 0
 qc9: quad_aux_6815 + [ - pow_base_aux_6814 ^2 ] = 0
 qc10: quad_aux_6817 + [ - pow_base_aux_6816 ^2 ] = 0
 qc11: quad_aux_6819 + [ - pow_base_aux_6818 ^2 ] = 0
 qc12: quad_aux_6821 + [ - pow_base_aux_6820 ^2 ] = 0
 qc13: quad_aux_6823 + [ - pow_base_aux_6822 ^2 ] = 0
 qc14: quad_aux_6825 + [ - pow_base_aux_6824 ^2 ] = 0
 qc15: quad_aux_6827 + [ - pow_base_aux_6826 ^2 ] = 0
 qc16: quad_aux_6829 + [ - pow_base_aux_6828 ^2 ] = 0
 qc17: quad_aux_6831 + [ - pow_base_aux_6830 ^2 ] = 0
 qc18: quad_aux_6833 + [ - pow_base_aux_6832 ^2 ] = 0
 qc19: quad_aux_6835 + [ - pow_base_aux_6834 ^2 ] = 0
 qc20: quad_aux_6837 + [ - pow_base_aux_6836 ^2 ] = 0
 qc21: quad_aux_6839 + [ - pow_base_aux_6838 ^2 ] = 0
 qc22: quad_aux_6841 + [ - pow_base_aux_6840 ^2 ] = 0
 qc23: quad_aux_6843 + [ - pow_base_aux_6842 ^2 ] = 0
 qc24: 0.005 ppp + 0.0001 pow_aux_6845 + pow_base_aux_6848 + [
   0.00015 qq7 * pow_aux_6847 ] = 1
 qc25: quad_aux_6849 + [ - pow_base_aux_6848 ^2 ] = 0
 qc26: 0.005 ppp + 0.0001 pow_aux_6851 + pow_base_aux_6854 + [
   0.00015 qq8 * pow_aux_6853 ] = 1
 qc27: quad_aux_6855 + [ - pow_base_aux_6854 ^2 ] = 0
 qc28: 0.005 ppp + 0.0001 pow_aux_6857 + pow_base_aux_6860 + [
   0.00015 qq9 * pow_aux_6859 ] = 1
 qc29: quad_aux_6861 + [ - pow_base_aux_6860 ^2 ] = 0
Bounds
General Constraints
 GC0: pow_aux_6845 = POW ( pow_base_aux_6844 ^ 1.5 )
 GC1: pow_aux_6847 = POW ( pow_base_aux_6846 ^ 0.5 )
 GC2: pow_aux_6851 = POW ( pow_base_aux_6850 ^ 1.5 )
 GC3: pow_aux_6853 = POW ( pow_base_aux_6852 ^ 0.5 )
 GC4: pow_aux_6857 = POW ( pow_base_aux_6856 ^ 1.5 )
 GC5: pow_aux_6859 = POW ( pow_base_aux_6858 ^ 0.5 )
End
