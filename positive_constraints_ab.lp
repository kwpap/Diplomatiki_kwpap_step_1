\ Model positive_constraints_ab
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  - 0.4 qq1 - 4 ab1 - 0.4 qq2 - 6 ab2 - 0.4 qq3 - 8 ab3 - 0.4 qq4 - 10 ab4
   - 0.4 qq5 - 12 ab5 - 0.4 qq6 - 14 ab6 - 16 ab7 - 18 ab8 - 20 ab9
   - 33 quad_aux_6875 - 30 quad_aux_6877 - 15 quad_aux_6879
   - 12 quad_aux_6881 - 9 quad_aux_6883 - 9 quad_aux_6885 - 6 quad_aux_6887
   - 0.06 pow_aux_6889 - 0.3 quad_aux_6891 - 0.6 quad_aux_6893
   + 1302 Constant + [ - 0.06 qq7 * pow_aux_6895 - 0.06 qq8 * pow_aux_6897
   - 0.06 qq9 * pow_aux_6899 ] / 2 
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
 R18: - 0.2 qq1 - 0.1 qq2 - 0.1 qq3 - ppp >= -100
 R19: - 0.1 qq1 - 0.2 qq2 - 0.1 qq3 - ppp >= -100
 R20: - 0.1 qq1 - 0.1 qq2 - 0.2 qq3 - ppp >= -100
 R21: - 0.2 qq4 - 0.1 qq5 - 0.1 qq6 - ppp >= -150
 R22: - 0.1 qq4 - 0.2 qq5 - 0.1 qq6 - ppp >= -150
 R23: - 0.1 qq4 - 0.1 qq5 - 0.2 qq6 - ppp >= -150
 R24: - qq7 - qq8 - qq9 + pow_base_aux_6862 = 0
 R25: - qq7 - qq8 - qq9 + pow_base_aux_6864 = 0
 R26: - qq7 - qq8 - qq9 + pow_base_aux_6866 = 0
 R27: - qq7 - qq8 - qq9 + pow_base_aux_6868 = 0
 R28: - qq7 - qq8 - qq9 + pow_base_aux_6870 = 0
 R29: - qq7 - qq8 - qq9 + pow_base_aux_6872 = 0
 R30: qq1 - ab1 + qq2 - ab2 + qq3 - ab3 + qq4 - ab4 + qq5 - ab5 + qq6 - ab6
   + qq7 - ab7 + qq8 - ab8 + qq9 - ab9 = 117.3289016365619
 R31: - ab9 + pow_base_aux_6874 = 0
 R32: - ab8 + pow_base_aux_6876 = 0
 R33: - ab3 + pow_base_aux_6878 = 0
 R34: - ab7 + pow_base_aux_6880 = 0
 R35: - ab4 + pow_base_aux_6882 = 0
 R36: - ab6 + pow_base_aux_6884 = 0
 R37: - ab5 + pow_base_aux_6886 = 0
 R38: - qq7 - qq8 - qq9 + pow_base_aux_6888 = 0
 R39: - ab1 + pow_base_aux_6890 = 0
 R40: - ab2 + pow_base_aux_6892 = 0
 R41: - qq7 - qq8 - qq9 + pow_base_aux_6894 = 0
 R42: - qq7 - qq8 - qq9 + pow_base_aux_6896 = 0
 R43: - qq7 - qq8 - qq9 + pow_base_aux_6898 = 0
 qc0: - 4 ab1 + ppp + [ - 0.3 ab1 ^2 ] >= 10
 qc1: - 6 ab2 + ppp + [ - 0.6 ab2 ^2 ] >= 11
 qc2: - 8 ab3 + ppp + [ - 15 ab3 ^2 ] >= 5
 qc3: - 10 ab4 + ppp + [ - 9 ab4 ^2 ] >= 7
 qc4: - 12 ab5 + ppp + [ - 6 ab5 ^2 ] >= 1
 qc5: - 14 ab6 + ppp + [ - 9 ab6 ^2 ] >= 2
 qc6: - ppp - 0.02 pow_aux_6863 + [ - 0.03 qq7 * pow_aux_6865 ] >= -200
 qc7: - 16 ab7 + ppp + [ - 12 ab7 ^2 ] >= 3
 qc8: - ppp - 0.02 pow_aux_6867 + [ - 0.03 qq8 * pow_aux_6869 ] >= -200
 qc9: - 18 ab8 + ppp + [ - 30 ab8 ^2 ] >= 4
 qc10: - ppp - 0.02 pow_aux_6871 + [ - 0.03 qq9 * pow_aux_6873 ] >= -200
 qc11: - 20 ab9 + ppp + [ - 33 ab9 ^2 ] >= 5
 qc12: quad_aux_6875 + [ - pow_base_aux_6874 ^2 ] = 0
 qc13: quad_aux_6877 + [ - pow_base_aux_6876 ^2 ] = 0
 qc14: quad_aux_6879 + [ - pow_base_aux_6878 ^2 ] = 0
 qc15: quad_aux_6881 + [ - pow_base_aux_6880 ^2 ] = 0
 qc16: quad_aux_6883 + [ - pow_base_aux_6882 ^2 ] = 0
 qc17: quad_aux_6885 + [ - pow_base_aux_6884 ^2 ] = 0
 qc18: quad_aux_6887 + [ - pow_base_aux_6886 ^2 ] = 0
 qc19: quad_aux_6891 + [ - pow_base_aux_6890 ^2 ] = 0
 qc20: quad_aux_6893 + [ - pow_base_aux_6892 ^2 ] = 0
Bounds
 Constant = 1
General Constraints
 GC0: pow_aux_6863 = POW ( pow_base_aux_6862 ^ 1.5 )
 GC1: pow_aux_6865 = POW ( pow_base_aux_6864 ^ 0.5 )
 GC2: pow_aux_6867 = POW ( pow_base_aux_6866 ^ 1.5 )
 GC3: pow_aux_6869 = POW ( pow_base_aux_6868 ^ 0.5 )
 GC4: pow_aux_6871 = POW ( pow_base_aux_6870 ^ 1.5 )
 GC5: pow_aux_6873 = POW ( pow_base_aux_6872 ^ 0.5 )
 GC6: pow_aux_6889 = POW ( pow_base_aux_6888 ^ 1.5 )
 GC7: pow_aux_6895 = POW ( pow_base_aux_6894 ^ 0.5 )
 GC8: pow_aux_6897 = POW ( pow_base_aux_6896 ^ 0.5 )
 GC9: pow_aux_6899 = POW ( pow_base_aux_6898 ^ 0.5 )
End
