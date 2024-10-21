\ Model Positive Constraints
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  - 0.4 qq1 - 0.4 qq2 - 0.4 qq3 - 0.4 qq4 - 0.4 qq5 - 0.4 qq6 - 9 ppp
   + 13766.07115924202 quad_aux_6919 - 0.06 pow_aux_6921 + 1350 Constant
   + [ - 0.06 qq7 * pow_aux_6923 - 0.06 qq8 * pow_aux_6925
   - 0.06 qq9 * pow_aux_6927 ] / 2 
Subject To
 R0: qq1 - xx1 >= 0
 R1: qq2 - xx2 >= 0
 R2: qq3 - xx3 >= 0
 R3: qq4 - xx4 >= 0
 R4: qq5 - xx5 >= 0
 R5: qq6 - xx6 >= 0
 R6: qq7 - xx7 >= 0
 R7: qq8 - xx8 >= 0
 R8: qq9 - xx9 >= 0
 R9: - qq7 + xx7 + pow_base_aux_6900 = 0
 R10: - qq7 - qq8 - qq9 + pow_base_aux_6902 = 0
 R11: - qq7 - qq8 - qq9 + pow_base_aux_6904 = 0
 R12: - qq8 + xx8 + pow_base_aux_6906 = 0
 R13: - qq7 - qq8 - qq9 + pow_base_aux_6908 = 0
 R14: - qq7 - qq8 - qq9 + pow_base_aux_6910 = 0
 R15: - qq9 + xx9 + pow_base_aux_6912 = 0
 R16: - qq7 - qq8 - qq9 + pow_base_aux_6914 = 0
 R17: - qq7 - qq8 - qq9 + pow_base_aux_6916 = 0
 R18: 0.00852304918951343 xx1 + 0.00852304918951343 xx2
   + 0.00852304918951343 xx3 + 0.00852304918951343 xx4
   + 0.00852304918951343 xx5 + 0.00852304918951343 xx6
   + 0.00852304918951343 xx7 + 0.00852304918951343 xx8
   + 0.00852304918951343 xx9 + pow_base_aux_6918 = 1
 R19: - qq7 - qq8 - qq9 + pow_base_aux_6920 = 0
 R20: - qq7 - qq8 - qq9 + pow_base_aux_6922 = 0
 R21: - qq7 - qq8 - qq9 + pow_base_aux_6924 = 0
 R22: - qq7 - qq8 - qq9 + pow_base_aux_6926 = 0
 qc0: - 4.2 qq1 + 4 xx1 - 0.1 qq2 - 0.1 qq3 + [ - 0.3 qq1 ^2
   + 0.6 qq1 * xx1 - 0.3 xx1 ^2 ] >= -90
 qc1: 4 qq1 - 4 xx1 - ppp + [ 0.3 qq1 ^2 - 0.6 qq1 * xx1 + 0.3 xx1 ^2 ]
   >= -10
 qc2: - 0.1 qq1 - 6.2 qq2 + 6 xx2 - 0.1 qq3 + [ - 0.6 qq2 ^2
   + 1.2 qq2 * xx2 - 0.6 xx2 ^2 ] >= -89
 qc3: 6 qq2 - 6 xx2 - ppp + [ 0.6 qq2 ^2 - 1.2 qq2 * xx2 + 0.6 xx2 ^2 ]
   >= -11
 qc4: - 0.1 qq1 - 0.1 qq2 - 8.2 qq3 + 8 xx3 + [ - 15 qq3 ^2 + 30 qq3 * xx3
   - 15 xx3 ^2 ] >= -95
 qc5: 8 qq3 - 8 xx3 - ppp + [ 15 qq3 ^2 - 30 qq3 * xx3 + 15 xx3 ^2 ] >= -5
 qc6: - 10.2 qq4 + 10 xx4 - 0.1 qq5 - 0.1 qq6 + [ - 9 qq4 ^2 + 18 qq4 * xx4
   - 9 xx4 ^2 ] >= -143
 qc7: 10 qq4 - 10 xx4 - ppp + [ 9 qq4 ^2 - 18 qq4 * xx4 + 9 xx4 ^2 ] >= -7
 qc8: - 0.1 qq4 - 12.2 qq5 + 12 xx5 - 0.1 qq6 + [ - 6 qq5 ^2 + 12 qq5 * xx5
   - 6 xx5 ^2 ] >= -149
 qc9: 12 qq5 - 12 xx5 - ppp + [ 6 qq5 ^2 - 12 qq5 * xx5 + 6 xx5 ^2 ] >= -1
 qc10: - 0.1 qq4 - 0.1 qq5 - 14.2 qq6 + 14 xx6 + [ - 9 qq6 ^2
   + 18 qq6 * xx6 - 9 xx6 ^2 ] >= -148
 qc11: 14 qq6 - 14 xx6 - ppp + [ 9 qq6 ^2 - 18 qq6 * xx6 + 9 xx6 ^2 ]
   >= -2
 qc12: quad_aux_6901 + [ - pow_base_aux_6900 ^2 ] = 0
 qc13: - 16 qq7 + 16 xx7 - 12 quad_aux_6901 - 0.02 pow_aux_6903 + [
   - 0.03 qq7 * pow_aux_6905 ] >= -197
 qc14: 16 qq7 - 16 xx7 - ppp + [ 12 qq7 ^2 - 24 qq7 * xx7 + 12 xx7 ^2 ]
   >= -3
 qc15: quad_aux_6907 + [ - pow_base_aux_6906 ^2 ] = 0
 qc16: - 18 qq8 + 18 xx8 - 30 quad_aux_6907 - 0.02 pow_aux_6909 + [
   - 0.03 qq8 * pow_aux_6911 ] >= -196
 qc17: 18 qq8 - 18 xx8 - ppp + [ 30 qq8 ^2 - 60 qq8 * xx8 + 30 xx8 ^2 ]
   >= -4
 qc18: quad_aux_6913 + [ - pow_base_aux_6912 ^2 ] = 0
 qc19: - 20 qq9 + 20 xx9 - 33 quad_aux_6913 - 0.02 pow_aux_6915 + [
   - 0.03 qq9 * pow_aux_6917 ] >= -195
 qc20: 20 qq9 - 20 xx9 - ppp + [ 33 qq9 ^2 - 66 qq9 * xx9 + 33 xx9 ^2 ]
   >= -5
 qc21: quad_aux_6919 + [ - pow_base_aux_6918 ^2 ] = 0
Bounds
 Constant = 1
General Constraints
 GC0: pow_aux_6903 = POW ( pow_base_aux_6902 ^ 1.5 )
 GC1: pow_aux_6905 = POW ( pow_base_aux_6904 ^ 0.5 )
 GC2: pow_aux_6909 = POW ( pow_base_aux_6908 ^ 1.5 )
 GC3: pow_aux_6911 = POW ( pow_base_aux_6910 ^ 0.5 )
 GC4: pow_aux_6915 = POW ( pow_base_aux_6914 ^ 1.5 )
 GC5: pow_aux_6917 = POW ( pow_base_aux_6916 ^ 0.5 )
 GC6: pow_aux_6921 = POW ( pow_base_aux_6920 ^ 1.5 )
 GC7: pow_aux_6923 = POW ( pow_base_aux_6922 ^ 0.5 )
 GC8: pow_aux_6925 = POW ( pow_base_aux_6924 ^ 0.5 )
 GC9: pow_aux_6927 = POW ( pow_base_aux_6926 ^ 0.5 )
End
