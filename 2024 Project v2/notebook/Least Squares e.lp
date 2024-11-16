\ Model Least Squares e
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  quad_aux_3 + quad_aux_7 + 22500 quad_aux_9 + 40000 quad_aux_11
Subject To
 R0: qq1 - ab1 >= 0
 R1: ab1 >= 0
 R2: qq2 - ab2 >= 0
 R3: ab2 >= 0
 R4: qq1 - ab1 + qq2 - ab2 = 2000
 R5: - ab2 + pow_base_aux_0 = 0
 R6: 4 ab2 - ppp + 9 quad_aux_1 + pow_base_aux_2 = -1
 R7: - ab1 + pow_base_aux_4 = 0
 R8: 6 ab1 - ppp + 3 quad_aux_5 + pow_base_aux_6 = -2
 R9: 6.6666666666666664e-04 qq2 + 0.0025 ppp + pow_base_aux_8 = 1
 R10: 0.001 qq1 + 0.0009375 ppp + pow_base_aux_10 = 1
 qc0: quad_aux_1 + [ - pow_base_aux_0 ^2 ] = 0
 qc1: quad_aux_3 + [ - pow_base_aux_2 ^2 ] = 0
 qc2: quad_aux_5 + [ - pow_base_aux_4 ^2 ] = 0
 qc3: quad_aux_7 + [ - pow_base_aux_6 ^2 ] = 0
 qc4: quad_aux_9 + [ - pow_base_aux_8 ^2 ] = 0
 qc5: quad_aux_11 + [ - pow_base_aux_10 ^2 ] = 0
Bounds
End
