\ Model Least Squares e
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  quad_aux_29 + 100 quad_aux_33 + 121 quad_aux_37 + 10000 quad_aux_39
   + 10000 quad_aux_41 + 10000 quad_aux_43
Subject To
 R0: qq1 - ab1 >= 0
 R1: ab1 >= 0
 R2: qq2 - ab2 >= 0
 R3: ab2 >= 0
 R4: qq3 - ab3 >= 0
 R5: ab3 >= 0
 R6: qq1 - ab1 + qq2 - ab2 + qq3 - ab3 = 200
 R7: - ab3 + pow_base_aux_26 = 0
 R8: 8 ab3 - ppp + 15 quad_aux_27 + pow_base_aux_28 = -5
 R9: - ab1 + pow_base_aux_30 = 0
 R10: 0.4 ab1 - 0.1 ppp + 0.03 quad_aux_31 + pow_base_aux_32 = -1
 R11: - ab2 + pow_base_aux_34 = 0
 R12: 0.5454545454545454 ab2 - 0.0909090909090909 ppp
   + 0.0545454545454545 quad_aux_35 + pow_base_aux_36 = -1
 R13: 0.001 qq1 + 0.001 qq2 + 0.002 qq3 + 0.01 ppp + pow_base_aux_38 = 1
 R14: 0.001 qq1 + 0.002 qq2 + 0.001 qq3 + 0.01 ppp + pow_base_aux_40 = 1
 R15: 0.002 qq1 + 0.001 qq2 + 0.001 qq3 + 0.01 ppp + pow_base_aux_42 = 1
 qc0: quad_aux_27 + [ - pow_base_aux_26 ^2 ] = 0
 qc1: quad_aux_29 + [ - pow_base_aux_28 ^2 ] = 0
 qc2: quad_aux_31 + [ - pow_base_aux_30 ^2 ] = 0
 qc3: quad_aux_33 + [ - pow_base_aux_32 ^2 ] = 0
 qc4: quad_aux_35 + [ - pow_base_aux_34 ^2 ] = 0
 qc5: quad_aux_37 + [ - pow_base_aux_36 ^2 ] = 0
 qc6: quad_aux_39 + [ - pow_base_aux_38 ^2 ] = 0
 qc7: quad_aux_41 + [ - pow_base_aux_40 ^2 ] = 0
 qc8: quad_aux_43 + [ - pow_base_aux_42 ^2 ] = 0
Bounds
End
