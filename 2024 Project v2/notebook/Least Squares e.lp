\ Model Least Squares e
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  pow_aux_43 + 100 pow_aux_47 + 121 pow_aux_51 + 10000 pow_aux_53
   + 10000 pow_aux_55 + 10000 pow_aux_57
Subject To
 R0: qq1 - ab1 >= 0
 R1: ab1 >= 0
 R2: qq2 - ab2 >= 0
 R3: ab2 >= 0
 R4: qq3 - ab3 >= 0
 R5: ab3 >= 0
 R6: qq1 - ab1 + qq2 - ab2 + qq3 - ab3 = 650
 R7: - ab3 + pow_base_aux_40 = 0
 R8: 8 ab3 - ppp + 15 pow_aux_41 + pow_base_aux_42 = -5
 R9: - ab1 + pow_base_aux_44 = 0
 R10: 0.4 ab1 - 0.1 ppp + 0.03 pow_aux_45 + pow_base_aux_46 = -1
 R11: - ab2 + pow_base_aux_48 = 0
 R12: 0.5454545454545454 ab2 - 0.0909090909090909 ppp
   + 0.0545454545454545 pow_aux_49 + pow_base_aux_50 = -1
 R13: 0.001 qq1 + 0.001 qq2 + 0.002 qq3 + 0.01 ppp + pow_base_aux_52 = 1
 R14: 0.001 qq1 + 0.002 qq2 + 0.001 qq3 + 0.01 ppp + pow_base_aux_54 = 1
 R15: 0.002 qq1 + 0.001 qq2 + 0.001 qq3 + 0.01 ppp + pow_base_aux_56 = 1
Bounds
General Constraints
 GC0: pow_aux_41 = POW ( pow_base_aux_40 ^ 2 )
 GC1: pow_aux_43 = POW ( pow_base_aux_42 ^ 2 )
 GC2: pow_aux_45 = POW ( pow_base_aux_44 ^ 2 )
 GC3: pow_aux_47 = POW ( pow_base_aux_46 ^ 2 )
 GC4: pow_aux_49 = POW ( pow_base_aux_48 ^ 2 )
 GC5: pow_aux_51 = POW ( pow_base_aux_50 ^ 2 )
 GC6: pow_aux_53 = POW ( pow_base_aux_52 ^ 2 )
 GC7: pow_aux_55 = POW ( pow_base_aux_54 ^ 2 )
 GC8: pow_aux_57 = POW ( pow_base_aux_56 ^ 2 )
End
