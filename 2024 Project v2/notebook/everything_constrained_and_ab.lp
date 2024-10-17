\ Model everything_constrained_and_ab
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  - 0.1 qq1 - 0.1 qq2 - 0.2 qq3 - ppp + 100 Constant
Subject To
 R0: qq1 - ab1 >= 0
 R1: ab1 >= 0
 R2: qq2 - ab2 >= 0
 R3: ab2 >= 0
 R4: qq3 - ab3 >= 0
 R5: ab3 >= 0
 R6: - 0.2 qq1 - 0.1 qq2 - 0.1 qq3 - ppp >= -100
 R7: - ab1 + pow_base_aux_70 = 0
 R8: - 4 ab1 + ppp - 0.3 pow_aux_71 = 10
 R9: - 0.1 qq1 - 0.2 qq2 - 0.1 qq3 - ppp >= -100
 R10: - ab2 + pow_base_aux_72 = 0
 R11: - 6 ab2 + ppp - 0.6 pow_aux_73 = 11
 R12: - 0.1 qq1 - 0.1 qq2 - 0.2 qq3 - ppp >= -100
 R13: - ab3 + pow_base_aux_74 = 0
 R14: - 8 ab3 + ppp - 15 pow_aux_75 = 5
 R15: qq1 - ab1 + qq2 - ab2 + qq3 - ab3 = 650
Bounds
 Constant = 1
General Constraints
 GC0: pow_aux_71 = POW ( pow_base_aux_70 ^ 2 )
 GC1: pow_aux_73 = POW ( pow_base_aux_72 ^ 2 )
 GC2: pow_aux_75 = POW ( pow_base_aux_74 ^ 2 )
End
