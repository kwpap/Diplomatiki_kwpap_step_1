\ Model positive_constraints_ab
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  - 0.4 qq1 - 4 ab1 - 0.4 qq2 - 6 ab2 - 0.4 qq3 - 8 ab3 - 15 pow_aux_65
   - 0.3 pow_aux_67 - 0.6 pow_aux_69 + 274 Constant
Subject To
 R0: qq1 - ab1 >= 0
 R1: ab1 >= 0
 R2: qq2 - ab2 >= 0
 R3: ab2 >= 0
 R4: qq3 - ab3 >= 0
 R5: ab3 >= 0
 R6: - 0.2 qq1 - 0.1 qq2 - 0.1 qq3 - ppp >= -100
 R7: - ab1 + pow_base_aux_58 = 0
 R8: - 4 ab1 + ppp - 0.3 pow_aux_59 >= 10
 R9: - 0.1 qq1 - 0.2 qq2 - 0.1 qq3 - ppp >= -100
 R10: - ab2 + pow_base_aux_60 = 0
 R11: - 6 ab2 + ppp - 0.6 pow_aux_61 >= 11
 R12: - 0.1 qq1 - 0.1 qq2 - 0.2 qq3 - ppp >= -100
 R13: - ab3 + pow_base_aux_62 = 0
 R14: - 8 ab3 + ppp - 15 pow_aux_63 >= 5
 R15: qq1 - ab1 + qq2 - ab2 + qq3 - ab3 = 650
 R16: - ab3 + pow_base_aux_64 = 0
 R17: - ab1 + pow_base_aux_66 = 0
 R18: - ab2 + pow_base_aux_68 = 0
Bounds
 Constant = 1
General Constraints
 GC0: pow_aux_59 = POW ( pow_base_aux_58 ^ 2 )
 GC1: pow_aux_61 = POW ( pow_base_aux_60 ^ 2 )
 GC2: pow_aux_63 = POW ( pow_base_aux_62 ^ 2 )
 GC3: pow_aux_65 = POW ( pow_base_aux_64 ^ 2 )
 GC4: pow_aux_67 = POW ( pow_base_aux_66 ^ 2 )
 GC5: pow_aux_69 = POW ( pow_base_aux_68 ^ 2 )
End
