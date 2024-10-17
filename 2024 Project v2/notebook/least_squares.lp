\ Model Positive Constraints
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  - 0.4 qq1 - 0.4 qq2 - 0.4 qq3 - 3 ppp + pow_aux_39 + 300 Constant
Subject To
 R0: qq1 - xx1 >= 0
 R1: qq2 - xx2 >= 0
 R2: qq3 - xx3 >= 0
 R3: - qq1 + xx1 + pow_base_aux_26 = 0
 R4: - 4.2 qq1 + 4 xx1 - 0.1 qq2 - 0.1 qq3 - 0.3 pow_aux_27 >= -90
 R5: - qq1 + xx1 + pow_base_aux_28 = 0
 R6: 4 qq1 - 4 xx1 - ppp + 0.3 pow_aux_29 >= -10
 R7: - qq2 + xx2 + pow_base_aux_30 = 0
 R8: - 0.1 qq1 - 6.2 qq2 + 6 xx2 - 0.1 qq3 - 0.6 pow_aux_31 >= -89
 R9: - qq2 + xx2 + pow_base_aux_32 = 0
 R10: 6 qq2 - 6 xx2 - ppp + 0.6 pow_aux_33 >= -11
 R11: - qq3 + xx3 + pow_base_aux_34 = 0
 R12: - 0.1 qq1 - 0.1 qq2 - 8.2 qq3 + 8 xx3 - 15 pow_aux_35 >= -95
 R13: - qq3 + xx3 + pow_base_aux_36 = 0
 R14: 8 qq3 - 8 xx3 - ppp + 15 pow_aux_37 >= -5
 R15: xx1 + xx2 + xx3 + pow_base_aux_38 = 650
Bounds
 Constant = 1
General Constraints
 GC0: pow_aux_27 = POW ( pow_base_aux_26 ^ 2 )
 GC1: pow_aux_29 = POW ( pow_base_aux_28 ^ 2 )
 GC2: pow_aux_31 = POW ( pow_base_aux_30 ^ 2 )
 GC3: pow_aux_33 = POW ( pow_base_aux_32 ^ 2 )
 GC4: pow_aux_35 = POW ( pow_base_aux_34 ^ 2 )
 GC5: pow_aux_37 = POW ( pow_base_aux_36 ^ 2 )
 GC6: pow_aux_39 = POW ( pow_base_aux_38 ^ 2 )
End
