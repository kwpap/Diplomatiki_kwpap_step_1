\ Model Least Squares
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  0 ppp + pow_aux_1 + 10000 pow_aux_3 + 10000 pow_aux_5 + 10000 pow_aux_7
   + 22500 pow_aux_9 + 22500 pow_aux_11 + 22500 pow_aux_13
   + 40000 pow_aux_19 + 40000 pow_aux_25 + 40000 pow_aux_31
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
 R9: xx1 + xx2 + xx3 + xx4 + xx5 + xx6 + xx7 + xx8 + xx9 + pow_base_aux_0
   = 2000
 R10: 0.001 qq1 + 0.001 qq2 + 0.002 qq3 + pow_base_aux_2 = 1
 R11: 0.001 qq1 + 0.002 qq2 + 0.001 qq3 + pow_base_aux_4 = 1
 R12: 0.002 qq1 + 0.001 qq2 + 0.001 qq3 + pow_base_aux_6 = 1
 R13: 6.6666666666666664e-04 qq4 + 6.6666666666666664e-04 qq5
   + 0.00133333333333333 qq6 + pow_base_aux_8 = 1
 R14: 6.6666666666666664e-04 qq4 + 0.00133333333333333 qq5
   + 6.6666666666666664e-04 qq6 + pow_base_aux_10 = 1
 R15: 0.00133333333333333 qq4 + 6.6666666666666664e-04 qq5
   + 6.6666666666666664e-04 qq6 + pow_base_aux_12 = 1
 R16: - qq7 - qq8 - qq9 + pow_base_aux_14 = 0
 R17: - qq7 - qq8 - qq9 + pow_base_aux_16 = 0
 R18: - qq7 - qq8 - qq9 + pow_base_aux_20 = 0
 R19: - qq7 - qq8 - qq9 + pow_base_aux_22 = 0
 R20: - qq7 - qq8 - qq9 + pow_base_aux_26 = 0
 R21: - qq7 - qq8 - qq9 + pow_base_aux_28 = 0
 qc0: 0.0001 pow_aux_15 + pow_base_aux_18 + [ 0.00015 qq7 * pow_aux_17 ]
   = 1
 qc1: 0.0001 pow_aux_21 + pow_base_aux_24 + [ 0.00015 qq8 * pow_aux_23 ]
   = 1
 qc2: 0.0001 pow_aux_27 + pow_base_aux_30 + [ 0.00015 qq9 * pow_aux_29 ]
   = 1
Bounds
General Constraints
 GC0: pow_aux_1 = POW ( pow_base_aux_0 ^ 2 )
 GC1: pow_aux_3 = POW ( pow_base_aux_2 ^ 2 )
 GC2: pow_aux_5 = POW ( pow_base_aux_4 ^ 2 )
 GC3: pow_aux_7 = POW ( pow_base_aux_6 ^ 2 )
 GC4: pow_aux_9 = POW ( pow_base_aux_8 ^ 2 )
 GC5: pow_aux_11 = POW ( pow_base_aux_10 ^ 2 )
 GC6: pow_aux_13 = POW ( pow_base_aux_12 ^ 2 )
 GC7: pow_aux_15 = POW ( pow_base_aux_14 ^ 1.5 )
 GC8: pow_aux_17 = POW ( pow_base_aux_16 ^ 0.5 )
 GC9: pow_aux_19 = POW ( pow_base_aux_18 ^ 2 )
 GC10: pow_aux_21 = POW ( pow_base_aux_20 ^ 1.5 )
 GC11: pow_aux_23 = POW ( pow_base_aux_22 ^ 0.5 )
 GC12: pow_aux_25 = POW ( pow_base_aux_24 ^ 2 )
 GC13: pow_aux_27 = POW ( pow_base_aux_26 ^ 1.5 )
 GC14: pow_aux_29 = POW ( pow_base_aux_28 ^ 0.5 )
 GC15: pow_aux_31 = POW ( pow_base_aux_30 ^ 2 )
End
