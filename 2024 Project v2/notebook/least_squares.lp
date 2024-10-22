\ Model Least Squares
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  quad_aux_1 + quad_aux_5 + quad_aux_9 + quad_aux_13 + 9409 quad_aux_21
   + 9604 quad_aux_29 + 9801 quad_aux_37
Subject To
 R0: qq1 - xx1 >= 0
 R1: qq2 - xx2 >= 0
 R2: qq3 - xx3 >= 0
 R3: xx1 + xx2 + xx3 + pow_base_aux_0 = 300
 R4: - qq3 + xx3 + pow_base_aux_2 = 0
 R5: - 6 qq3 + 6 xx3 + ppp - 6 quad_aux_3 + pow_base_aux_4 = 1
 R6: - qq1 + xx1 + pow_base_aux_6 = 0
 R7: - 4 qq1 + 4 xx1 + ppp - 6 quad_aux_7 + pow_base_aux_8 = 2
 R8: - qq2 + xx2 + pow_base_aux_10 = 0
 R9: - 6 qq2 + 6 xx2 + ppp - 6 quad_aux_11 + pow_base_aux_12 = 3
 R10: - qq2 + xx2 + pow_base_aux_14 = 0
 R11: - qq1 - qq2 - qq3 + pow_base_aux_16 = 0
 R12: - qq1 - qq2 - qq3 + pow_base_aux_18 = 0
 R13: - qq1 + xx1 + pow_base_aux_22 = 0
 R14: - qq1 - qq2 - qq3 + pow_base_aux_24 = 0
 R15: - qq1 - qq2 - qq3 + pow_base_aux_26 = 0
 R16: - qq3 + xx3 + pow_base_aux_30 = 0
 R17: - qq1 - qq2 - qq3 + pow_base_aux_32 = 0
 R18: - qq1 - qq2 - qq3 + pow_base_aux_34 = 0
 qc0: quad_aux_1 + [ - pow_base_aux_0 ^2 ] = 0
 qc1: quad_aux_3 + [ - pow_base_aux_2 ^2 ] = 0
 qc2: quad_aux_5 + [ - pow_base_aux_4 ^2 ] = 0
 qc3: quad_aux_7 + [ - pow_base_aux_6 ^2 ] = 0
 qc4: quad_aux_9 + [ - pow_base_aux_8 ^2 ] = 0
 qc5: quad_aux_11 + [ - pow_base_aux_10 ^2 ] = 0
 qc6: quad_aux_13 + [ - pow_base_aux_12 ^2 ] = 0
 qc7: quad_aux_15 + [ - pow_base_aux_14 ^2 ] = 0
 qc8: 0.0618556701030928 qq2 - 0.0618556701030928 xx2
   + 0.0618556701030928 quad_aux_15 + 0.00103092783505155 pow_aux_17
   + pow_base_aux_20 + [ 0.00154639175257732 qq2 * pow_aux_19 ] = 1
 qc9: quad_aux_21 + [ - pow_base_aux_20 ^2 ] = 0
 qc10: quad_aux_23 + [ - pow_base_aux_22 ^2 ] = 0
 qc11: 0.0408163265306122 qq1 - 0.0408163265306122 xx1
   + 0.0612244897959184 quad_aux_23 + 0.00102040816326531 pow_aux_25
   + pow_base_aux_28 + [ 0.00153061224489796 qq1 * pow_aux_27 ] = 1
 qc12: quad_aux_29 + [ - pow_base_aux_28 ^2 ] = 0
 qc13: quad_aux_31 + [ - pow_base_aux_30 ^2 ] = 0
 qc14: 0.0606060606060606 qq3 - 0.0606060606060606 xx3
   + 0.0606060606060606 quad_aux_31 + 0.00101010101010101 pow_aux_33
   + pow_base_aux_36 + [ 0.00151515151515152 qq3 * pow_aux_35 ] = 1
 qc15: quad_aux_37 + [ - pow_base_aux_36 ^2 ] = 0
Bounds
General Constraints
 GC0: pow_aux_17 = POW ( pow_base_aux_16 ^ 1.5 )
 GC1: pow_aux_19 = POW ( pow_base_aux_18 ^ 0.5 )
 GC2: pow_aux_25 = POW ( pow_base_aux_24 ^ 1.5 )
 GC3: pow_aux_27 = POW ( pow_base_aux_26 ^ 0.5 )
 GC4: pow_aux_33 = POW ( pow_base_aux_32 ^ 1.5 )
 GC5: pow_aux_35 = POW ( pow_base_aux_34 ^ 0.5 )
End
