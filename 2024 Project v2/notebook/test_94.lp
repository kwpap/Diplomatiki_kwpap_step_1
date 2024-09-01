\ Model example
\ LP format - for model browsing. Use MPS format to capture full model detail.
Maximize
  0 y + [ 2 pow_aux_1 * pow_aux_6 ] / 2 
Subject To
 R0: pow_base_aux_0 = 10
 R1: base_aux_2 = 10
 R2: - x + pow_base_aux_5 = 0
 qc0: prod_aux_4 + [ - x * log_base_aux_3 ] = 0
Bounds
 x >= -1
 y <= 1
General Constraints
 GC0: log_base_aux_3 = LOG ( base_aux_2 )
 GC1: pow_aux_1 = EXP ( prod_aux_4 )
 GC2: pow_aux_6 = POW ( pow_base_aux_5 ^ 1.5 )
End
