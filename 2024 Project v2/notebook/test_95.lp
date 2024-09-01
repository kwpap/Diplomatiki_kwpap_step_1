\ Model example
\ LP format - for model browsing. Use MPS format to capture full model detail.
Maximize
  0 y + [ 2 num_aux_1 * inv_denom_aux_3 ] / 2 
Subject To
 R0: - x + pow_base_aux_0 = 0
 R1: - x + denom_aux_2 = 0
 qc0: - x + num_aux_1 + [ - pow_base_aux_0 ^2 ] = 1
 qc1: [ denom_aux_2 * inv_denom_aux_3 ] = 1
Bounds
 x >= 0.1
 y <= 1
End
