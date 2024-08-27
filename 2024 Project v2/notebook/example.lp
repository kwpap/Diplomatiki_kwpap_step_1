\ Model example
\ LP format - for model browsing. Use MPS format to capture full model detail.
Maximize
  0 y + pow_aux_1 + [ 2 x * pow_aux_1 ] / 2 
Subject To
 R0: - x + pow_base_aux_0 = 0
Bounds
 x >= 0.1
General Constraints
 GC0: pow_aux_1 = POW ( pow_base_aux_0 ^ -1 )
End
