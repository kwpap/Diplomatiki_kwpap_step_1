\ Model firm
\ LP format - for model browsing. Use MPS format to capture full model detail.
Maximize
  9.4 output + 10 emission + [ - 0.2 output ^2 - 2 pow_base_aux_64 ^2 ] / 2 
Subject To
 R0: - output + emission + pow_base_aux_64 = 0
 R1: - output + emission <= 0
Bounds
End
