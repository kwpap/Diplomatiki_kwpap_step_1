\ Model Firm Optimization
\ LP format - for model browsing. Use MPS format to capture full model detail.
Maximize
  2 output_1 - 20 emission_1 + 2 output_2 - 20 emission_2 + 2 output_3
   - 20 emission_3 + 20 output_4 - 20 emission_4 + 20 output_5
   - 20 emission_5 + 20 output_6 - 20 emission_6 + 2 output_7
   - 20 emission_7 + 2 output_8 - 20 emission_8 + output_9 - 19 emission_9
   - 0.1 exp_aux_2 - 5 pow_aux_4 - pow_aux_6 - pow_aux_8 - pow_aux_10
   - pow_aux_12 - pow_aux_14 - pow_aux_16 - pow_aux_18 + [
   2 output_1 * price_1 + 2 output_2 * price_1 + 2 output_3 * price_1
   + 2 output_4 * price_2 + 2 output_5 * price_2 + 2 output_6 * price_2
   + 2 output_7 * price_3 + 2 output_8 * price_3 + 2 output_9 * price_3
   ] / 2 
Subject To
 R0: - output_1 + emission_1 <= 0
 R1: - output_2 + emission_2 <= 0
 R2: - output_3 + emission_3 <= 0
 R3: - output_4 + emission_4 <= 0
 R4: - output_5 + emission_5 <= 0
 R5: - output_6 + emission_6 <= 0
 R6: - output_7 + emission_7 <= 0
 R7: - output_8 + emission_8 <= 0
 R8: - output_9 + emission_9 <= 0
 R9: 0.1 output_4 + 0.1 output_5 + 0.1 output_6 + price_2 = 20
 R10: 0.1 output_1 + 0.1 output_2 + 0.1 output_3 + price_1 = 10
 R11: - output_7 - output_8 - output_9 + pow_base_aux_0 = 0
 R12: price_3 + 0.02 pow_aux_1 = 100
 R13: - 2 output_1 + 2 emission_1 + aux_exp_aux_2_arg = 0
 R14: - output_2 + emission_2 + pow_base_aux_3 = 0
 R15: - output_3 + emission_3 + pow_base_aux_5 = 0
 R16: - output_4 + emission_4 + pow_base_aux_7 = 0
 R17: - output_5 + emission_5 + pow_base_aux_9 = 0
 R18: - output_6 + emission_6 + pow_base_aux_11 = 0
 R19: - output_7 + emission_7 + pow_base_aux_13 = 0
 R20: - output_8 + emission_8 + pow_base_aux_15 = 0
 R21: - output_9 + emission_9 + pow_base_aux_17 = 0
Bounds
General Constraints
 GC0: pow_aux_1 = POW ( pow_base_aux_0 ^ 1.5 )
 GC1: exp_aux_2 = EXP ( aux_exp_aux_2_arg )
 GC2: pow_aux_4 = POW ( pow_base_aux_3 ^ 2 )
 GC3: pow_aux_6 = POW ( pow_base_aux_5 ^ 1.5 )
 GC4: pow_aux_8 = POW ( pow_base_aux_7 ^ 1.1 )
 GC5: pow_aux_10 = POW ( pow_base_aux_9 ^ 1.2 )
 GC6: pow_aux_12 = POW ( pow_base_aux_11 ^ 1.35 )
 GC7: pow_aux_14 = POW ( pow_base_aux_13 ^ 1.4 )
 GC8: pow_aux_16 = POW ( pow_base_aux_15 ^ 1.3 )
 GC9: pow_aux_18 = POW ( pow_base_aux_17 ^ 1.2 )
End
