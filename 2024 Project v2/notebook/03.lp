\ Model Firm Optimization
\ LP format - for model browsing. Use MPS format to capture full model detail.
Maximize
  0.2 output_1 - 2 emission_1 + 0.2 output_2 - 2 emission_2 + 0.2 output_3
   - 2 emission_3 + 0.04 output_4 - 2 emission_4 + 0.04 output_5
   - 2 emission_5 + 0.04 output_6 - 2 emission_6 + 0.2 output_7
   - 2 emission_7 + 0.2 output_8 - 2 emission_8 - 0.8 output_9 - emission_9
   - 0.1 exp_aux_21 - 5 pow_aux_23 - pow_aux_25 - pow_aux_27 - pow_aux_29
   - pow_aux_31 - pow_aux_33 - pow_aux_35 - pow_aux_37 + [
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
 R10: - output_7 - output_8 - output_9 + pow_base_aux_19 = 0
 R11: price_3 + 0.02 pow_aux_20 = 100
 R12: 0.1 output_1 + 0.1 output_2 + 0.1 output_3 + price_1 = 10
 R13: - 2 output_1 + 2 emission_1 + aux_exp_aux_21_arg = 0
 R14: - output_2 + emission_2 + pow_base_aux_22 = 0
 R15: - output_3 + emission_3 + pow_base_aux_24 = 0
 R16: - output_4 + emission_4 + pow_base_aux_26 = 0
 R17: - output_5 + emission_5 + pow_base_aux_28 = 0
 R18: - output_6 + emission_6 + pow_base_aux_30 = 0
 R19: - output_7 + emission_7 + pow_base_aux_32 = 0
 R20: - output_8 + emission_8 + pow_base_aux_34 = 0
 R21: - output_9 + emission_9 + pow_base_aux_36 = 0
Bounds
General Constraints
 GC0: pow_aux_20 = POW ( pow_base_aux_19 ^ 1.5 )
 GC1: exp_aux_21 = EXP ( aux_exp_aux_21_arg )
 GC2: pow_aux_23 = POW ( pow_base_aux_22 ^ 2 )
 GC3: pow_aux_25 = POW ( pow_base_aux_24 ^ 1.5 )
 GC4: pow_aux_27 = POW ( pow_base_aux_26 ^ 1.1 )
 GC5: pow_aux_29 = POW ( pow_base_aux_28 ^ 1.2 )
 GC6: pow_aux_31 = POW ( pow_base_aux_30 ^ 1.35 )
 GC7: pow_aux_33 = POW ( pow_base_aux_32 ^ 1.4 )
 GC8: pow_aux_35 = POW ( pow_base_aux_34 ^ 1.3 )
 GC9: pow_aux_37 = POW ( pow_base_aux_36 ^ 1.2 )
End
