\ Model optimization_concave_formulation_ab
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  0.4 qq1 + 4 ab1 + 0.4 qq2 + 6 ab2 + 0.4 qq3 + 6 ab3 + 0.4 qq4 + 10 ab4
   + 0.4 qq5 + 12 ab5 + 0.4 qq6 + 14 ab6 + 16 ab7 + 18 ab8 + 20 ab9
   - 90 ppp + 6 quad_aux_4117 + 6 quad_aux_4119 + 6 quad_aux_4121
   + 6 quad_aux_4123 + 9 quad_aux_4125 + 9 quad_aux_4127 + 12 quad_aux_4129
   + 30 quad_aux_4131 + 33 quad_aux_4133 + 0.06 pow_aux_4135
   - 1322 Constant + [ 0.06 qq7 * pow_aux_4137 + 0.06 qq8 * pow_aux_4139
   + 0.06 qq9 * pow_aux_4141 ] / 2 
Subject To
 R0: qq1 - ab1 >= 0
 R1: ab1 >= 0
 R2: qq2 - ab2 >= 0
 R3: ab2 >= 0
 R4: qq3 - ab3 >= 0
 R5: ab3 >= 0
 R6: qq4 - ab4 >= 0
 R7: ab4 >= 0
 R8: qq5 - ab5 >= 0
 R9: ab5 >= 0
 R10: qq6 - ab6 >= 0
 R11: ab6 >= 0
 R12: qq7 - ab7 >= 0
 R13: ab7 >= 0
 R14: qq8 - ab8 >= 0
 R15: ab8 >= 0
 R16: qq9 - ab9 >= 0
 R17: ab9 >= 0
 R18: - 0.2 qq1 - 0.1 qq2 - 0.1 qq3 - ppp <= -100
 R19: - 0.1 qq1 - 0.2 qq2 - 0.1 qq3 - ppp <= -100
 R20: - 0.1 qq1 - 0.1 qq2 - 0.2 qq3 - ppp <= -100
 R21: - 0.2 qq4 - 0.1 qq5 - 0.1 qq6 - ppp <= -150
 R22: - 0.1 qq4 - 0.2 qq5 - 0.1 qq6 - ppp <= -150
 R23: - 0.1 qq4 - 0.1 qq5 - 0.2 qq6 - ppp <= -150
 R24: - qq7 - qq8 - qq9 + pow_base_aux_4104 = 0
 R25: - qq7 - qq8 - qq9 + pow_base_aux_4106 = 0
 R26: - qq7 - qq8 - qq9 + pow_base_aux_4108 = 0
 R27: - qq7 - qq8 - qq9 + pow_base_aux_4110 = 0
 R28: - qq7 - qq8 - qq9 + pow_base_aux_4112 = 0
 R29: - qq7 - qq8 - qq9 + pow_base_aux_4114 = 0
 R30: qq1 - ab1 + qq2 - ab2 + qq3 - ab3 + qq4 - ab4 + qq5 - ab5 + qq6 - ab6
   + qq7 - ab7 + qq8 - ab8 + qq9 - ab9 = 1784.68072606113
 R31: - ab1 + pow_base_aux_4116 = 0
 R32: - ab2 + pow_base_aux_4118 = 0
 R33: - ab3 + pow_base_aux_4120 = 0
 R34: - ab5 + pow_base_aux_4122 = 0
 R35: - ab4 + pow_base_aux_4124 = 0
 R36: - ab6 + pow_base_aux_4126 = 0
 R37: - ab7 + pow_base_aux_4128 = 0
 R38: - ab8 + pow_base_aux_4130 = 0
 R39: - ab9 + pow_base_aux_4132 = 0
 R40: - qq7 - qq8 - qq9 + pow_base_aux_4134 = 0
 R41: - qq7 - qq8 - qq9 + pow_base_aux_4136 = 0
 R42: - qq7 - qq8 - qq9 + pow_base_aux_4138 = 0
 R43: - qq7 - qq8 - qq9 + pow_base_aux_4140 = 0
 qc0: - 4 ab1 + 11 ppp + [ - 6 ab1 ^2 ] <= 2
 qc1: - 6 ab2 + 11 ppp + [ - 6 ab2 ^2 ] <= 3
 qc2: - 6 ab3 + 11 ppp + [ - 6 ab3 ^2 ] <= 1
 qc3: - 10 ab4 + 11 ppp + [ - 9 ab4 ^2 ] <= 7
 qc4: - 12 ab5 + 11 ppp + [ - 6 ab5 ^2 ] <= 1
 qc5: - 14 ab6 + 11 ppp + [ - 9 ab6 ^2 ] <= 2
 qc6: - ppp - 0.02 pow_aux_4105 + [ - 0.03 qq7 * pow_aux_4107 ] <= -200
 qc7: - 16 ab7 + 11 ppp + [ - 12 ab7 ^2 ] <= 3
 qc8: - ppp - 0.02 pow_aux_4109 + [ - 0.03 qq8 * pow_aux_4111 ] <= -200
 qc9: - 18 ab8 + 11 ppp + [ - 30 ab8 ^2 ] <= 4
 qc10: - ppp - 0.02 pow_aux_4113 + [ - 0.03 qq9 * pow_aux_4115 ] <= -200
 qc11: - 20 ab9 + 11 ppp + [ - 33 ab9 ^2 ] <= 5
 qc12: quad_aux_4117 + [ - pow_base_aux_4116 ^2 ] = 0
 qc13: quad_aux_4119 + [ - pow_base_aux_4118 ^2 ] = 0
 qc14: quad_aux_4121 + [ - pow_base_aux_4120 ^2 ] = 0
 qc15: quad_aux_4123 + [ - pow_base_aux_4122 ^2 ] = 0
 qc16: quad_aux_4125 + [ - pow_base_aux_4124 ^2 ] = 0
 qc17: quad_aux_4127 + [ - pow_base_aux_4126 ^2 ] = 0
 qc18: quad_aux_4129 + [ - pow_base_aux_4128 ^2 ] = 0
 qc19: quad_aux_4131 + [ - pow_base_aux_4130 ^2 ] = 0
 qc20: quad_aux_4133 + [ - pow_base_aux_4132 ^2 ] = 0
Bounds
 Constant = 1
General Constraints
 GC0: pow_aux_4105 = POW ( pow_base_aux_4104 ^ 1.5 )
 GC1: pow_aux_4107 = POW ( pow_base_aux_4106 ^ 0.5 )
 GC2: pow_aux_4109 = POW ( pow_base_aux_4108 ^ 1.5 )
 GC3: pow_aux_4111 = POW ( pow_base_aux_4110 ^ 0.5 )
 GC4: pow_aux_4113 = POW ( pow_base_aux_4112 ^ 1.5 )
 GC5: pow_aux_4115 = POW ( pow_base_aux_4114 ^ 0.5 )
 GC6: pow_aux_4135 = POW ( pow_base_aux_4134 ^ 1.5 )
 GC7: pow_aux_4137 = POW ( pow_base_aux_4136 ^ 0.5 )
 GC8: pow_aux_4139 = POW ( pow_base_aux_4138 ^ 0.5 )
 GC9: pow_aux_4141 = POW ( pow_base_aux_4140 ^ 0.5 )
End
