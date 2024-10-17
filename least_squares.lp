\ Model Positive Constraints
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  - 0.4 qq1 - 0.4 qq2 - 0.4 qq3 - 0.4 qq4 - 0.4 qq5 - 0.4 qq6 - 9 ppp
   + 4.9695516884863703e+06 pow_aux_27135 - 0.06 pow_aux_27137
   + 1350 Constant + [ - 0.06 qq7 * pow_aux_27139
   - 0.06 qq8 * pow_aux_27141 - 0.06 qq9 * pow_aux_27143 ] / 2 
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
 R9: - qq1 + xx1 + pow_base_aux_27086 = 0
 R10: - 4.2 qq1 + 4 xx1 - 0.1 qq2 - 0.1 qq3 - 0.3 pow_aux_27087 >= -90
 R11: - qq1 + xx1 + pow_base_aux_27088 = 0
 R12: 4 qq1 - 4 xx1 - ppp + 0.3 pow_aux_27089 >= -10
 R13: - qq2 + xx2 + pow_base_aux_27090 = 0
 R14: - 0.1 qq1 - 6.2 qq2 + 6 xx2 - 0.1 qq3 - 0.6 pow_aux_27091 >= -89
 R15: - qq2 + xx2 + pow_base_aux_27092 = 0
 R16: 6 qq2 - 6 xx2 - ppp + 0.6 pow_aux_27093 >= -11
 R17: - qq3 + xx3 + pow_base_aux_27094 = 0
 R18: - 0.1 qq1 - 0.1 qq2 - 8.2 qq3 + 8 xx3 - 15 pow_aux_27095 >= -95
 R19: - qq3 + xx3 + pow_base_aux_27096 = 0
 R20: 8 qq3 - 8 xx3 - ppp + 15 pow_aux_27097 >= -5
 R21: - qq4 + xx4 + pow_base_aux_27098 = 0
 R22: - 10.2 qq4 + 10 xx4 - 0.1 qq5 - 0.1 qq6 - 9 pow_aux_27099 >= -143
 R23: - qq4 + xx4 + pow_base_aux_27100 = 0
 R24: 10 qq4 - 10 xx4 - ppp + 9 pow_aux_27101 >= -7
 R25: - qq5 + xx5 + pow_base_aux_27102 = 0
 R26: - 0.1 qq4 - 12.2 qq5 + 12 xx5 - 0.1 qq6 - 6 pow_aux_27103 >= -149
 R27: - qq5 + xx5 + pow_base_aux_27104 = 0
 R28: 12 qq5 - 12 xx5 - ppp + 6 pow_aux_27105 >= -1
 R29: - qq6 + xx6 + pow_base_aux_27106 = 0
 R30: - 0.1 qq4 - 0.1 qq5 - 14.2 qq6 + 14 xx6 - 9 pow_aux_27107 >= -148
 R31: - qq6 + xx6 + pow_base_aux_27108 = 0
 R32: 14 qq6 - 14 xx6 - ppp + 9 pow_aux_27109 >= -2
 R33: - qq7 + xx7 + pow_base_aux_27110 = 0
 R34: - qq7 - qq8 - qq9 + pow_base_aux_27112 = 0
 R35: - qq7 - qq8 - qq9 + pow_base_aux_27114 = 0
 R36: - qq7 + xx7 + pow_base_aux_27116 = 0
 R37: 16 qq7 - 16 xx7 - ppp + 12 pow_aux_27117 >= -3
 R38: - qq8 + xx8 + pow_base_aux_27118 = 0
 R39: - qq7 - qq8 - qq9 + pow_base_aux_27120 = 0
 R40: - qq7 - qq8 - qq9 + pow_base_aux_27122 = 0
 R41: - qq8 + xx8 + pow_base_aux_27124 = 0
 R42: 18 qq8 - 18 xx8 - ppp + 30 pow_aux_27125 >= -4
 R43: - qq9 + xx9 + pow_base_aux_27126 = 0
 R44: - qq7 - qq8 - qq9 + pow_base_aux_27128 = 0
 R45: - qq7 - qq8 - qq9 + pow_base_aux_27130 = 0
 R46: - qq9 + xx9 + pow_base_aux_27132 = 0
 R47: 20 qq9 - 20 xx9 - ppp + 33 pow_aux_27133 >= -5
 R48: 4.4858153629018048e-04 xx1 + 4.4858153629018048e-04 xx2
   + 4.4858153629018048e-04 xx3 + 4.4858153629018048e-04 xx4
   + 4.4858153629018048e-04 xx5 + 4.4858153629018048e-04 xx6
   + 4.4858153629018048e-04 xx7 + 4.4858153629018048e-04 xx8
   + 4.4858153629018048e-04 xx9 + pow_base_aux_27134 = 1
 R49: - qq7 - qq8 - qq9 + pow_base_aux_27136 = 0
 R50: - qq7 - qq8 - qq9 + pow_base_aux_27138 = 0
 R51: - qq7 - qq8 - qq9 + pow_base_aux_27140 = 0
 R52: - qq7 - qq8 - qq9 + pow_base_aux_27142 = 0
 qc0: - 16 qq7 + 16 xx7 - 12 pow_aux_27111 - 0.02 pow_aux_27113 + [
   - 0.03 qq7 * pow_aux_27115 ] >= -197
 qc1: - 18 qq8 + 18 xx8 - 30 pow_aux_27119 - 0.02 pow_aux_27121 + [
   - 0.03 qq8 * pow_aux_27123 ] >= -196
 qc2: - 20 qq9 + 20 xx9 - 33 pow_aux_27127 - 0.02 pow_aux_27129 + [
   - 0.03 qq9 * pow_aux_27131 ] >= -195
Bounds
 Constant = 1
General Constraints
 GC0: pow_aux_27087 = POW ( pow_base_aux_27086 ^ 2 )
 GC1: pow_aux_27089 = POW ( pow_base_aux_27088 ^ 2 )
 GC2: pow_aux_27091 = POW ( pow_base_aux_27090 ^ 2 )
 GC3: pow_aux_27093 = POW ( pow_base_aux_27092 ^ 2 )
 GC4: pow_aux_27095 = POW ( pow_base_aux_27094 ^ 2 )
 GC5: pow_aux_27097 = POW ( pow_base_aux_27096 ^ 2 )
 GC6: pow_aux_27099 = POW ( pow_base_aux_27098 ^ 2 )
 GC7: pow_aux_27101 = POW ( pow_base_aux_27100 ^ 2 )
 GC8: pow_aux_27103 = POW ( pow_base_aux_27102 ^ 2 )
 GC9: pow_aux_27105 = POW ( pow_base_aux_27104 ^ 2 )
 GC10: pow_aux_27107 = POW ( pow_base_aux_27106 ^ 2 )
 GC11: pow_aux_27109 = POW ( pow_base_aux_27108 ^ 2 )
 GC12: pow_aux_27111 = POW ( pow_base_aux_27110 ^ 2 )
 GC13: pow_aux_27113 = POW ( pow_base_aux_27112 ^ 1.5 )
 GC14: pow_aux_27115 = POW ( pow_base_aux_27114 ^ 0.5 )
 GC15: pow_aux_27117 = POW ( pow_base_aux_27116 ^ 2 )
 GC16: pow_aux_27119 = POW ( pow_base_aux_27118 ^ 2 )
 GC17: pow_aux_27121 = POW ( pow_base_aux_27120 ^ 1.5 )
 GC18: pow_aux_27123 = POW ( pow_base_aux_27122 ^ 0.5 )
 GC19: pow_aux_27125 = POW ( pow_base_aux_27124 ^ 2 )
 GC20: pow_aux_27127 = POW ( pow_base_aux_27126 ^ 2 )
 GC21: pow_aux_27129 = POW ( pow_base_aux_27128 ^ 1.5 )
 GC22: pow_aux_27131 = POW ( pow_base_aux_27130 ^ 0.5 )
 GC23: pow_aux_27133 = POW ( pow_base_aux_27132 ^ 2 )
 GC24: pow_aux_27135 = POW ( pow_base_aux_27134 ^ 2 )
 GC25: pow_aux_27137 = POW ( pow_base_aux_27136 ^ 1.5 )
 GC26: pow_aux_27139 = POW ( pow_base_aux_27138 ^ 0.5 )
 GC27: pow_aux_27141 = POW ( pow_base_aux_27140 ^ 0.5 )
 GC28: pow_aux_27143 = POW ( pow_base_aux_27142 ^ 0.5 )
End
