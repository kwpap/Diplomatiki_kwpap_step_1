\ Model Least Squares e
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  quad_aux_6087 + quad_aux_6091 + quad_aux_6095 + quad_aux_6099
   + quad_aux_6103 + quad_aux_6107 + quad_aux_6111 + quad_aux_6115
   + quad_aux_6119 + 10000 quad_aux_6121 + 10000 quad_aux_6123
   + 10000 quad_aux_6125 + 22500 quad_aux_6127 + 22500 quad_aux_6129
   + 22500 quad_aux_6131 + 40000 quad_aux_6137 + 40000 quad_aux_6143
   + 40000 quad_aux_6149
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
 R18: qq1 - ab1 + qq2 - ab2 + qq3 - ab3 + qq4 - ab4 + qq5 - ab5 + qq6 - ab6
   + qq7 - ab7 + qq8 - ab8 + qq9 - ab9 = 1784.68072606113
 R19: - ab5 + pow_base_aux_6084 = 0
 R20: 12 ab5 - ppp + 6 quad_aux_6085 + pow_base_aux_6086 = -1
 R21: - ab3 + pow_base_aux_6088 = 0
 R22: 6 ab3 - ppp + 6 quad_aux_6089 + pow_base_aux_6090 = -1
 R23: - ab4 + pow_base_aux_6092 = 0
 R24: 10 ab4 - ppp + 9 quad_aux_6093 + pow_base_aux_6094 = -7
 R25: - ab9 + pow_base_aux_6096 = 0
 R26: 20 ab9 - ppp + 33 quad_aux_6097 + pow_base_aux_6098 = -5
 R27: - ab8 + pow_base_aux_6100 = 0
 R28: 18 ab8 - ppp + 30 quad_aux_6101 + pow_base_aux_6102 = -4
 R29: - ab7 + pow_base_aux_6104 = 0
 R30: 16 ab7 - ppp + 12 quad_aux_6105 + pow_base_aux_6106 = -3
 R31: - ab2 + pow_base_aux_6108 = 0
 R32: 6 ab2 - ppp + 6 quad_aux_6109 + pow_base_aux_6110 = -3
 R33: - ab6 + pow_base_aux_6112 = 0
 R34: 14 ab6 - ppp + 9 quad_aux_6113 + pow_base_aux_6114 = -2
 R35: - ab1 + pow_base_aux_6116 = 0
 R36: 4 ab1 - ppp + 6 quad_aux_6117 + pow_base_aux_6118 = -2
 R37: 0.001 qq1 + 0.001 qq2 + 0.002 qq3 + 0.000909091 ppp
   + pow_base_aux_6120 = 1
 R38: 0.001 qq1 + 0.002 qq2 + 0.001 qq3 + 0.000909091 ppp
   + pow_base_aux_6122 = 1
 R39: 0.002 qq1 + 0.001 qq2 + 0.001 qq3 + 0.000909091 ppp
   + pow_base_aux_6124 = 1
 R40: 6.6666666666666664e-04 qq4 + 6.6666666666666664e-04 qq5
   + 0.00133333333333333 qq6 + 0.00666666666666667 ppp + pow_base_aux_6126
   = 1
 R41: 6.6666666666666664e-04 qq4 + 0.00133333333333333 qq5
   + 6.6666666666666664e-04 qq6 + 0.00666666666666667 ppp
   + pow_base_aux_6128 = 1
 R42: 0.00133333333333333 qq4 + 6.6666666666666664e-04 qq5
   + 6.6666666666666664e-04 qq6 + 0.00666666666666667 ppp
   + pow_base_aux_6130 = 1
 R43: - qq7 - qq8 - qq9 + pow_base_aux_6132 = 0
 R44: - qq7 - qq8 - qq9 + pow_base_aux_6134 = 0
 R45: - qq7 - qq8 - qq9 + pow_base_aux_6138 = 0
 R46: - qq7 - qq8 - qq9 + pow_base_aux_6140 = 0
 R47: - qq7 - qq8 - qq9 + pow_base_aux_6144 = 0
 R48: - qq7 - qq8 - qq9 + pow_base_aux_6146 = 0
 qc0: quad_aux_6085 + [ - pow_base_aux_6084 ^2 ] = 0
 qc1: quad_aux_6087 + [ - pow_base_aux_6086 ^2 ] = 0
 qc2: quad_aux_6089 + [ - pow_base_aux_6088 ^2 ] = 0
 qc3: quad_aux_6091 + [ - pow_base_aux_6090 ^2 ] = 0
 qc4: quad_aux_6093 + [ - pow_base_aux_6092 ^2 ] = 0
 qc5: quad_aux_6095 + [ - pow_base_aux_6094 ^2 ] = 0
 qc6: quad_aux_6097 + [ - pow_base_aux_6096 ^2 ] = 0
 qc7: quad_aux_6099 + [ - pow_base_aux_6098 ^2 ] = 0
 qc8: quad_aux_6101 + [ - pow_base_aux_6100 ^2 ] = 0
 qc9: quad_aux_6103 + [ - pow_base_aux_6102 ^2 ] = 0
 qc10: quad_aux_6105 + [ - pow_base_aux_6104 ^2 ] = 0
 qc11: quad_aux_6107 + [ - pow_base_aux_6106 ^2 ] = 0
 qc12: quad_aux_6109 + [ - pow_base_aux_6108 ^2 ] = 0
 qc13: quad_aux_6111 + [ - pow_base_aux_6110 ^2 ] = 0
 qc14: quad_aux_6113 + [ - pow_base_aux_6112 ^2 ] = 0
 qc15: quad_aux_6115 + [ - pow_base_aux_6114 ^2 ] = 0
 qc16: quad_aux_6117 + [ - pow_base_aux_6116 ^2 ] = 0
 qc17: quad_aux_6119 + [ - pow_base_aux_6118 ^2 ] = 0
 qc18: quad_aux_6121 + [ - pow_base_aux_6120 ^2 ] = 0
 qc19: quad_aux_6123 + [ - pow_base_aux_6122 ^2 ] = 0
 qc20: quad_aux_6125 + [ - pow_base_aux_6124 ^2 ] = 0
 qc21: quad_aux_6127 + [ - pow_base_aux_6126 ^2 ] = 0
 qc22: quad_aux_6129 + [ - pow_base_aux_6128 ^2 ] = 0
 qc23: quad_aux_6131 + [ - pow_base_aux_6130 ^2 ] = 0
 qc24: 0.005 ppp + 0.0001 pow_aux_6133 + pow_base_aux_6136 + [
   0.00015 qq7 * pow_aux_6135 ] = 1
 qc25: quad_aux_6137 + [ - pow_base_aux_6136 ^2 ] = 0
 qc26: 0.005 ppp + 0.0001 pow_aux_6139 + pow_base_aux_6142 + [
   0.00015 qq8 * pow_aux_6141 ] = 1
 qc27: quad_aux_6143 + [ - pow_base_aux_6142 ^2 ] = 0
 qc28: 0.005 ppp + 0.0001 pow_aux_6145 + pow_base_aux_6148 + [
   0.00015 qq9 * pow_aux_6147 ] = 1
 qc29: quad_aux_6149 + [ - pow_base_aux_6148 ^2 ] = 0
Bounds
General Constraints
 GC0: pow_aux_6133 = POW ( pow_base_aux_6132 ^ 1.5 )
 GC1: pow_aux_6135 = POW ( pow_base_aux_6134 ^ 0.5 )
 GC2: pow_aux_6139 = POW ( pow_base_aux_6138 ^ 1.5 )
 GC3: pow_aux_6141 = POW ( pow_base_aux_6140 ^ 0.5 )
 GC4: pow_aux_6145 = POW ( pow_base_aux_6144 ^ 1.5 )
 GC5: pow_aux_6147 = POW ( pow_base_aux_6146 ^ 0.5 )
End
