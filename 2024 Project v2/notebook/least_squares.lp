\ Model Least Squares
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  pow_aux_1097 + pow_aux_1101 + pow_aux_1105 + pow_aux_1109 + pow_aux_1113
   + pow_aux_1117 + pow_aux_1121 + 100 pow_aux_1125 + 121 pow_aux_1129
   + 7921 pow_aux_1133 + 8100 pow_aux_1137 + 9025 pow_aux_1141
   + 20449 pow_aux_1145 + 21904 pow_aux_1149 + 22201 pow_aux_1153
   + 38025 pow_aux_1161 + 38416 pow_aux_1169 + 38809 pow_aux_1177
   + 13766.07115924202 pow_aux_1179
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
 R9: - qq5 + xx5 + pow_base_aux_1094 = 0
 R10: - 12 qq5 + 12 xx5 + ppp - 6 pow_aux_1095 + pow_base_aux_1096 = 1
 R11: - qq6 + xx6 + pow_base_aux_1098 = 0
 R12: - 14 qq6 + 14 xx6 + ppp - 9 pow_aux_1099 + pow_base_aux_1100 = 2
 R13: - qq7 + xx7 + pow_base_aux_1102 = 0
 R14: - 16 qq7 + 16 xx7 + ppp - 12 pow_aux_1103 + pow_base_aux_1104 = 3
 R15: - qq8 + xx8 + pow_base_aux_1106 = 0
 R16: - 18 qq8 + 18 xx8 + ppp - 30 pow_aux_1107 + pow_base_aux_1108 = 4
 R17: - qq9 + xx9 + pow_base_aux_1110 = 0
 R18: - 20 qq9 + 20 xx9 + ppp - 33 pow_aux_1111 + pow_base_aux_1112 = 5
 R19: - qq3 + xx3 + pow_base_aux_1114 = 0
 R20: - 8 qq3 + 8 xx3 + ppp - 15 pow_aux_1115 + pow_base_aux_1116 = 5
 R21: - qq4 + xx4 + pow_base_aux_1118 = 0
 R22: - 10 qq4 + 10 xx4 + ppp - 9 pow_aux_1119 + pow_base_aux_1120 = 7
 R23: - qq1 + xx1 + pow_base_aux_1122 = 0
 R24: - 0.4 qq1 + 0.4 xx1 + 0.1 ppp - 0.03 pow_aux_1123 + pow_base_aux_1124
   = 1
 R25: - qq2 + xx2 + pow_base_aux_1126 = 0
 R26: - 0.5454545454545454 qq2 + 0.5454545454545454 xx2
   + 0.0909090909090909 ppp - 0.0545454545454546 pow_aux_1127
   + pow_base_aux_1128 = 1
 R27: - qq2 + xx2 + pow_base_aux_1130 = 0
 R28: 0.00112359550561798 qq1 + 0.0696629213483146 qq2
   - 0.0674157303370786 xx2 + 0.00112359550561798 qq3
   + 0.00674157303370787 pow_aux_1131 + pow_base_aux_1132 = 1
 R29: - qq1 + xx1 + pow_base_aux_1134 = 0
 R30: 0.0466666666666667 qq1 - 0.0444444444444444 xx1
   + 0.00111111111111111 qq2 + 0.00111111111111111 qq3
   + 0.00333333333333333 pow_aux_1135 + pow_base_aux_1136 = 1
 R31: - qq3 + xx3 + pow_base_aux_1138 = 0
 R32: 0.00105263157894737 qq1 + 0.00105263157894737 qq2
   + 0.0863157894736842 qq3 - 0.0842105263157895 xx3
   + 0.1578947368421053 pow_aux_1139 + pow_base_aux_1140 = 1
 R33: - qq4 + xx4 + pow_base_aux_1142 = 0
 R34: 0.0713286713286713 qq4 - 0.0699300699300699 xx4
   + 6.993006993006993e-04 qq5 + 6.993006993006993e-04 qq6
   + 0.0629370629370629 pow_aux_1143 + pow_base_aux_1144 = 1
 R35: - qq6 + xx6 + pow_base_aux_1146 = 0
 R36: 6.7567567567567571e-04 qq4 + 6.7567567567567571e-04 qq5
   + 0.095945945945946 qq6 - 0.0945945945945946 xx6
   + 0.0608108108108108 pow_aux_1147 + pow_base_aux_1148 = 1
 R37: - qq5 + xx5 + pow_base_aux_1150 = 0
 R38: 0.000671141 qq4 + 0.0818791946308725 qq5 - 0.0805369127516778 xx5
   + 0.000671141 qq6 + 0.0402684563758389 pow_aux_1151 + pow_base_aux_1152
   = 1
 R39: - qq9 + xx9 + pow_base_aux_1154 = 0
 R40: - qq7 - qq8 - qq9 + pow_base_aux_1156 = 0
 R41: - qq7 - qq8 - qq9 + pow_base_aux_1158 = 0
 R42: - qq8 + xx8 + pow_base_aux_1162 = 0
 R43: - qq7 - qq8 - qq9 + pow_base_aux_1164 = 0
 R44: - qq7 - qq8 - qq9 + pow_base_aux_1166 = 0
 R45: - qq7 + xx7 + pow_base_aux_1170 = 0
 R46: - qq7 - qq8 - qq9 + pow_base_aux_1172 = 0
 R47: - qq7 - qq8 - qq9 + pow_base_aux_1174 = 0
 R48: 0.00852304918951343 xx1 + 0.00852304918951343 xx2
   + 0.00852304918951343 xx3 + 0.00852304918951343 xx4
   + 0.00852304918951343 xx5 + 0.00852304918951343 xx6
   + 0.00852304918951343 xx7 + 0.00852304918951343 xx8
   + 0.00852304918951343 xx9 + pow_base_aux_1178 = 1
 qc0: 0.1025641025641026 qq9 - 0.1025641025641026 xx9
   + 0.1692307692307692 pow_aux_1155 + 1.0256410256410256e-04 pow_aux_1157
   + pow_base_aux_1160 + [ 1.5384615384615385e-04 qq9 * pow_aux_1159 ] = 1
 qc1: 0.0918367346938776 qq8 - 0.0918367346938776 xx8
   + 0.1530612244897959 pow_aux_1163 + 1.0204081632653062e-04 pow_aux_1165
   + pow_base_aux_1168 + [ 1.5306122448979591e-04 qq8 * pow_aux_1167 ] = 1
 qc2: 0.0812182741116751 qq7 - 0.0812182741116751 xx7
   + 0.0609137055837563 pow_aux_1171 + 1.0152284263959391e-04 pow_aux_1173
   + pow_base_aux_1176 + [ 1.5228426395939087e-04 qq7 * pow_aux_1175 ] = 1
Bounds
General Constraints
 GC0: pow_aux_1095 = POW ( pow_base_aux_1094 ^ 2 )
 GC1: pow_aux_1097 = POW ( pow_base_aux_1096 ^ 2 )
 GC2: pow_aux_1099 = POW ( pow_base_aux_1098 ^ 2 )
 GC3: pow_aux_1101 = POW ( pow_base_aux_1100 ^ 2 )
 GC4: pow_aux_1103 = POW ( pow_base_aux_1102 ^ 2 )
 GC5: pow_aux_1105 = POW ( pow_base_aux_1104 ^ 2 )
 GC6: pow_aux_1107 = POW ( pow_base_aux_1106 ^ 2 )
 GC7: pow_aux_1109 = POW ( pow_base_aux_1108 ^ 2 )
 GC8: pow_aux_1111 = POW ( pow_base_aux_1110 ^ 2 )
 GC9: pow_aux_1113 = POW ( pow_base_aux_1112 ^ 2 )
 GC10: pow_aux_1115 = POW ( pow_base_aux_1114 ^ 2 )
 GC11: pow_aux_1117 = POW ( pow_base_aux_1116 ^ 2 )
 GC12: pow_aux_1119 = POW ( pow_base_aux_1118 ^ 2 )
 GC13: pow_aux_1121 = POW ( pow_base_aux_1120 ^ 2 )
 GC14: pow_aux_1123 = POW ( pow_base_aux_1122 ^ 2 )
 GC15: pow_aux_1125 = POW ( pow_base_aux_1124 ^ 2 )
 GC16: pow_aux_1127 = POW ( pow_base_aux_1126 ^ 2 )
 GC17: pow_aux_1129 = POW ( pow_base_aux_1128 ^ 2 )
 GC18: pow_aux_1131 = POW ( pow_base_aux_1130 ^ 2 )
 GC19: pow_aux_1133 = POW ( pow_base_aux_1132 ^ 2 )
 GC20: pow_aux_1135 = POW ( pow_base_aux_1134 ^ 2 )
 GC21: pow_aux_1137 = POW ( pow_base_aux_1136 ^ 2 )
 GC22: pow_aux_1139 = POW ( pow_base_aux_1138 ^ 2 )
 GC23: pow_aux_1141 = POW ( pow_base_aux_1140 ^ 2 )
 GC24: pow_aux_1143 = POW ( pow_base_aux_1142 ^ 2 )
 GC25: pow_aux_1145 = POW ( pow_base_aux_1144 ^ 2 )
 GC26: pow_aux_1147 = POW ( pow_base_aux_1146 ^ 2 )
 GC27: pow_aux_1149 = POW ( pow_base_aux_1148 ^ 2 )
 GC28: pow_aux_1151 = POW ( pow_base_aux_1150 ^ 2 )
 GC29: pow_aux_1153 = POW ( pow_base_aux_1152 ^ 2 )
 GC30: pow_aux_1155 = POW ( pow_base_aux_1154 ^ 2 )
 GC31: pow_aux_1157 = POW ( pow_base_aux_1156 ^ 1.5 )
 GC32: pow_aux_1159 = POW ( pow_base_aux_1158 ^ 0.5 )
 GC33: pow_aux_1161 = POW ( pow_base_aux_1160 ^ 2 )
 GC34: pow_aux_1163 = POW ( pow_base_aux_1162 ^ 2 )
 GC35: pow_aux_1165 = POW ( pow_base_aux_1164 ^ 1.5 )
 GC36: pow_aux_1167 = POW ( pow_base_aux_1166 ^ 0.5 )
 GC37: pow_aux_1169 = POW ( pow_base_aux_1168 ^ 2 )
 GC38: pow_aux_1171 = POW ( pow_base_aux_1170 ^ 2 )
 GC39: pow_aux_1173 = POW ( pow_base_aux_1172 ^ 1.5 )
 GC40: pow_aux_1175 = POW ( pow_base_aux_1174 ^ 0.5 )
 GC41: pow_aux_1177 = POW ( pow_base_aux_1176 ^ 2 )
 GC42: pow_aux_1179 = POW ( pow_base_aux_1178 ^ 2 )
End
