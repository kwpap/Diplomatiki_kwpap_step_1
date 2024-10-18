\ Model Positive Constraints
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  - 0.4 qq1 - 2000 xx1 - 0.4 qq2 - 2000 xx2 - 0.4 qq3 - 2000 xx3 - 0.4 qq4
   - 2000 xx4 - 0.4 qq5 - 2000 xx5 - 0.4 qq6 - 2000 xx6 - 0.8 qq7
   - 2000 xx7 - 0.8 qq8 - 2000 xx8 - 0.8 qq9 - 2000 xx9 - 9 ppp
   + 1.00135e+06 Constant + [ 2 xx1 ^2 + 4 xx1 * xx2 + 4 xx1 * xx3
   + 4 xx1 * xx4 + 4 xx1 * xx5 + 4 xx1 * xx6 + 4 xx1 * xx7 + 4 xx1 * xx8
   + 4 xx1 * xx9 + 2 xx2 ^2 + 4 xx2 * xx3 + 4 xx2 * xx4 + 4 xx2 * xx5
   + 4 xx2 * xx6 + 4 xx2 * xx7 + 4 xx2 * xx8 + 4 xx2 * xx9 + 2 xx3 ^2
   + 4 xx3 * xx4 + 4 xx3 * xx5 + 4 xx3 * xx6 + 4 xx3 * xx7 + 4 xx3 * xx8
   + 4 xx3 * xx9 + 2 xx4 ^2 + 4 xx4 * xx5 + 4 xx4 * xx6 + 4 xx4 * xx7
   + 4 xx4 * xx8 + 4 xx4 * xx9 + 2 xx5 ^2 + 4 xx5 * xx6 + 4 xx5 * xx7
   + 4 xx5 * xx8 + 4 xx5 * xx9 + 2 xx6 ^2 + 4 xx6 * xx7 + 4 xx6 * xx8
   + 4 xx6 * xx9 + 2 xx7 ^2 + 4 xx7 * xx8 + 4 xx7 * xx9 + 2 xx8 ^2
   + 4 xx8 * xx9 + 2 xx9 ^2 ] / 2 
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
 R9: - 40.2 qq1 + 40 xx1 - 0.1 qq2 - 0.1 qq3 >= -90
 R10: 40 qq1 - 40 xx1 - ppp >= -10
 R11: - 0.1 qq1 - 60.2 qq2 + 60 xx2 - 0.1 qq3 >= -89
 R12: 60 qq2 - 60 xx2 - ppp >= -11
 R13: - 0.1 qq1 - 0.1 qq2 - 80.2 qq3 + 80 xx3 >= -95
 R14: 80 qq3 - 80 xx3 - ppp >= -5
 R15: - 100.2 qq4 + 100 xx4 - 0.1 qq5 - 0.1 qq6 >= -143
 R16: 100 qq4 - 100 xx4 - ppp >= -7
 R17: - 0.1 qq4 - 120.2 qq5 + 120 xx5 - 0.1 qq6 >= -149
 R18: 120 qq5 - 120 xx5 - ppp >= -1
 R19: - 0.1 qq4 - 0.1 qq5 - 140.2 qq6 + 140 xx6 >= -148
 R20: 140 qq6 - 140 xx6 - ppp >= -2
 R21: - 160.4 qq7 + 160 xx7 - 0.2 qq8 - 0.2 qq9 >= -197
 R22: 160 qq7 - 160 xx7 - ppp >= -3
 R23: - 0.2 qq7 - 180.4 qq8 + 180 xx8 - 0.2 qq9 >= -196
 R24: 180 qq8 - 180 xx8 - ppp >= -4
 R25: - 0.2 qq7 - 0.2 qq8 - 20.4 qq9 + 20 xx9 >= -195
 R26: 20 qq9 - 20 xx9 - ppp >= -5
Bounds
 Constant = 1
End
