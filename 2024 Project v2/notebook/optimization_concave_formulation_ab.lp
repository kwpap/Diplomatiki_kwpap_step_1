\ Model optimization_concave_formulation_ab
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  0.4 qq1 + 4 ab1 + 0.4 qq2 + 6 ab2 + 0.4 qq3 + 6 ab3 + 0.4 qq4 + 10 ab4
   + 0.4 qq5 + 12 ab5 + 0.4 qq6 + 14 ab6 + 0.08 qq7 + 16 ab7 + 0.08 qq8
   + 18 ab8 + 0.08 qq9 + 20 ab9 - 0.6969696969696971 ppp - 1322 Constant
   + [ 12 ab1 ^2 + 12 ab2 ^2 + 12 ab3 ^2 + 18 ab4 ^2 + 12 ab5 ^2
   + 18 ab6 ^2 + 24 ab7 ^2 + 60 ab8 ^2 + 66 ab9 ^2 ] / 2 
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
 R18: - 0.2 qq1 - 0.1 qq2 - 0.1 qq3 - 0.7676767676767676 ppp <= -100
 R19: - 0.1 qq1 - 0.2 qq2 - 0.1 qq3 - 0.7676767676767676 ppp <= -100
 R20: - 0.1 qq1 - 0.1 qq2 - 0.2 qq3 - 0.7676767676767676 ppp <= -100
 R21: - 0.2 qq4 - 0.1 qq5 - 0.1 qq6 - ppp <= -150
 R22: - 0.1 qq4 - 0.2 qq5 - 0.1 qq6 - ppp <= -150
 R23: - 0.1 qq4 - 0.1 qq5 - 0.2 qq6 - ppp <= -150
 R24: - 0.04 qq7 - 0.02 qq8 - 0.02 qq9 - ppp <= -200
 R25: - 0.02 qq7 - 0.04 qq8 - 0.02 qq9 - ppp <= -200
 R26: - 0.02 qq7 - 0.02 qq8 - 0.04 qq9 - ppp <= -200
 R27: qq1 - ab1 + qq2 - ab2 + qq3 - ab3 + qq4 - ab4 + qq5 - ab5 + qq6 - ab6
   + qq7 - ab7 + qq8 - ab8 + qq9 - ab9 = 7500.001712185168
 qc0: - 4 ab1 + ppp + [ - 6 ab1 ^2 ] <= 2
 qc1: - 6 ab2 + ppp + [ - 6 ab2 ^2 ] <= 3
 qc2: - 6 ab3 + ppp + [ - 6 ab3 ^2 ] <= 1
 qc3: - 10 ab4 + ppp + [ - 9 ab4 ^2 ] <= 7
 qc4: - 12 ab5 + ppp + [ - 6 ab5 ^2 ] <= 1
 qc5: - 14 ab6 + ppp + [ - 9 ab6 ^2 ] <= 2
 qc6: - 16 ab7 + ppp + [ - 12 ab7 ^2 ] <= 3
 qc7: - 18 ab8 + ppp + [ - 30 ab8 ^2 ] <= 4
 qc8: - 20 ab9 + ppp + [ - 33 ab9 ^2 ] <= 5
Bounds
 Constant = 1
End
