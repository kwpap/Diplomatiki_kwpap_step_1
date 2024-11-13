\ Model positive_constraints_ab
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  - 0.4 qq1 - 6 ab1 - 0.4 qq2 - 4 ab2 - 0.4 qq3 - 2 ab3 - 0.2 qq4 - 4 ab4
   - 0.2 qq5 - 2 ab5 - 0.2 qq6 - 2 ab6 + 0.9 ppp + 1035 Constant + [
   - 6 ab1 ^2 - 12 ab2 ^2 - 18 ab3 ^2 - 18 ab4 ^2 - 12 ab5 ^2 - 6 ab6 ^2
   ] / 2 
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
 R12: - 0.2 qq1 - 0.1 qq2 - 0.1 qq3 - 0.9 ppp >= -200
 R13: - 0.1 qq1 - 0.2 qq2 - 0.1 qq3 - 0.9 ppp >= -200
 R14: - 0.1 qq1 - 0.1 qq2 - 0.2 qq3 - 0.9 ppp >= -200
 R15: - 0.1 qq4 - 0.05 qq5 - 0.05 qq6 - 0.8 ppp >= -150
 R16: - 0.05 qq4 - 0.1 qq5 - 0.05 qq6 - 0.8 ppp >= -150
 R17: - 0.05 qq4 - 0.05 qq5 - 0.1 qq6 - 0.8 ppp >= -150
 R18: qq1 - ab1 + qq2 - ab2 + qq3 - ab3 + qq4 - ab4 + qq5 - ab5 + qq6 - ab6
   = 3527.775546
 qc0: - 6 ab1 + ppp + [ - 3 ab1 ^2 ] >= 2
 qc1: - 4 ab2 + ppp + [ - 6 ab2 ^2 ] >= 3
 qc2: - 2 ab3 + ppp + [ - 9 ab3 ^2 ] >= 4
 qc3: - 4 ab4 + ppp + [ - 9 ab4 ^2 ] >= 1
 qc4: - 2 ab5 + ppp + [ - 6 ab5 ^2 ] >= 2
 qc5: - 2 ab6 + ppp + [ - 3 ab6 ^2 ] >= 3
Bounds
 Constant = 1
End
