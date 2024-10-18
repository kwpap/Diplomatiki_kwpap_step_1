\ Model positive_constraints_ab
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  - 0.4 qq1 - 4 ab1 - 0.4 qq2 - 6 ab2 - 0.4 qq3 - 8 ab3 + 274 Constant + [
   - 0.6 ab1 ^2 - 1.2 ab2 ^2 - 30 ab3 ^2 ] / 2 
Subject To
 R0: qq1 - ab1 >= 0
 R1: ab1 >= 0
 R2: qq2 - ab2 >= 0
 R3: ab2 >= 0
 R4: qq3 - ab3 >= 0
 R5: ab3 >= 0
 R6: - 0.2 qq1 - 0.1 qq2 - 0.1 qq3 - ppp >= -100
 R7: - 0.1 qq1 - 0.2 qq2 - 0.1 qq3 - ppp >= -100
 R8: - 0.1 qq1 - 0.1 qq2 - 0.2 qq3 - ppp >= -100
 R9: qq1 - ab1 + qq2 - ab2 + qq3 - ab3 = 200
 qc0: - 4 ab1 + ppp + [ - 0.3 ab1 ^2 ] >= 10
 qc1: - 6 ab2 + ppp + [ - 0.6 ab2 ^2 ] >= 11
 qc2: - 8 ab3 + ppp + [ - 15 ab3 ^2 ] >= 5
Bounds
 Constant = 1
End
