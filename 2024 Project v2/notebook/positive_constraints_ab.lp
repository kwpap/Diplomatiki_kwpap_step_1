\ Model positive_constraints_ab
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  - 4 ab1 - 6 ab2 - 6 ab3 + 294 Constant + [ - qq1 ^2 - 2 qq1 * qq2
   - 2 qq1 * qq3 - 12 ab1 ^2 - qq2 ^2 - 2 qq2 * qq3 - 12 ab2 ^2 - qq3 ^2
   - 12 ab3 ^2 ] / 2 
Subject To
 R0: qq1 - ab1 >= 0
 R1: ab1 >= 0
 R2: qq2 - ab2 >= 0
 R3: ab2 >= 0
 R4: qq3 - ab3 >= 0
 R5: ab3 >= 0
 R6: qq1 - ab1 + qq2 - ab2 + qq3 - ab3 = 300
 qc0: - ppp + [ - 0.3 qq1 ^2 - 0.4 qq1 * qq2 - 0.4 qq1 * qq3 - 0.1 qq2 ^2
   - 0.2 qq2 * qq3 - 0.1 qq3 ^2 ] >= -100
 qc1: - 4 ab1 + ppp + [ - 6 ab1 ^2 ] >= 2
 qc2: - ppp + [ - 0.1 qq1 ^2 - 0.4 qq1 * qq2 - 0.2 qq1 * qq3 - 0.3 qq2 ^2
   - 0.4 qq2 * qq3 - 0.1 qq3 ^2 ] >= -100
 qc3: - 6 ab2 + ppp + [ - 6 ab2 ^2 ] >= 3
 qc4: - ppp + [ - 0.1 qq1 ^2 - 0.2 qq1 * qq2 - 0.4 qq1 * qq3 - 0.1 qq2 ^2
   - 0.4 qq2 * qq3 - 0.3 qq3 ^2 ] >= -100
 qc5: - 6 ab3 + ppp + [ - 6 ab3 ^2 ] >= 1
Bounds
 Constant = 1
End
