\ Model everything_constrained_and_ab
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  - 0.1 qq2 - 0.375 ppp + 150 Constant
Subject To
 R0: qq1 - ab1 >= 0
 R1: ab1 >= 0
 R2: qq2 - ab2 >= 0
 R3: ab2 >= 0
 R4: - 0.2 qq1 - 0.1875 ppp >= -200
 R5: - 0.1 qq2 - 0.375 ppp >= -150
 R6: qq1 - ab1 + qq2 - ab2 = 2000
 qc0: - 6 ab1 + ppp + [ - 3 ab1 ^2 ] = 2
 qc1: - 4 ab2 + ppp + [ - 9 ab2 ^2 ] = 1
Bounds
 Constant = 1
End
