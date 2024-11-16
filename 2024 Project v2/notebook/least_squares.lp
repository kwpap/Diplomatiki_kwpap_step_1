\ Model Positive Constraints
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  - 0.2 qq1 - 4000 xx1 - 0.1 qq2 - 4000 xx2 - 0.5625 ppp
   + 4.00035e+06 Constant + [ 2 xx1 ^2 + 4 xx1 * xx2 + 2 xx2 ^2 ] / 2 
Subject To
 R0: qq1 - xx1 >= 0
 R1: qq2 - xx2 >= 0
 qc0: - 6.2 qq1 + 6 xx1 + 0.8125 ppp + [ - 3 qq1 ^2 + 6 qq1 * xx1
   - 3 xx1 ^2 ] >= -198
 qc1: 6 qq1 - 6 xx1 - ppp + [ 3 qq1 ^2 - 6 qq1 * xx1 + 3 xx1 ^2 ] >= -2
 qc2: - 4.1 qq2 + 4 xx2 + 0.625 ppp + [ - 9 qq2 ^2 + 18 qq2 * xx2
   - 9 xx2 ^2 ] >= -149
 qc3: 4 qq2 - 4 xx2 - ppp + [ 9 qq2 ^2 - 18 qq2 * xx2 + 9 xx2 ^2 ] >= -1
Bounds
 Constant = 1
End
