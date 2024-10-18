\ Model Least Squares e
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  - 80 qq1 + 800 ab1 - 80 qq2 + 1320 ab2 - 80 qq3 + 800 ab3 - 120 qq4
   + 1400 ab4 - 120 qq5 + 240 ab5 - 120 qq6 + 560 ab6 - 320 qq7 + 960 ab7
   - 320 qq8 + 1440 ab8 - 320 qq9 + 200 ab9 - 2796 ppp + 217850 Constant
   + [ 1.6 qq1 * ppp + 0.12 qq1 ^2 + 0.2 qq1 * qq2 + 0.2 qq1 * qq3
   - 160 ab1 * ppp + 3200 ab1 ^2 + 1.6 qq2 * ppp + 0.12 qq2 ^2
   + 0.2 qq2 * qq3 - 240 ab2 * ppp + 7200 ab2 ^2 + 1.6 qq3 * ppp
   + 0.12 qq3 ^2 - 320 ab3 * ppp + 12800 ab3 ^2 + 1.6 qq4 * ppp
   + 0.12 qq4 ^2 + 0.2 qq4 * qq5 + 0.2 qq4 * qq6 - 400 ab4 * ppp
   + 20000 ab4 ^2 + 1.6 qq5 * ppp + 0.12 qq5 ^2 + 0.2 qq5 * qq6
   - 480 ab5 * ppp + 28800 ab5 ^2 + 1.6 qq6 * ppp + 0.12 qq6 ^2
   - 560 ab6 * ppp + 39200 ab6 ^2 + 3.2 qq7 * ppp + 0.48 qq7 ^2
   + 0.8 qq7 * qq8 + 0.8 qq7 * qq9 - 640 ab7 * ppp + 51200 ab7 ^2
   + 3.2 qq8 * ppp + 0.48 qq8 ^2 + 0.8 qq8 * qq9 - 720 ab8 * ppp
   + 64800 ab8 ^2 + 3.2 qq9 * ppp + 0.48 qq9 ^2 - 80 ab9 * ppp + 800 ab9 ^2
   + 36 ppp ^2 ] / 2 
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
   + qq7 - ab7 + qq8 - ab8 + qq9 - ab9 = 1000
Bounds
 Constant = 1
End
