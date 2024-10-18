\ Model positive_constraints_ab
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  - 0.4 qq1 - 40 ab1 - 0.4 qq2 - 60 ab2 - 0.4 qq3 - 80 ab3 - 0.4 qq4
   - 100 ab4 - 0.4 qq5 - 120 ab5 - 0.4 qq6 - 140 ab6 - 0.8 qq7 - 160 ab7
   - 0.8 qq8 - 180 ab8 - 0.8 qq9 - 20 ab9 + 1302 Constant
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
 R18: - 0.2 qq1 - 0.1 qq2 - 0.1 qq3 - ppp >= -100
 R19: - 40 ab1 + ppp >= 10
 R20: - 0.1 qq1 - 0.2 qq2 - 0.1 qq3 - ppp >= -100
 R21: - 60 ab2 + ppp >= 11
 R22: - 0.1 qq1 - 0.1 qq2 - 0.2 qq3 - ppp >= -100
 R23: - 80 ab3 + ppp >= 5
 R24: - 0.2 qq4 - 0.1 qq5 - 0.1 qq6 - ppp >= -150
 R25: - 100 ab4 + ppp >= 7
 R26: - 0.1 qq4 - 0.2 qq5 - 0.1 qq6 - ppp >= -150
 R27: - 120 ab5 + ppp >= 1
 R28: - 0.1 qq4 - 0.1 qq5 - 0.2 qq6 - ppp >= -150
 R29: - 140 ab6 + ppp >= 2
 R30: - 0.4 qq7 - 0.2 qq8 - 0.2 qq9 - ppp >= -200
 R31: - 160 ab7 + ppp >= 3
 R32: - 0.2 qq7 - 0.4 qq8 - 0.2 qq9 - ppp >= -200
 R33: - 180 ab8 + ppp >= 4
 R34: - 0.2 qq7 - 0.2 qq8 - 0.4 qq9 - ppp >= -200
 R35: - 20 ab9 + ppp >= 5
 R36: qq1 - ab1 + qq2 - ab2 + qq3 - ab3 + qq4 - ab4 + qq5 - ab5 + qq6 - ab6
   + qq7 - ab7 + qq8 - ab8 + qq9 - ab9 = 1000
Bounds
 Constant = 1
End
