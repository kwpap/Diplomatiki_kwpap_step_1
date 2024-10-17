\ Model positive_constraints_ab
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  - 0.4 qq1 - 4 ab1 - 0.4 qq2 - 6 ab2 - 0.4 qq3 - 8 ab3 - 0.4 qq4 - 10 ab4
   - 0.4 qq5 - 12 ab5 - 0.4 qq6 - 14 ab6 - 16 ab7 - 18 ab8 - 20 ab9
   - 33 pow_aux_27061 - 30 pow_aux_27063 - 15 pow_aux_27065
   - 12 pow_aux_27067 - 9 pow_aux_27069 - 9 pow_aux_27071 - 6 pow_aux_27073
   - 0.06 pow_aux_27075 - 0.3 pow_aux_27077 - 0.6 pow_aux_27079
   + 1302 Constant + [ - 0.06 qq7 * pow_aux_27081
   - 0.06 qq8 * pow_aux_27083 - 0.06 qq9 * pow_aux_27085 ] / 2 
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
 R19: - ab1 + pow_base_aux_27030 = 0
 R20: - 4 ab1 + ppp - 0.3 pow_aux_27031 >= 10
 R21: - 0.1 qq1 - 0.2 qq2 - 0.1 qq3 - ppp >= -100
 R22: - ab2 + pow_base_aux_27032 = 0
 R23: - 6 ab2 + ppp - 0.6 pow_aux_27033 >= 11
 R24: - 0.1 qq1 - 0.1 qq2 - 0.2 qq3 - ppp >= -100
 R25: - ab3 + pow_base_aux_27034 = 0
 R26: - 8 ab3 + ppp - 15 pow_aux_27035 >= 5
 R27: - 0.2 qq4 - 0.1 qq5 - 0.1 qq6 - ppp >= -150
 R28: - ab4 + pow_base_aux_27036 = 0
 R29: - 10 ab4 + ppp - 9 pow_aux_27037 >= 7
 R30: - 0.1 qq4 - 0.2 qq5 - 0.1 qq6 - ppp >= -150
 R31: - ab5 + pow_base_aux_27038 = 0
 R32: - 12 ab5 + ppp - 6 pow_aux_27039 >= 1
 R33: - 0.1 qq4 - 0.1 qq5 - 0.2 qq6 - ppp >= -150
 R34: - ab6 + pow_base_aux_27040 = 0
 R35: - 14 ab6 + ppp - 9 pow_aux_27041 >= 2
 R36: - qq7 - qq8 - qq9 + pow_base_aux_27042 = 0
 R37: - qq7 - qq8 - qq9 + pow_base_aux_27044 = 0
 R38: - ab7 + pow_base_aux_27046 = 0
 R39: - 16 ab7 + ppp - 12 pow_aux_27047 >= 3
 R40: - qq7 - qq8 - qq9 + pow_base_aux_27048 = 0
 R41: - qq7 - qq8 - qq9 + pow_base_aux_27050 = 0
 R42: - ab8 + pow_base_aux_27052 = 0
 R43: - 18 ab8 + ppp - 30 pow_aux_27053 >= 4
 R44: - qq7 - qq8 - qq9 + pow_base_aux_27054 = 0
 R45: - qq7 - qq8 - qq9 + pow_base_aux_27056 = 0
 R46: - ab9 + pow_base_aux_27058 = 0
 R47: - 20 ab9 + ppp - 33 pow_aux_27059 >= 5
 R48: qq1 - ab1 + qq2 - ab2 + qq3 - ab3 + qq4 - ab4 + qq5 - ab5 + qq6 - ab6
   + qq7 - ab7 + qq8 - ab8 + qq9 - ab9 = 2229.249131094677
 R49: - ab9 + pow_base_aux_27060 = 0
 R50: - ab8 + pow_base_aux_27062 = 0
 R51: - ab3 + pow_base_aux_27064 = 0
 R52: - ab7 + pow_base_aux_27066 = 0
 R53: - ab4 + pow_base_aux_27068 = 0
 R54: - ab6 + pow_base_aux_27070 = 0
 R55: - ab5 + pow_base_aux_27072 = 0
 R56: - qq7 - qq8 - qq9 + pow_base_aux_27074 = 0
 R57: - ab1 + pow_base_aux_27076 = 0
 R58: - ab2 + pow_base_aux_27078 = 0
 R59: - qq7 - qq8 - qq9 + pow_base_aux_27080 = 0
 R60: - qq7 - qq8 - qq9 + pow_base_aux_27082 = 0
 R61: - qq7 - qq8 - qq9 + pow_base_aux_27084 = 0
 qc0: - ppp - 0.02 pow_aux_27043 + [ - 0.03 qq7 * pow_aux_27045 ] >= -200
 qc1: - ppp - 0.02 pow_aux_27049 + [ - 0.03 qq8 * pow_aux_27051 ] >= -200
 qc2: - ppp - 0.02 pow_aux_27055 + [ - 0.03 qq9 * pow_aux_27057 ] >= -200
Bounds
 Constant = 1
General Constraints
 GC0: pow_aux_27031 = POW ( pow_base_aux_27030 ^ 2 )
 GC1: pow_aux_27033 = POW ( pow_base_aux_27032 ^ 2 )
 GC2: pow_aux_27035 = POW ( pow_base_aux_27034 ^ 2 )
 GC3: pow_aux_27037 = POW ( pow_base_aux_27036 ^ 2 )
 GC4: pow_aux_27039 = POW ( pow_base_aux_27038 ^ 2 )
 GC5: pow_aux_27041 = POW ( pow_base_aux_27040 ^ 2 )
 GC6: pow_aux_27043 = POW ( pow_base_aux_27042 ^ 1.5 )
 GC7: pow_aux_27045 = POW ( pow_base_aux_27044 ^ 0.5 )
 GC8: pow_aux_27047 = POW ( pow_base_aux_27046 ^ 2 )
 GC9: pow_aux_27049 = POW ( pow_base_aux_27048 ^ 1.5 )
 GC10: pow_aux_27051 = POW ( pow_base_aux_27050 ^ 0.5 )
 GC11: pow_aux_27053 = POW ( pow_base_aux_27052 ^ 2 )
 GC12: pow_aux_27055 = POW ( pow_base_aux_27054 ^ 1.5 )
 GC13: pow_aux_27057 = POW ( pow_base_aux_27056 ^ 0.5 )
 GC14: pow_aux_27059 = POW ( pow_base_aux_27058 ^ 2 )
 GC15: pow_aux_27061 = POW ( pow_base_aux_27060 ^ 2 )
 GC16: pow_aux_27063 = POW ( pow_base_aux_27062 ^ 2 )
 GC17: pow_aux_27065 = POW ( pow_base_aux_27064 ^ 2 )
 GC18: pow_aux_27067 = POW ( pow_base_aux_27066 ^ 2 )
 GC19: pow_aux_27069 = POW ( pow_base_aux_27068 ^ 2 )
 GC20: pow_aux_27071 = POW ( pow_base_aux_27070 ^ 2 )
 GC21: pow_aux_27073 = POW ( pow_base_aux_27072 ^ 2 )
 GC22: pow_aux_27075 = POW ( pow_base_aux_27074 ^ 1.5 )
 GC23: pow_aux_27077 = POW ( pow_base_aux_27076 ^ 2 )
 GC24: pow_aux_27079 = POW ( pow_base_aux_27078 ^ 2 )
 GC25: pow_aux_27081 = POW ( pow_base_aux_27080 ^ 0.5 )
 GC26: pow_aux_27083 = POW ( pow_base_aux_27082 ^ 0.5 )
 GC27: pow_aux_27085 = POW ( pow_base_aux_27084 ^ 0.5 )
End
