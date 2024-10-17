\ Model positive_constraints_ab
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  - 0.4 qq1 - 4 ab1 - 0.4 qq2 - 6 ab2 - 0.4 qq3 - 8 ab3 - 0.4 qq4 - 10 ab4
   - 0.4 qq5 - 12 ab5 - 0.4 qq6 - 14 ab6 - 16 ab7 - 18 ab8 - 20 ab9
   - 33 pow_aux_1011 - 30 pow_aux_1013 - 15 pow_aux_1015 - 12 pow_aux_1017
   - 9 pow_aux_1019 - 9 pow_aux_1021 - 6 pow_aux_1023 - 0.06 pow_aux_1025
   - 0.3 pow_aux_1027 - 0.6 pow_aux_1029 + 1302 Constant + [
   - 0.06 qq7 * pow_aux_1031 - 0.06 qq8 * pow_aux_1033
   - 0.06 qq9 * pow_aux_1035 ] / 2 
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
 R19: - ab1 + pow_base_aux_980 = 0
 R20: - 4 ab1 + ppp - 0.3 pow_aux_981 >= 10
 R21: - 0.1 qq1 - 0.2 qq2 - 0.1 qq3 - ppp >= -100
 R22: - ab2 + pow_base_aux_982 = 0
 R23: - 6 ab2 + ppp - 0.6 pow_aux_983 >= 11
 R24: - 0.1 qq1 - 0.1 qq2 - 0.2 qq3 - ppp >= -100
 R25: - ab3 + pow_base_aux_984 = 0
 R26: - 8 ab3 + ppp - 15 pow_aux_985 >= 5
 R27: - 0.2 qq4 - 0.1 qq5 - 0.1 qq6 - ppp >= -150
 R28: - ab4 + pow_base_aux_986 = 0
 R29: - 10 ab4 + ppp - 9 pow_aux_987 >= 7
 R30: - 0.1 qq4 - 0.2 qq5 - 0.1 qq6 - ppp >= -150
 R31: - ab5 + pow_base_aux_988 = 0
 R32: - 12 ab5 + ppp - 6 pow_aux_989 >= 1
 R33: - 0.1 qq4 - 0.1 qq5 - 0.2 qq6 - ppp >= -150
 R34: - ab6 + pow_base_aux_990 = 0
 R35: - 14 ab6 + ppp - 9 pow_aux_991 >= 2
 R36: - qq7 - qq8 - qq9 + pow_base_aux_992 = 0
 R37: - qq7 - qq8 - qq9 + pow_base_aux_994 = 0
 R38: - ab7 + pow_base_aux_996 = 0
 R39: - 16 ab7 + ppp - 12 pow_aux_997 >= 3
 R40: - qq7 - qq8 - qq9 + pow_base_aux_998 = 0
 R41: - qq7 - qq8 - qq9 + pow_base_aux_1000 = 0
 R42: - ab8 + pow_base_aux_1002 = 0
 R43: - 18 ab8 + ppp - 30 pow_aux_1003 >= 4
 R44: - qq7 - qq8 - qq9 + pow_base_aux_1004 = 0
 R45: - qq7 - qq8 - qq9 + pow_base_aux_1006 = 0
 R46: - ab9 + pow_base_aux_1008 = 0
 R47: - 20 ab9 + ppp - 33 pow_aux_1009 >= 5
 R48: qq1 - ab1 + qq2 - ab2 + qq3 - ab3 + qq4 - ab4 + qq5 - ab5 + qq6 - ab6
   + qq7 - ab7 + qq8 - ab8 + qq9 - ab9 = 117.3289016365619
 R49: - ab9 + pow_base_aux_1010 = 0
 R50: - ab8 + pow_base_aux_1012 = 0
 R51: - ab3 + pow_base_aux_1014 = 0
 R52: - ab7 + pow_base_aux_1016 = 0
 R53: - ab4 + pow_base_aux_1018 = 0
 R54: - ab6 + pow_base_aux_1020 = 0
 R55: - ab5 + pow_base_aux_1022 = 0
 R56: - qq7 - qq8 - qq9 + pow_base_aux_1024 = 0
 R57: - ab1 + pow_base_aux_1026 = 0
 R58: - ab2 + pow_base_aux_1028 = 0
 R59: - qq7 - qq8 - qq9 + pow_base_aux_1030 = 0
 R60: - qq7 - qq8 - qq9 + pow_base_aux_1032 = 0
 R61: - qq7 - qq8 - qq9 + pow_base_aux_1034 = 0
 qc0: - ppp - 0.02 pow_aux_993 + [ - 0.03 qq7 * pow_aux_995 ] >= -200
 qc1: - ppp - 0.02 pow_aux_999 + [ - 0.03 qq8 * pow_aux_1001 ] >= -200
 qc2: - ppp - 0.02 pow_aux_1005 + [ - 0.03 qq9 * pow_aux_1007 ] >= -200
Bounds
 Constant = 1
General Constraints
 GC0: pow_aux_981 = POW ( pow_base_aux_980 ^ 2 )
 GC1: pow_aux_983 = POW ( pow_base_aux_982 ^ 2 )
 GC2: pow_aux_985 = POW ( pow_base_aux_984 ^ 2 )
 GC3: pow_aux_987 = POW ( pow_base_aux_986 ^ 2 )
 GC4: pow_aux_989 = POW ( pow_base_aux_988 ^ 2 )
 GC5: pow_aux_991 = POW ( pow_base_aux_990 ^ 2 )
 GC6: pow_aux_993 = POW ( pow_base_aux_992 ^ 1.5 )
 GC7: pow_aux_995 = POW ( pow_base_aux_994 ^ 0.5 )
 GC8: pow_aux_997 = POW ( pow_base_aux_996 ^ 2 )
 GC9: pow_aux_999 = POW ( pow_base_aux_998 ^ 1.5 )
 GC10: pow_aux_1001 = POW ( pow_base_aux_1000 ^ 0.5 )
 GC11: pow_aux_1003 = POW ( pow_base_aux_1002 ^ 2 )
 GC12: pow_aux_1005 = POW ( pow_base_aux_1004 ^ 1.5 )
 GC13: pow_aux_1007 = POW ( pow_base_aux_1006 ^ 0.5 )
 GC14: pow_aux_1009 = POW ( pow_base_aux_1008 ^ 2 )
 GC15: pow_aux_1011 = POW ( pow_base_aux_1010 ^ 2 )
 GC16: pow_aux_1013 = POW ( pow_base_aux_1012 ^ 2 )
 GC17: pow_aux_1015 = POW ( pow_base_aux_1014 ^ 2 )
 GC18: pow_aux_1017 = POW ( pow_base_aux_1016 ^ 2 )
 GC19: pow_aux_1019 = POW ( pow_base_aux_1018 ^ 2 )
 GC20: pow_aux_1021 = POW ( pow_base_aux_1020 ^ 2 )
 GC21: pow_aux_1023 = POW ( pow_base_aux_1022 ^ 2 )
 GC22: pow_aux_1025 = POW ( pow_base_aux_1024 ^ 1.5 )
 GC23: pow_aux_1027 = POW ( pow_base_aux_1026 ^ 2 )
 GC24: pow_aux_1029 = POW ( pow_base_aux_1028 ^ 2 )
 GC25: pow_aux_1031 = POW ( pow_base_aux_1030 ^ 0.5 )
 GC26: pow_aux_1033 = POW ( pow_base_aux_1032 ^ 0.5 )
 GC27: pow_aux_1035 = POW ( pow_base_aux_1034 ^ 0.5 )
End
