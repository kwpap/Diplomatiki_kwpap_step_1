\ Model Least Squares e
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  pow_aux_26967 + pow_aux_26971 + pow_aux_26975 + pow_aux_26979
   + pow_aux_26983 + pow_aux_26987 + pow_aux_26991 + 100 pow_aux_26995
   + 121 pow_aux_26999 + 10000 pow_aux_27001 + 10000 pow_aux_27003
   + 10000 pow_aux_27005 + 22500 pow_aux_27007 + 22500 pow_aux_27009
   + 22500 pow_aux_27011 + 40000 pow_aux_27017 + 40000 pow_aux_27023
   + 40000 pow_aux_27029
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
   + qq7 - ab7 + qq8 - ab8 + qq9 - ab9 = 2229.249131094677
 R19: - ab5 + pow_base_aux_26964 = 0
 R20: 12 ab5 - ppp + 6 pow_aux_26965 + pow_base_aux_26966 = -1
 R21: - ab4 + pow_base_aux_26968 = 0
 R22: 10 ab4 - ppp + 9 pow_aux_26969 + pow_base_aux_26970 = -7
 R23: - ab9 + pow_base_aux_26972 = 0
 R24: 20 ab9 - ppp + 33 pow_aux_26973 + pow_base_aux_26974 = -5
 R25: - ab3 + pow_base_aux_26976 = 0
 R26: 8 ab3 - ppp + 15 pow_aux_26977 + pow_base_aux_26978 = -5
 R27: - ab8 + pow_base_aux_26980 = 0
 R28: 18 ab8 - ppp + 30 pow_aux_26981 + pow_base_aux_26982 = -4
 R29: - ab7 + pow_base_aux_26984 = 0
 R30: 16 ab7 - ppp + 12 pow_aux_26985 + pow_base_aux_26986 = -3
 R31: - ab6 + pow_base_aux_26988 = 0
 R32: 14 ab6 - ppp + 9 pow_aux_26989 + pow_base_aux_26990 = -2
 R33: - ab1 + pow_base_aux_26992 = 0
 R34: 0.4 ab1 - 0.1 ppp + 0.03 pow_aux_26993 + pow_base_aux_26994 = -1
 R35: - ab2 + pow_base_aux_26996 = 0
 R36: 0.5454545454545454 ab2 - 0.0909090909090909 ppp
   + 0.0545454545454545 pow_aux_26997 + pow_base_aux_26998 = -1
 R37: 0.001 qq1 + 0.001 qq2 + 0.002 qq3 + 0.01 ppp + pow_base_aux_27000
   = 1
 R38: 0.001 qq1 + 0.002 qq2 + 0.001 qq3 + 0.01 ppp + pow_base_aux_27002
   = 1
 R39: 0.002 qq1 + 0.001 qq2 + 0.001 qq3 + 0.01 ppp + pow_base_aux_27004
   = 1
 R40: 6.6666666666666664e-04 qq4 + 6.6666666666666664e-04 qq5
   + 0.00133333333333333 qq6 + 0.00666666666666667 ppp + pow_base_aux_27006
   = 1
 R41: 6.6666666666666664e-04 qq4 + 0.00133333333333333 qq5
   + 6.6666666666666664e-04 qq6 + 0.00666666666666667 ppp
   + pow_base_aux_27008 = 1
 R42: 0.00133333333333333 qq4 + 6.6666666666666664e-04 qq5
   + 6.6666666666666664e-04 qq6 + 0.00666666666666667 ppp
   + pow_base_aux_27010 = 1
 R43: - qq7 - qq8 - qq9 + pow_base_aux_27012 = 0
 R44: - qq7 - qq8 - qq9 + pow_base_aux_27014 = 0
 R45: - qq7 - qq8 - qq9 + pow_base_aux_27018 = 0
 R46: - qq7 - qq8 - qq9 + pow_base_aux_27020 = 0
 R47: - qq7 - qq8 - qq9 + pow_base_aux_27024 = 0
 R48: - qq7 - qq8 - qq9 + pow_base_aux_27026 = 0
 qc0: 0.005 ppp + 0.0001 pow_aux_27013 + pow_base_aux_27016 + [
   0.00015 qq7 * pow_aux_27015 ] = 1
 qc1: 0.005 ppp + 0.0001 pow_aux_27019 + pow_base_aux_27022 + [
   0.00015 qq8 * pow_aux_27021 ] = 1
 qc2: 0.005 ppp + 0.0001 pow_aux_27025 + pow_base_aux_27028 + [
   0.00015 qq9 * pow_aux_27027 ] = 1
Bounds
General Constraints
 GC0: pow_aux_26965 = POW ( pow_base_aux_26964 ^ 2 )
 GC1: pow_aux_26967 = POW ( pow_base_aux_26966 ^ 2 )
 GC2: pow_aux_26969 = POW ( pow_base_aux_26968 ^ 2 )
 GC3: pow_aux_26971 = POW ( pow_base_aux_26970 ^ 2 )
 GC4: pow_aux_26973 = POW ( pow_base_aux_26972 ^ 2 )
 GC5: pow_aux_26975 = POW ( pow_base_aux_26974 ^ 2 )
 GC6: pow_aux_26977 = POW ( pow_base_aux_26976 ^ 2 )
 GC7: pow_aux_26979 = POW ( pow_base_aux_26978 ^ 2 )
 GC8: pow_aux_26981 = POW ( pow_base_aux_26980 ^ 2 )
 GC9: pow_aux_26983 = POW ( pow_base_aux_26982 ^ 2 )
 GC10: pow_aux_26985 = POW ( pow_base_aux_26984 ^ 2 )
 GC11: pow_aux_26987 = POW ( pow_base_aux_26986 ^ 2 )
 GC12: pow_aux_26989 = POW ( pow_base_aux_26988 ^ 2 )
 GC13: pow_aux_26991 = POW ( pow_base_aux_26990 ^ 2 )
 GC14: pow_aux_26993 = POW ( pow_base_aux_26992 ^ 2 )
 GC15: pow_aux_26995 = POW ( pow_base_aux_26994 ^ 2 )
 GC16: pow_aux_26997 = POW ( pow_base_aux_26996 ^ 2 )
 GC17: pow_aux_26999 = POW ( pow_base_aux_26998 ^ 2 )
 GC18: pow_aux_27001 = POW ( pow_base_aux_27000 ^ 2 )
 GC19: pow_aux_27003 = POW ( pow_base_aux_27002 ^ 2 )
 GC20: pow_aux_27005 = POW ( pow_base_aux_27004 ^ 2 )
 GC21: pow_aux_27007 = POW ( pow_base_aux_27006 ^ 2 )
 GC22: pow_aux_27009 = POW ( pow_base_aux_27008 ^ 2 )
 GC23: pow_aux_27011 = POW ( pow_base_aux_27010 ^ 2 )
 GC24: pow_aux_27013 = POW ( pow_base_aux_27012 ^ 1.5 )
 GC25: pow_aux_27015 = POW ( pow_base_aux_27014 ^ 0.5 )
 GC26: pow_aux_27017 = POW ( pow_base_aux_27016 ^ 2 )
 GC27: pow_aux_27019 = POW ( pow_base_aux_27018 ^ 1.5 )
 GC28: pow_aux_27021 = POW ( pow_base_aux_27020 ^ 0.5 )
 GC29: pow_aux_27023 = POW ( pow_base_aux_27022 ^ 2 )
 GC30: pow_aux_27025 = POW ( pow_base_aux_27024 ^ 1.5 )
 GC31: pow_aux_27027 = POW ( pow_base_aux_27026 ^ 0.5 )
 GC32: pow_aux_27029 = POW ( pow_base_aux_27028 ^ 2 )
End
