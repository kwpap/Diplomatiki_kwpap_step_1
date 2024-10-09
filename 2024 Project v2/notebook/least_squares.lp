\ Model Least Squares
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  pow_aux_3 + pow_aux_7 + pow_aux_11 + pow_aux_15 + pow_aux_19 + pow_aux_23
   + pow_aux_27 + pow_aux_29 + 100 pow_aux_33 + 121 pow_aux_37
   + 7921 pow_aux_41 + 8100 pow_aux_45 + 9025 pow_aux_49 + 20449 pow_aux_53
   + 21904 pow_aux_57 + 22201 pow_aux_61 + 38025 pow_aux_69
   + 38416 pow_aux_77 + 38809 pow_aux_85
Subject To
 R0: qq1 - xx1 >= 0
 R1: qq2 - xx2 >= 0
 R2: qq3 - xx3 >= 0
 R3: qq4 - xx4 >= 0
 R4: qq5 - xx5 >= 0
 R5: qq6 - xx6 >= 0
 R6: qq7 - xx7 >= 0
 R7: qq8 - xx8 >= 0
 R8: qq9 - xx9 >= 0
 R9: - qq5 + xx5 + pow_base_aux_0 = 0
 R10: - 12 qq5 + 12 xx5 + ppp - 6 pow_aux_1 + pow_base_aux_2 = 1
 R11: - qq6 + xx6 + pow_base_aux_4 = 0
 R12: - 14 qq6 + 14 xx6 + ppp - 9 pow_aux_5 + pow_base_aux_6 = 2
 R13: - qq7 + xx7 + pow_base_aux_8 = 0
 R14: - 16 qq7 + 16 xx7 + ppp - 12 pow_aux_9 + pow_base_aux_10 = 3
 R15: - qq8 + xx8 + pow_base_aux_12 = 0
 R16: - 18 qq8 + 18 xx8 + ppp - 30 pow_aux_13 + pow_base_aux_14 = 4
 R17: - qq9 + xx9 + pow_base_aux_16 = 0
 R18: - 20 qq9 + 20 xx9 + ppp - 33 pow_aux_17 + pow_base_aux_18 = 5
 R19: - qq3 + xx3 + pow_base_aux_20 = 0
 R20: - 8 qq3 + 8 xx3 + ppp - 15 pow_aux_21 + pow_base_aux_22 = 5
 R21: - qq4 + xx4 + pow_base_aux_24 = 0
 R22: - 10 qq4 + 10 xx4 + ppp - 9 pow_aux_25 + pow_base_aux_26 = 7
 R23: xx1 + xx2 + xx3 + xx4 + xx5 + xx6 + xx7 + xx8 + xx9 + pow_base_aux_28
   = 2000
 R24: - qq1 + xx1 + pow_base_aux_30 = 0
 R25: - 0.4 qq1 + 0.4 xx1 + 0.1 ppp - 0.03 pow_aux_31 + pow_base_aux_32
   = 1
 R26: - qq2 + xx2 + pow_base_aux_34 = 0
 R27: - 0.5454545454545454 qq2 + 0.5454545454545454 xx2
   + 0.0909090909090909 ppp - 0.0545454545454546 pow_aux_35
   + pow_base_aux_36 = 1
 R28: - qq2 + xx2 + pow_base_aux_38 = 0
 R29: 0.00112359550561798 qq1 + 0.0696629213483146 qq2
   - 0.0674157303370786 xx2 + 0.00112359550561798 qq3
   + 0.00674157303370787 pow_aux_39 + pow_base_aux_40 = 1
 R30: - qq1 + xx1 + pow_base_aux_42 = 0
 R31: 0.0466666666666667 qq1 - 0.0444444444444444 xx1
   + 0.00111111111111111 qq2 + 0.00111111111111111 qq3
   + 0.00333333333333333 pow_aux_43 + pow_base_aux_44 = 1
 R32: - qq3 + xx3 + pow_base_aux_46 = 0
 R33: 0.00105263157894737 qq1 + 0.00105263157894737 qq2
   + 0.0863157894736842 qq3 - 0.0842105263157895 xx3
   + 0.1578947368421053 pow_aux_47 + pow_base_aux_48 = 1
 R34: - qq4 + xx4 + pow_base_aux_50 = 0
 R35: 0.0713286713286713 qq4 - 0.0699300699300699 xx4
   + 6.993006993006993e-04 qq5 + 6.993006993006993e-04 qq6
   + 0.0629370629370629 pow_aux_51 + pow_base_aux_52 = 1
 R36: - qq6 + xx6 + pow_base_aux_54 = 0
 R37: 6.7567567567567571e-04 qq4 + 6.7567567567567571e-04 qq5
   + 0.095945945945946 qq6 - 0.0945945945945946 xx6
   + 0.0608108108108108 pow_aux_55 + pow_base_aux_56 = 1
 R38: - qq5 + xx5 + pow_base_aux_58 = 0
 R39: 0.000671141 qq4 + 0.0818791946308725 qq5 - 0.0805369127516778 xx5
   + 0.000671141 qq6 + 0.0402684563758389 pow_aux_59 + pow_base_aux_60 = 1
 R40: - qq9 + xx9 + pow_base_aux_62 = 0
 R41: - qq7 - qq8 - qq9 + pow_base_aux_64 = 0
 R42: - qq7 - qq8 - qq9 + pow_base_aux_66 = 0
 R43: - qq8 + xx8 + pow_base_aux_70 = 0
 R44: - qq7 - qq8 - qq9 + pow_base_aux_72 = 0
 R45: - qq7 - qq8 - qq9 + pow_base_aux_74 = 0
 R46: - qq7 + xx7 + pow_base_aux_78 = 0
 R47: - qq7 - qq8 - qq9 + pow_base_aux_80 = 0
 R48: - qq7 - qq8 - qq9 + pow_base_aux_82 = 0
 qc0: 0.1025641025641026 qq9 - 0.1025641025641026 xx9
   + 0.1692307692307692 pow_aux_63 + 1.0256410256410256e-04 pow_aux_65
   + pow_base_aux_68 + [ 1.5384615384615385e-04 qq9 * pow_aux_67 ] = 1
 qc1: 0.0918367346938776 qq8 - 0.0918367346938776 xx8
   + 0.1530612244897959 pow_aux_71 + 1.0204081632653062e-04 pow_aux_73
   + pow_base_aux_76 + [ 1.5306122448979591e-04 qq8 * pow_aux_75 ] = 1
 qc2: 0.0812182741116751 qq7 - 0.0812182741116751 xx7
   + 0.0609137055837563 pow_aux_79 + 1.0152284263959391e-04 pow_aux_81
   + pow_base_aux_84 + [ 1.5228426395939087e-04 qq7 * pow_aux_83 ] = 1
Bounds
General Constraints
 GC0: pow_aux_1 = POW ( pow_base_aux_0 ^ 2 )
 GC1: pow_aux_3 = POW ( pow_base_aux_2 ^ 2 )
 GC2: pow_aux_5 = POW ( pow_base_aux_4 ^ 2 )
 GC3: pow_aux_7 = POW ( pow_base_aux_6 ^ 2 )
 GC4: pow_aux_9 = POW ( pow_base_aux_8 ^ 2 )
 GC5: pow_aux_11 = POW ( pow_base_aux_10 ^ 2 )
 GC6: pow_aux_13 = POW ( pow_base_aux_12 ^ 2 )
 GC7: pow_aux_15 = POW ( pow_base_aux_14 ^ 2 )
 GC8: pow_aux_17 = POW ( pow_base_aux_16 ^ 2 )
 GC9: pow_aux_19 = POW ( pow_base_aux_18 ^ 2 )
 GC10: pow_aux_21 = POW ( pow_base_aux_20 ^ 2 )
 GC11: pow_aux_23 = POW ( pow_base_aux_22 ^ 2 )
 GC12: pow_aux_25 = POW ( pow_base_aux_24 ^ 2 )
 GC13: pow_aux_27 = POW ( pow_base_aux_26 ^ 2 )
 GC14: pow_aux_29 = POW ( pow_base_aux_28 ^ 2 )
 GC15: pow_aux_31 = POW ( pow_base_aux_30 ^ 2 )
 GC16: pow_aux_33 = POW ( pow_base_aux_32 ^ 2 )
 GC17: pow_aux_35 = POW ( pow_base_aux_34 ^ 2 )
 GC18: pow_aux_37 = POW ( pow_base_aux_36 ^ 2 )
 GC19: pow_aux_39 = POW ( pow_base_aux_38 ^ 2 )
 GC20: pow_aux_41 = POW ( pow_base_aux_40 ^ 2 )
 GC21: pow_aux_43 = POW ( pow_base_aux_42 ^ 2 )
 GC22: pow_aux_45 = POW ( pow_base_aux_44 ^ 2 )
 GC23: pow_aux_47 = POW ( pow_base_aux_46 ^ 2 )
 GC24: pow_aux_49 = POW ( pow_base_aux_48 ^ 2 )
 GC25: pow_aux_51 = POW ( pow_base_aux_50 ^ 2 )
 GC26: pow_aux_53 = POW ( pow_base_aux_52 ^ 2 )
 GC27: pow_aux_55 = POW ( pow_base_aux_54 ^ 2 )
 GC28: pow_aux_57 = POW ( pow_base_aux_56 ^ 2 )
 GC29: pow_aux_59 = POW ( pow_base_aux_58 ^ 2 )
 GC30: pow_aux_61 = POW ( pow_base_aux_60 ^ 2 )
 GC31: pow_aux_63 = POW ( pow_base_aux_62 ^ 2 )
 GC32: pow_aux_65 = POW ( pow_base_aux_64 ^ 1.5 )
 GC33: pow_aux_67 = POW ( pow_base_aux_66 ^ 0.5 )
 GC34: pow_aux_69 = POW ( pow_base_aux_68 ^ 2 )
 GC35: pow_aux_71 = POW ( pow_base_aux_70 ^ 2 )
 GC36: pow_aux_73 = POW ( pow_base_aux_72 ^ 1.5 )
 GC37: pow_aux_75 = POW ( pow_base_aux_74 ^ 0.5 )
 GC38: pow_aux_77 = POW ( pow_base_aux_76 ^ 2 )
 GC39: pow_aux_79 = POW ( pow_base_aux_78 ^ 2 )
 GC40: pow_aux_81 = POW ( pow_base_aux_80 ^ 1.5 )
 GC41: pow_aux_83 = POW ( pow_base_aux_82 ^ 0.5 )
 GC42: pow_aux_85 = POW ( pow_base_aux_84 ^ 2 )
End
