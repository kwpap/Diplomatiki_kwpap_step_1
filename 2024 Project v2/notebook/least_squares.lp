\ Model Least Squares
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  quad_aux_3 + quad_aux_7 + quad_aux_11 + quad_aux_15 + quad_aux_19
   + quad_aux_23 + quad_aux_27 + quad_aux_29 + 100 quad_aux_33
   + 121 quad_aux_37 + 7921 quad_aux_41 + 8100 quad_aux_45
   + 9025 quad_aux_49 + 20449 quad_aux_53 + 21904 quad_aux_57
   + 22201 quad_aux_61 + 38025 quad_aux_65 + 38416 quad_aux_69
   + 38809 quad_aux_73
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
 R10: - 12 qq5 + 12 xx5 + ppp - 6 quad_aux_1 + pow_base_aux_2 = 1
 R11: - qq6 + xx6 + pow_base_aux_4 = 0
 R12: - 14 qq6 + 14 xx6 + ppp - 9 quad_aux_5 + pow_base_aux_6 = 2
 R13: - qq7 + xx7 + pow_base_aux_8 = 0
 R14: - 16 qq7 + 16 xx7 + ppp - 12 quad_aux_9 + pow_base_aux_10 = 3
 R15: - qq8 + xx8 + pow_base_aux_12 = 0
 R16: - 18 qq8 + 18 xx8 + ppp - 30 quad_aux_13 + pow_base_aux_14 = 4
 R17: - qq9 + xx9 + pow_base_aux_16 = 0
 R18: - 20 qq9 + 20 xx9 + ppp - 33 quad_aux_17 + pow_base_aux_18 = 5
 R19: - qq3 + xx3 + pow_base_aux_20 = 0
 R20: - 8 qq3 + 8 xx3 + ppp - 15 quad_aux_21 + pow_base_aux_22 = 5
 R21: - qq4 + xx4 + pow_base_aux_24 = 0
 R22: - 10 qq4 + 10 xx4 + ppp - 9 quad_aux_25 + pow_base_aux_26 = 7
 R23: xx1 + xx2 + xx3 + xx4 + xx5 + xx6 + xx7 + xx8 + xx9 + pow_base_aux_28
   = 200
 R24: - qq1 + xx1 + pow_base_aux_30 = 0
 R25: - 0.4 qq1 + 0.4 xx1 + 0.1 ppp - 0.03 quad_aux_31 + pow_base_aux_32
   = 1
 R26: - qq2 + xx2 + pow_base_aux_34 = 0
 R27: - 0.5454545454545454 qq2 + 0.5454545454545454 xx2
   + 0.0909090909090909 ppp - 0.0545454545454546 quad_aux_35
   + pow_base_aux_36 = 1
 R28: - qq2 + xx2 + pow_base_aux_38 = 0
 R29: 0.00112359550561798 qq1 + 0.0696629213483146 qq2
   - 0.0674157303370786 xx2 + 0.00112359550561798 qq3
   + 0.00674157303370787 quad_aux_39 + pow_base_aux_40 = 1
 R30: - qq1 + xx1 + pow_base_aux_42 = 0
 R31: 0.0466666666666667 qq1 - 0.0444444444444444 xx1
   + 0.00111111111111111 qq2 + 0.00111111111111111 qq3
   + 0.00333333333333333 quad_aux_43 + pow_base_aux_44 = 1
 R32: - qq3 + xx3 + pow_base_aux_46 = 0
 R33: 0.00105263157894737 qq1 + 0.00105263157894737 qq2
   + 0.0863157894736842 qq3 - 0.0842105263157895 xx3
   + 0.1578947368421053 quad_aux_47 + pow_base_aux_48 = 1
 R34: - qq4 + xx4 + pow_base_aux_50 = 0
 R35: 0.0713286713286713 qq4 - 0.0699300699300699 xx4
   + 6.993006993006993e-04 qq5 + 6.993006993006993e-04 qq6
   + 0.0629370629370629 quad_aux_51 + pow_base_aux_52 = 1
 R36: - qq6 + xx6 + pow_base_aux_54 = 0
 R37: 6.7567567567567571e-04 qq4 + 6.7567567567567571e-04 qq5
   + 0.095945945945946 qq6 - 0.0945945945945946 xx6
   + 0.0608108108108108 quad_aux_55 + pow_base_aux_56 = 1
 R38: - qq5 + xx5 + pow_base_aux_58 = 0
 R39: 0.000671141 qq4 + 0.0818791946308725 qq5 - 0.0805369127516778 xx5
   + 0.000671141 qq6 + 0.0402684563758389 quad_aux_59 + pow_base_aux_60
   = 1
 R40: - qq9 + xx9 + pow_base_aux_62 = 0
 R41: 0.00102564102564103 qq7 + 0.00102564102564103 qq8
   + 0.1046153846153846 qq9 - 0.1025641025641026 xx9
   + 0.1692307692307692 quad_aux_63 + pow_base_aux_64 = 1
 R42: - qq8 + xx8 + pow_base_aux_66 = 0
 R43: 0.00102040816326531 qq7 + 0.0938775510204082 qq8
   - 0.0918367346938776 xx8 + 0.00102040816326531 qq9
   + 0.1530612244897959 quad_aux_67 + pow_base_aux_68 = 1
 R44: - qq7 + xx7 + pow_base_aux_70 = 0
 R45: 0.083248730964467 qq7 - 0.0812182741116751 xx7
   + 0.00101522842639594 qq8 + 0.00101522842639594 qq9
   + 0.0609137055837563 quad_aux_71 + pow_base_aux_72 = 1
 qc0: quad_aux_1 + [ - pow_base_aux_0 ^2 ] = 0
 qc1: quad_aux_3 + [ - pow_base_aux_2 ^2 ] = 0
 qc2: quad_aux_5 + [ - pow_base_aux_4 ^2 ] = 0
 qc3: quad_aux_7 + [ - pow_base_aux_6 ^2 ] = 0
 qc4: quad_aux_9 + [ - pow_base_aux_8 ^2 ] = 0
 qc5: quad_aux_11 + [ - pow_base_aux_10 ^2 ] = 0
 qc6: quad_aux_13 + [ - pow_base_aux_12 ^2 ] = 0
 qc7: quad_aux_15 + [ - pow_base_aux_14 ^2 ] = 0
 qc8: quad_aux_17 + [ - pow_base_aux_16 ^2 ] = 0
 qc9: quad_aux_19 + [ - pow_base_aux_18 ^2 ] = 0
 qc10: quad_aux_21 + [ - pow_base_aux_20 ^2 ] = 0
 qc11: quad_aux_23 + [ - pow_base_aux_22 ^2 ] = 0
 qc12: quad_aux_25 + [ - pow_base_aux_24 ^2 ] = 0
 qc13: quad_aux_27 + [ - pow_base_aux_26 ^2 ] = 0
 qc14: quad_aux_29 + [ - pow_base_aux_28 ^2 ] = 0
 qc15: quad_aux_31 + [ - pow_base_aux_30 ^2 ] = 0
 qc16: quad_aux_33 + [ - pow_base_aux_32 ^2 ] = 0
 qc17: quad_aux_35 + [ - pow_base_aux_34 ^2 ] = 0
 qc18: quad_aux_37 + [ - pow_base_aux_36 ^2 ] = 0
 qc19: quad_aux_39 + [ - pow_base_aux_38 ^2 ] = 0
 qc20: quad_aux_41 + [ - pow_base_aux_40 ^2 ] = 0
 qc21: quad_aux_43 + [ - pow_base_aux_42 ^2 ] = 0
 qc22: quad_aux_45 + [ - pow_base_aux_44 ^2 ] = 0
 qc23: quad_aux_47 + [ - pow_base_aux_46 ^2 ] = 0
 qc24: quad_aux_49 + [ - pow_base_aux_48 ^2 ] = 0
 qc25: quad_aux_51 + [ - pow_base_aux_50 ^2 ] = 0
 qc26: quad_aux_53 + [ - pow_base_aux_52 ^2 ] = 0
 qc27: quad_aux_55 + [ - pow_base_aux_54 ^2 ] = 0
 qc28: quad_aux_57 + [ - pow_base_aux_56 ^2 ] = 0
 qc29: quad_aux_59 + [ - pow_base_aux_58 ^2 ] = 0
 qc30: quad_aux_61 + [ - pow_base_aux_60 ^2 ] = 0
 qc31: quad_aux_63 + [ - pow_base_aux_62 ^2 ] = 0
 qc32: quad_aux_65 + [ - pow_base_aux_64 ^2 ] = 0
 qc33: quad_aux_67 + [ - pow_base_aux_66 ^2 ] = 0
 qc34: quad_aux_69 + [ - pow_base_aux_68 ^2 ] = 0
 qc35: quad_aux_71 + [ - pow_base_aux_70 ^2 ] = 0
 qc36: quad_aux_73 + [ - pow_base_aux_72 ^2 ] = 0
Bounds
End
