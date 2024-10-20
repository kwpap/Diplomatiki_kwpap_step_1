\ Model optimization_concave_formulation_ab
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  0.4 qq1 + 4 ab1 + 0.4 qq2 + 6 ab2 + 0.4 qq3 + 8 ab3 + 0.4 qq4 + 10 ab4
   + 0.4 qq5 + 12 ab5 + 0.4 qq6 + 14 ab6 + 16 ab7 + 18 ab8 + 20 ab9 - 3 ppp
   + 6 quad_aux_11519 + 6 quad_aux_11521 + 6 quad_aux_11523
   + 9 quad_aux_11525 + 9 quad_aux_11527 + 12 quad_aux_11529
   + 15 quad_aux_11531 + 30 quad_aux_11533 + 33 quad_aux_11535
   + 0.06 pow_aux_11537 - 1302 Constant + [ 0.06 qq7 * pow_aux_11539
   + 0.06 qq8 * pow_aux_11541 + 0.06 qq9 * pow_aux_11543 ] / 2 
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
 R18: - 0.2 qq1 - 0.1 qq2 - 0.1 qq3 <= -100
 R19: - 0.1 qq1 - 0.2 qq2 - 0.1 qq3 <= -100
 R20: - 0.1 qq1 - 0.1 qq2 - 0.2 qq3 <= -100
 R21: - 0.2 qq4 - 0.1 qq5 - 0.1 qq6 - ppp <= -150
 R22: - 0.1 qq4 - 0.2 qq5 - 0.1 qq6 - ppp <= -150
 R23: - 0.1 qq4 - 0.1 qq5 - 0.2 qq6 - ppp <= -150
 R24: - qq7 - qq8 - qq9 + pow_base_aux_11506 = 0
 R25: - qq7 - qq8 - qq9 + pow_base_aux_11508 = 0
 R26: - qq7 - qq8 - qq9 + pow_base_aux_11510 = 0
 R27: - qq7 - qq8 - qq9 + pow_base_aux_11512 = 0
 R28: - qq7 - qq8 - qq9 + pow_base_aux_11514 = 0
 R29: - qq7 - qq8 - qq9 + pow_base_aux_11516 = 0
 R30: qq1 - ab1 + qq2 - ab2 + qq3 - ab3 + qq4 - ab4 + qq5 - ab5 + qq6 - ab6
   + qq7 - ab7 + qq8 - ab8 + qq9 - ab9 = 2007.765816818772
 R31: - ab1 + pow_base_aux_11518 = 0
 R32: - ab2 + pow_base_aux_11520 = 0
 R33: - ab5 + pow_base_aux_11522 = 0
 R34: - ab4 + pow_base_aux_11524 = 0
 R35: - ab6 + pow_base_aux_11526 = 0
 R36: - ab7 + pow_base_aux_11528 = 0
 R37: - ab3 + pow_base_aux_11530 = 0
 R38: - ab8 + pow_base_aux_11532 = 0
 R39: - ab9 + pow_base_aux_11534 = 0
 R40: - qq7 - qq8 - qq9 + pow_base_aux_11536 = 0
 R41: - qq7 - qq8 - qq9 + pow_base_aux_11538 = 0
 R42: - qq7 - qq8 - qq9 + pow_base_aux_11540 = 0
 R43: - qq7 - qq8 - qq9 + pow_base_aux_11542 = 0
 qc0: - 4 ab1 + ppp + [ - 6 ab1 ^2 ] <= 10
 qc1: - 6 ab2 + ppp + [ - 6 ab2 ^2 ] <= 11
 qc2: - 8 ab3 + ppp + [ - 15 ab3 ^2 ] <= 5
 qc3: - 10 ab4 + ppp + [ - 9 ab4 ^2 ] <= 7
 qc4: - 12 ab5 + ppp + [ - 6 ab5 ^2 ] <= 1
 qc5: - 14 ab6 + ppp + [ - 9 ab6 ^2 ] <= 2
 qc6: - ppp - 0.02 pow_aux_11507 + [ - 0.03 qq7 * pow_aux_11509 ] <= -200
 qc7: - 16 ab7 + ppp + [ - 12 ab7 ^2 ] <= 3
 qc8: - ppp - 0.02 pow_aux_11511 + [ - 0.03 qq8 * pow_aux_11513 ] <= -200
 qc9: - 18 ab8 + ppp + [ - 30 ab8 ^2 ] <= 4
 qc10: - ppp - 0.02 pow_aux_11515 + [ - 0.03 qq9 * pow_aux_11517 ] <= -200
 qc11: - 20 ab9 + ppp + [ - 33 ab9 ^2 ] <= 5
 qc12: quad_aux_11519 + [ - pow_base_aux_11518 ^2 ] = 0
 qc13: quad_aux_11521 + [ - pow_base_aux_11520 ^2 ] = 0
 qc14: quad_aux_11523 + [ - pow_base_aux_11522 ^2 ] = 0
 qc15: quad_aux_11525 + [ - pow_base_aux_11524 ^2 ] = 0
 qc16: quad_aux_11527 + [ - pow_base_aux_11526 ^2 ] = 0
 qc17: quad_aux_11529 + [ - pow_base_aux_11528 ^2 ] = 0
 qc18: quad_aux_11531 + [ - pow_base_aux_11530 ^2 ] = 0
 qc19: quad_aux_11533 + [ - pow_base_aux_11532 ^2 ] = 0
 qc20: quad_aux_11535 + [ - pow_base_aux_11534 ^2 ] = 0
Bounds
 Constant = 1
General Constraints
 GC0: pow_aux_11507 = POW ( pow_base_aux_11506 ^ 1.5 )
 GC1: pow_aux_11509 = POW ( pow_base_aux_11508 ^ 0.5 )
 GC2: pow_aux_11511 = POW ( pow_base_aux_11510 ^ 1.5 )
 GC3: pow_aux_11513 = POW ( pow_base_aux_11512 ^ 0.5 )
 GC4: pow_aux_11515 = POW ( pow_base_aux_11514 ^ 1.5 )
 GC5: pow_aux_11517 = POW ( pow_base_aux_11516 ^ 0.5 )
 GC6: pow_aux_11537 = POW ( pow_base_aux_11536 ^ 1.5 )
 GC7: pow_aux_11539 = POW ( pow_base_aux_11538 ^ 0.5 )
 GC8: pow_aux_11541 = POW ( pow_base_aux_11540 ^ 0.5 )
 GC9: pow_aux_11543 = POW ( pow_base_aux_11542 ^ 0.5 )
End
