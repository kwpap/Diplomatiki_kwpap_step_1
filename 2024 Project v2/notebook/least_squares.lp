\ Model Least Squares
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  quad_aux_8661 + quad_aux_8665 + quad_aux_8669 + quad_aux_8673
   + quad_aux_8677 + quad_aux_8681 + quad_aux_8685 + quad_aux_8689
   + quad_aux_8693 + 7921 quad_aux_8697 + 8100 quad_aux_8701
   + 9025 quad_aux_8705 + 20449 quad_aux_8709 + 21904 quad_aux_8713
   + 22201 quad_aux_8717 + 38025 quad_aux_8725 + 38416 quad_aux_8733
   + 38809 quad_aux_8741 + 3.1850852939740834e+06 quad_aux_8743
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
 R9: - qq5 + xx5 + pow_base_aux_8658 = 0
 R10: - 12 qq5 + 12 xx5 + ppp - 6 quad_aux_8659 + pow_base_aux_8660 = 1
 R11: - qq6 + xx6 + pow_base_aux_8662 = 0
 R12: - 14 qq6 + 14 xx6 + ppp - 9 quad_aux_8663 + pow_base_aux_8664 = 2
 R13: - qq7 + xx7 + pow_base_aux_8666 = 0
 R14: - 16 qq7 + 16 xx7 + ppp - 12 quad_aux_8667 + pow_base_aux_8668 = 3
 R15: - qq8 + xx8 + pow_base_aux_8670 = 0
 R16: - 18 qq8 + 18 xx8 + ppp - 30 quad_aux_8671 + pow_base_aux_8672 = 4
 R17: - qq9 + xx9 + pow_base_aux_8674 = 0
 R18: - 20 qq9 + 20 xx9 + ppp - 33 quad_aux_8675 + pow_base_aux_8676 = 5
 R19: - qq3 + xx3 + pow_base_aux_8678 = 0
 R20: - 8 qq3 + 8 xx3 + ppp - 15 quad_aux_8679 + pow_base_aux_8680 = 5
 R21: - qq4 + xx4 + pow_base_aux_8682 = 0
 R22: - 10 qq4 + 10 xx4 + ppp - 9 quad_aux_8683 + pow_base_aux_8684 = 7
 R23: - qq1 + xx1 + pow_base_aux_8686 = 0
 R24: - 4 qq1 + 4 xx1 + ppp - 6 quad_aux_8687 + pow_base_aux_8688 = 10
 R25: - qq2 + xx2 + pow_base_aux_8690 = 0
 R26: - 6 qq2 + 6 xx2 + ppp - 6 quad_aux_8691 + pow_base_aux_8692 = 11
 R27: - qq2 + xx2 + pow_base_aux_8694 = 0
 R28: 0.00112359550561798 qq1 + 0.0696629213483146 qq2
   - 0.0674157303370786 xx2 + 0.00112359550561798 qq3
   - 0.0112359550561798 ppp + 0.0674157303370786 quad_aux_8695
   + pow_base_aux_8696 = 1
 R29: - qq1 + xx1 + pow_base_aux_8698 = 0
 R30: 0.0466666666666667 qq1 - 0.0444444444444444 xx1
   + 0.00111111111111111 qq2 + 0.00111111111111111 qq3
   - 0.0111111111111111 ppp + 0.0666666666666667 quad_aux_8699
   + pow_base_aux_8700 = 1
 R31: - qq3 + xx3 + pow_base_aux_8702 = 0
 R32: 0.00105263157894737 qq1 + 0.00105263157894737 qq2
   + 0.0863157894736842 qq3 - 0.0842105263157895 xx3
   - 0.0105263157894737 ppp + 0.1578947368421053 quad_aux_8703
   + pow_base_aux_8704 = 1
 R33: - qq4 + xx4 + pow_base_aux_8706 = 0
 R34: 0.0713286713286713 qq4 - 0.0699300699300699 xx4
   + 6.993006993006993e-04 qq5 + 6.993006993006993e-04 qq6
   + 0.0629370629370629 quad_aux_8707 + pow_base_aux_8708 = 1
 R35: - qq6 + xx6 + pow_base_aux_8710 = 0
 R36: 6.7567567567567571e-04 qq4 + 6.7567567567567571e-04 qq5
   + 0.095945945945946 qq6 - 0.0945945945945946 xx6
   + 0.0608108108108108 quad_aux_8711 + pow_base_aux_8712 = 1
 R37: - qq5 + xx5 + pow_base_aux_8714 = 0
 R38: 0.000671141 qq4 + 0.0818791946308725 qq5 - 0.0805369127516778 xx5
   + 0.000671141 qq6 + 0.0402684563758389 quad_aux_8715 + pow_base_aux_8716
   = 1
 R39: - qq9 + xx9 + pow_base_aux_8718 = 0
 R40: - qq7 - qq8 - qq9 + pow_base_aux_8720 = 0
 R41: - qq7 - qq8 - qq9 + pow_base_aux_8722 = 0
 R42: - qq8 + xx8 + pow_base_aux_8726 = 0
 R43: - qq7 - qq8 - qq9 + pow_base_aux_8728 = 0
 R44: - qq7 - qq8 - qq9 + pow_base_aux_8730 = 0
 R45: - qq7 + xx7 + pow_base_aux_8734 = 0
 R46: - qq7 - qq8 - qq9 + pow_base_aux_8736 = 0
 R47: - qq7 - qq8 - qq9 + pow_base_aux_8738 = 0
 R48: 5.6032431201688635e-04 xx1 + 5.6032431201688635e-04 xx2
   + 5.6032431201688635e-04 xx3 + 5.6032431201688635e-04 xx4
   + 5.6032431201688635e-04 xx5 + 5.6032431201688635e-04 xx6
   + 5.6032431201688635e-04 xx7 + 5.6032431201688635e-04 xx8
   + 5.6032431201688635e-04 xx9 + pow_base_aux_8742 = 1
 qc0: quad_aux_8659 + [ - pow_base_aux_8658 ^2 ] = 0
 qc1: quad_aux_8661 + [ - pow_base_aux_8660 ^2 ] = 0
 qc2: quad_aux_8663 + [ - pow_base_aux_8662 ^2 ] = 0
 qc3: quad_aux_8665 + [ - pow_base_aux_8664 ^2 ] = 0
 qc4: quad_aux_8667 + [ - pow_base_aux_8666 ^2 ] = 0
 qc5: quad_aux_8669 + [ - pow_base_aux_8668 ^2 ] = 0
 qc6: quad_aux_8671 + [ - pow_base_aux_8670 ^2 ] = 0
 qc7: quad_aux_8673 + [ - pow_base_aux_8672 ^2 ] = 0
 qc8: quad_aux_8675 + [ - pow_base_aux_8674 ^2 ] = 0
 qc9: quad_aux_8677 + [ - pow_base_aux_8676 ^2 ] = 0
 qc10: quad_aux_8679 + [ - pow_base_aux_8678 ^2 ] = 0
 qc11: quad_aux_8681 + [ - pow_base_aux_8680 ^2 ] = 0
 qc12: quad_aux_8683 + [ - pow_base_aux_8682 ^2 ] = 0
 qc13: quad_aux_8685 + [ - pow_base_aux_8684 ^2 ] = 0
 qc14: quad_aux_8687 + [ - pow_base_aux_8686 ^2 ] = 0
 qc15: quad_aux_8689 + [ - pow_base_aux_8688 ^2 ] = 0
 qc16: quad_aux_8691 + [ - pow_base_aux_8690 ^2 ] = 0
 qc17: quad_aux_8693 + [ - pow_base_aux_8692 ^2 ] = 0
 qc18: quad_aux_8695 + [ - pow_base_aux_8694 ^2 ] = 0
 qc19: quad_aux_8697 + [ - pow_base_aux_8696 ^2 ] = 0
 qc20: quad_aux_8699 + [ - pow_base_aux_8698 ^2 ] = 0
 qc21: quad_aux_8701 + [ - pow_base_aux_8700 ^2 ] = 0
 qc22: quad_aux_8703 + [ - pow_base_aux_8702 ^2 ] = 0
 qc23: quad_aux_8705 + [ - pow_base_aux_8704 ^2 ] = 0
 qc24: quad_aux_8707 + [ - pow_base_aux_8706 ^2 ] = 0
 qc25: quad_aux_8709 + [ - pow_base_aux_8708 ^2 ] = 0
 qc26: quad_aux_8711 + [ - pow_base_aux_8710 ^2 ] = 0
 qc27: quad_aux_8713 + [ - pow_base_aux_8712 ^2 ] = 0
 qc28: quad_aux_8715 + [ - pow_base_aux_8714 ^2 ] = 0
 qc29: quad_aux_8717 + [ - pow_base_aux_8716 ^2 ] = 0
 qc30: quad_aux_8719 + [ - pow_base_aux_8718 ^2 ] = 0
 qc31: 0.1025641025641026 qq9 - 0.1025641025641026 xx9
   + 0.1692307692307692 quad_aux_8719 + 1.0256410256410256e-04 pow_aux_8721
   + pow_base_aux_8724 + [ 1.5384615384615385e-04 qq9 * pow_aux_8723 ] = 1
 qc32: quad_aux_8725 + [ - pow_base_aux_8724 ^2 ] = 0
 qc33: quad_aux_8727 + [ - pow_base_aux_8726 ^2 ] = 0
 qc34: 0.0918367346938776 qq8 - 0.0918367346938776 xx8
   + 0.1530612244897959 quad_aux_8727 + 1.0204081632653062e-04 pow_aux_8729
   + pow_base_aux_8732 + [ 1.5306122448979591e-04 qq8 * pow_aux_8731 ] = 1
 qc35: quad_aux_8733 + [ - pow_base_aux_8732 ^2 ] = 0
 qc36: quad_aux_8735 + [ - pow_base_aux_8734 ^2 ] = 0
 qc37: 0.0812182741116751 qq7 - 0.0812182741116751 xx7
   + 0.0609137055837563 quad_aux_8735 + 1.0152284263959391e-04 pow_aux_8737
   + pow_base_aux_8740 + [ 1.5228426395939087e-04 qq7 * pow_aux_8739 ] = 1
 qc38: quad_aux_8741 + [ - pow_base_aux_8740 ^2 ] = 0
 qc39: quad_aux_8743 + [ - pow_base_aux_8742 ^2 ] = 0
Bounds
General Constraints
 GC0: pow_aux_8721 = POW ( pow_base_aux_8720 ^ 1.5 )
 GC1: pow_aux_8723 = POW ( pow_base_aux_8722 ^ 0.5 )
 GC2: pow_aux_8729 = POW ( pow_base_aux_8728 ^ 1.5 )
 GC3: pow_aux_8731 = POW ( pow_base_aux_8730 ^ 0.5 )
 GC4: pow_aux_8737 = POW ( pow_base_aux_8736 ^ 1.5 )
 GC5: pow_aux_8739 = POW ( pow_base_aux_8738 ^ 0.5 )
End
