\ Model optimization_concave_formulation_ab
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  0.4 qq1 + 4 ab1 + 0.4 qq2 + 6 ab2 + 0.4 qq3 + 6 ab3 + 0.4 qq4 + 10 ab4
   + 0.4 qq5 + 12 ab5 + 0.4 qq6 + 14 ab6 + 16 ab7 + 18 ab8 + 20 ab9
   + 6 quad_aux_3919 + 6 quad_aux_3921 + 6 quad_aux_3923 + 6 quad_aux_3925
   + 9 quad_aux_3927 + 9 quad_aux_3929 + 12 quad_aux_3931
   + 30 quad_aux_3933 + 33 quad_aux_3935 + 0.06 pow_aux_3937
   - 1322 Constant + [ 0.06 qq7 * pow_aux_3939 + 0.06 qq8 * pow_aux_3941
   + 0.06 qq9 * pow_aux_3943 ] / 2 
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
 R18: - 0.2 qq1 - 0.1 qq2 - 0.1 qq3 - ppp <= -100
 R19: - 0.1 qq1 - 0.2 qq2 - 0.1 qq3 - ppp <= -100
 R20: - 0.1 qq1 - 0.1 qq2 - 0.2 qq3 - ppp <= -100
 R21: - 0.2 qq4 - 0.1 qq5 - 0.1 qq6 - ppp <= -150
 R22: - 0.1 qq4 - 0.2 qq5 - 0.1 qq6 - ppp <= -150
 R23: - 0.1 qq4 - 0.1 qq5 - 0.2 qq6 - ppp <= -150
 R24: - qq7 - qq8 - qq9 + pow_base_aux_3906 = 0
 R25: - qq7 - qq8 - qq9 + pow_base_aux_3908 = 0
 R26: - qq7 - qq8 - qq9 + pow_base_aux_3910 = 0
 R27: - qq7 - qq8 - qq9 + pow_base_aux_3912 = 0
 R28: - qq7 - qq8 - qq9 + pow_base_aux_3914 = 0
 R29: - qq7 - qq8 - qq9 + pow_base_aux_3916 = 0
 R30: qq1 - ab1 + qq2 - ab2 + qq3 - ab3 + qq4 - ab4 + qq5 - ab5 + qq6 - ab6
   + qq7 - ab7 + qq8 - ab8 + qq9 - ab9 = 2230.850907576413
 R31: - ab1 + pow_base_aux_3918 = 0
 R32: - ab2 + pow_base_aux_3920 = 0
 R33: - ab3 + pow_base_aux_3922 = 0
 R34: - ab5 + pow_base_aux_3924 = 0
 R35: - ab4 + pow_base_aux_3926 = 0
 R36: - ab6 + pow_base_aux_3928 = 0
 R37: - ab7 + pow_base_aux_3930 = 0
 R38: - ab8 + pow_base_aux_3932 = 0
 R39: - ab9 + pow_base_aux_3934 = 0
 R40: - qq7 - qq8 - qq9 + pow_base_aux_3936 = 0
 R41: - qq7 - qq8 - qq9 + pow_base_aux_3938 = 0
 R42: - qq7 - qq8 - qq9 + pow_base_aux_3940 = 0
 R43: - qq7 - qq8 - qq9 + pow_base_aux_3942 = 0
 qc0: - 4 ab1 + ppp + [ - 6 ab1 ^2 ] <= 2
 qc1: - 6 ab2 + ppp + [ - 6 ab2 ^2 ] <= 3
 qc2: - 6 ab3 + ppp + [ - 6 ab3 ^2 ] <= 1
 qc3: - 10 ab4 + ppp + [ - 9 ab4 ^2 ] <= 7
 qc4: - 12 ab5 + ppp + [ - 6 ab5 ^2 ] <= 1
 qc5: - 14 ab6 + ppp + [ - 9 ab6 ^2 ] <= 2
 qc6: - ppp - 0.02 pow_aux_3907 + [ - 0.03 qq7 * pow_aux_3909 ] <= -200
 qc7: - 16 ab7 + ppp + [ - 12 ab7 ^2 ] <= 3
 qc8: - ppp - 0.02 pow_aux_3911 + [ - 0.03 qq8 * pow_aux_3913 ] <= -200
 qc9: - 18 ab8 + ppp + [ - 30 ab8 ^2 ] <= 4
 qc10: - ppp - 0.02 pow_aux_3915 + [ - 0.03 qq9 * pow_aux_3917 ] <= -200
 qc11: - 20 ab9 + ppp + [ - 33 ab9 ^2 ] <= 5
 qc12: quad_aux_3919 + [ - pow_base_aux_3918 ^2 ] = 0
 qc13: quad_aux_3921 + [ - pow_base_aux_3920 ^2 ] = 0
 qc14: quad_aux_3923 + [ - pow_base_aux_3922 ^2 ] = 0
 qc15: quad_aux_3925 + [ - pow_base_aux_3924 ^2 ] = 0
 qc16: quad_aux_3927 + [ - pow_base_aux_3926 ^2 ] = 0
 qc17: quad_aux_3929 + [ - pow_base_aux_3928 ^2 ] = 0
 qc18: quad_aux_3931 + [ - pow_base_aux_3930 ^2 ] = 0
 qc19: quad_aux_3933 + [ - pow_base_aux_3932 ^2 ] = 0
 qc20: quad_aux_3935 + [ - pow_base_aux_3934 ^2 ] = 0
Bounds
 Constant = 1
General Constraints
 GC0: pow_aux_3907 = POW ( pow_base_aux_3906 ^ 1.5 )
 GC1: pow_aux_3909 = POW ( pow_base_aux_3908 ^ 0.5 )
 GC2: pow_aux_3911 = POW ( pow_base_aux_3910 ^ 1.5 )
 GC3: pow_aux_3913 = POW ( pow_base_aux_3912 ^ 0.5 )
 GC4: pow_aux_3915 = POW ( pow_base_aux_3914 ^ 1.5 )
 GC5: pow_aux_3917 = POW ( pow_base_aux_3916 ^ 0.5 )
 GC6: pow_aux_3937 = POW ( pow_base_aux_3936 ^ 1.5 )
 GC7: pow_aux_3939 = POW ( pow_base_aux_3938 ^ 0.5 )
 GC8: pow_aux_3941 = POW ( pow_base_aux_3940 ^ 0.5 )
 GC9: pow_aux_3943 = POW ( pow_base_aux_3942 ^ 0.5 )
End
