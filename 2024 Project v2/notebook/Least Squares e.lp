\ Model Least Squares e
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  pow_aux_917 + pow_aux_921 + pow_aux_925 + pow_aux_929 + pow_aux_933
   + pow_aux_937 + pow_aux_941 + 100 pow_aux_945 + 121 pow_aux_949
   + 10000 pow_aux_951 + 10000 pow_aux_953 + 10000 pow_aux_955
   + 22500 pow_aux_957 + 22500 pow_aux_959 + 22500 pow_aux_961
   + 40000 pow_aux_967 + 40000 pow_aux_973 + 40000 pow_aux_979
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
   + qq7 - ab7 + qq8 - ab8 + qq9 - ab9 = 117.3289016365619
 R19: - ab5 + pow_base_aux_914 = 0
 R20: 12 ab5 - ppp + 6 pow_aux_915 + pow_base_aux_916 = -1
 R21: - ab4 + pow_base_aux_918 = 0
 R22: 10 ab4 - ppp + 9 pow_aux_919 + pow_base_aux_920 = -7
 R23: - ab9 + pow_base_aux_922 = 0
 R24: 20 ab9 - ppp + 33 pow_aux_923 + pow_base_aux_924 = -5
 R25: - ab3 + pow_base_aux_926 = 0
 R26: 8 ab3 - ppp + 15 pow_aux_927 + pow_base_aux_928 = -5
 R27: - ab8 + pow_base_aux_930 = 0
 R28: 18 ab8 - ppp + 30 pow_aux_931 + pow_base_aux_932 = -4
 R29: - ab7 + pow_base_aux_934 = 0
 R30: 16 ab7 - ppp + 12 pow_aux_935 + pow_base_aux_936 = -3
 R31: - ab6 + pow_base_aux_938 = 0
 R32: 14 ab6 - ppp + 9 pow_aux_939 + pow_base_aux_940 = -2
 R33: - ab1 + pow_base_aux_942 = 0
 R34: 0.4 ab1 - 0.1 ppp + 0.03 pow_aux_943 + pow_base_aux_944 = -1
 R35: - ab2 + pow_base_aux_946 = 0
 R36: 0.5454545454545454 ab2 - 0.0909090909090909 ppp
   + 0.0545454545454545 pow_aux_947 + pow_base_aux_948 = -1
 R37: 0.001 qq1 + 0.001 qq2 + 0.002 qq3 + 0.01 ppp + pow_base_aux_950 = 1
 R38: 0.001 qq1 + 0.002 qq2 + 0.001 qq3 + 0.01 ppp + pow_base_aux_952 = 1
 R39: 0.002 qq1 + 0.001 qq2 + 0.001 qq3 + 0.01 ppp + pow_base_aux_954 = 1
 R40: 6.6666666666666664e-04 qq4 + 6.6666666666666664e-04 qq5
   + 0.00133333333333333 qq6 + 0.00666666666666667 ppp + pow_base_aux_956
   = 1
 R41: 6.6666666666666664e-04 qq4 + 0.00133333333333333 qq5
   + 6.6666666666666664e-04 qq6 + 0.00666666666666667 ppp
   + pow_base_aux_958 = 1
 R42: 0.00133333333333333 qq4 + 6.6666666666666664e-04 qq5
   + 6.6666666666666664e-04 qq6 + 0.00666666666666667 ppp
   + pow_base_aux_960 = 1
 R43: - qq7 - qq8 - qq9 + pow_base_aux_962 = 0
 R44: - qq7 - qq8 - qq9 + pow_base_aux_964 = 0
 R45: - qq7 - qq8 - qq9 + pow_base_aux_968 = 0
 R46: - qq7 - qq8 - qq9 + pow_base_aux_970 = 0
 R47: - qq7 - qq8 - qq9 + pow_base_aux_974 = 0
 R48: - qq7 - qq8 - qq9 + pow_base_aux_976 = 0
 qc0: 0.005 ppp + 0.0001 pow_aux_963 + pow_base_aux_966 + [
   0.00015 qq7 * pow_aux_965 ] = 1
 qc1: 0.005 ppp + 0.0001 pow_aux_969 + pow_base_aux_972 + [
   0.00015 qq8 * pow_aux_971 ] = 1
 qc2: 0.005 ppp + 0.0001 pow_aux_975 + pow_base_aux_978 + [
   0.00015 qq9 * pow_aux_977 ] = 1
Bounds
General Constraints
 GC0: pow_aux_915 = POW ( pow_base_aux_914 ^ 2 )
 GC1: pow_aux_917 = POW ( pow_base_aux_916 ^ 2 )
 GC2: pow_aux_919 = POW ( pow_base_aux_918 ^ 2 )
 GC3: pow_aux_921 = POW ( pow_base_aux_920 ^ 2 )
 GC4: pow_aux_923 = POW ( pow_base_aux_922 ^ 2 )
 GC5: pow_aux_925 = POW ( pow_base_aux_924 ^ 2 )
 GC6: pow_aux_927 = POW ( pow_base_aux_926 ^ 2 )
 GC7: pow_aux_929 = POW ( pow_base_aux_928 ^ 2 )
 GC8: pow_aux_931 = POW ( pow_base_aux_930 ^ 2 )
 GC9: pow_aux_933 = POW ( pow_base_aux_932 ^ 2 )
 GC10: pow_aux_935 = POW ( pow_base_aux_934 ^ 2 )
 GC11: pow_aux_937 = POW ( pow_base_aux_936 ^ 2 )
 GC12: pow_aux_939 = POW ( pow_base_aux_938 ^ 2 )
 GC13: pow_aux_941 = POW ( pow_base_aux_940 ^ 2 )
 GC14: pow_aux_943 = POW ( pow_base_aux_942 ^ 2 )
 GC15: pow_aux_945 = POW ( pow_base_aux_944 ^ 2 )
 GC16: pow_aux_947 = POW ( pow_base_aux_946 ^ 2 )
 GC17: pow_aux_949 = POW ( pow_base_aux_948 ^ 2 )
 GC18: pow_aux_951 = POW ( pow_base_aux_950 ^ 2 )
 GC19: pow_aux_953 = POW ( pow_base_aux_952 ^ 2 )
 GC20: pow_aux_955 = POW ( pow_base_aux_954 ^ 2 )
 GC21: pow_aux_957 = POW ( pow_base_aux_956 ^ 2 )
 GC22: pow_aux_959 = POW ( pow_base_aux_958 ^ 2 )
 GC23: pow_aux_961 = POW ( pow_base_aux_960 ^ 2 )
 GC24: pow_aux_963 = POW ( pow_base_aux_962 ^ 1.5 )
 GC25: pow_aux_965 = POW ( pow_base_aux_964 ^ 0.5 )
 GC26: pow_aux_967 = POW ( pow_base_aux_966 ^ 2 )
 GC27: pow_aux_969 = POW ( pow_base_aux_968 ^ 1.5 )
 GC28: pow_aux_971 = POW ( pow_base_aux_970 ^ 0.5 )
 GC29: pow_aux_973 = POW ( pow_base_aux_972 ^ 2 )
 GC30: pow_aux_975 = POW ( pow_base_aux_974 ^ 1.5 )
 GC31: pow_aux_977 = POW ( pow_base_aux_976 ^ 0.5 )
 GC32: pow_aux_979 = POW ( pow_base_aux_978 ^ 2 )
End
