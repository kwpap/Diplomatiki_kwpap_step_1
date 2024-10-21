\ Model Least Squares e
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  quad_aux_13281 + quad_aux_13285 + quad_aux_13289 + quad_aux_13293
   + quad_aux_13297 + quad_aux_13301 + quad_aux_13305 + quad_aux_13309
   + quad_aux_13313 + 10000 quad_aux_13315 + 10000 quad_aux_13317
   + 10000 quad_aux_13319 + 22500 quad_aux_13321 + 22500 quad_aux_13323
   + 22500 quad_aux_13325 + 40000 quad_aux_13331 + 40000 quad_aux_13337
   + 40000 quad_aux_13343
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
   + qq7 - ab7 + qq8 - ab8 + qq9 - ab9 = 1784.68072606113
 R19: - ab5 + pow_base_aux_13278 = 0
 R20: 12 ab5 - ppp + 6 quad_aux_13279 + pow_base_aux_13280 = -1
 R21: - ab3 + pow_base_aux_13282 = 0
 R22: 6 ab3 - ppp + 6 quad_aux_13283 + pow_base_aux_13284 = -1
 R23: - ab4 + pow_base_aux_13286 = 0
 R24: 10 ab4 - ppp + 9 quad_aux_13287 + pow_base_aux_13288 = -7
 R25: - ab9 + pow_base_aux_13290 = 0
 R26: 20 ab9 - ppp + 33 quad_aux_13291 + pow_base_aux_13292 = -5
 R27: - ab8 + pow_base_aux_13294 = 0
 R28: 18 ab8 - ppp + 30 quad_aux_13295 + pow_base_aux_13296 = -4
 R29: - ab7 + pow_base_aux_13298 = 0
 R30: 16 ab7 - ppp + 12 quad_aux_13299 + pow_base_aux_13300 = -3
 R31: - ab2 + pow_base_aux_13302 = 0
 R32: 6 ab2 - ppp + 6 quad_aux_13303 + pow_base_aux_13304 = -3
 R33: - ab6 + pow_base_aux_13306 = 0
 R34: 14 ab6 - ppp + 9 quad_aux_13307 + pow_base_aux_13308 = -2
 R35: - ab1 + pow_base_aux_13310 = 0
 R36: 4 ab1 - ppp + 6 quad_aux_13311 + pow_base_aux_13312 = -2
 R37: 0.001 qq1 + 0.001 qq2 + 0.002 qq3 + pow_base_aux_13314 = 1
 R38: 0.001 qq1 + 0.002 qq2 + 0.001 qq3 + pow_base_aux_13316 = 1
 R39: 0.002 qq1 + 0.001 qq2 + 0.001 qq3 + pow_base_aux_13318 = 1
 R40: 6.6666666666666664e-04 qq4 + 6.6666666666666664e-04 qq5
   + 0.00133333333333333 qq6 + 0.00333333333333333 ppp + pow_base_aux_13320
   = 1
 R41: 6.6666666666666664e-04 qq4 + 0.00133333333333333 qq5
   + 6.6666666666666664e-04 qq6 + 0.00333333333333333 ppp
   + pow_base_aux_13322 = 1
 R42: 0.00133333333333333 qq4 + 6.6666666666666664e-04 qq5
   + 6.6666666666666664e-04 qq6 + 0.00333333333333333 ppp
   + pow_base_aux_13324 = 1
 R43: - qq7 - qq8 - qq9 + pow_base_aux_13326 = 0
 R44: - qq7 - qq8 - qq9 + pow_base_aux_13328 = 0
 R45: - qq7 - qq8 - qq9 + pow_base_aux_13332 = 0
 R46: - qq7 - qq8 - qq9 + pow_base_aux_13334 = 0
 R47: - qq7 - qq8 - qq9 + pow_base_aux_13338 = 0
 R48: - qq7 - qq8 - qq9 + pow_base_aux_13340 = 0
 qc0: quad_aux_13279 + [ - pow_base_aux_13278 ^2 ] = 0
 qc1: quad_aux_13281 + [ - pow_base_aux_13280 ^2 ] = 0
 qc2: quad_aux_13283 + [ - pow_base_aux_13282 ^2 ] = 0
 qc3: quad_aux_13285 + [ - pow_base_aux_13284 ^2 ] = 0
 qc4: quad_aux_13287 + [ - pow_base_aux_13286 ^2 ] = 0
 qc5: quad_aux_13289 + [ - pow_base_aux_13288 ^2 ] = 0
 qc6: quad_aux_13291 + [ - pow_base_aux_13290 ^2 ] = 0
 qc7: quad_aux_13293 + [ - pow_base_aux_13292 ^2 ] = 0
 qc8: quad_aux_13295 + [ - pow_base_aux_13294 ^2 ] = 0
 qc9: quad_aux_13297 + [ - pow_base_aux_13296 ^2 ] = 0
 qc10: quad_aux_13299 + [ - pow_base_aux_13298 ^2 ] = 0
 qc11: quad_aux_13301 + [ - pow_base_aux_13300 ^2 ] = 0
 qc12: quad_aux_13303 + [ - pow_base_aux_13302 ^2 ] = 0
 qc13: quad_aux_13305 + [ - pow_base_aux_13304 ^2 ] = 0
 qc14: quad_aux_13307 + [ - pow_base_aux_13306 ^2 ] = 0
 qc15: quad_aux_13309 + [ - pow_base_aux_13308 ^2 ] = 0
 qc16: quad_aux_13311 + [ - pow_base_aux_13310 ^2 ] = 0
 qc17: quad_aux_13313 + [ - pow_base_aux_13312 ^2 ] = 0
 qc18: quad_aux_13315 + [ - pow_base_aux_13314 ^2 ] = 0
 qc19: quad_aux_13317 + [ - pow_base_aux_13316 ^2 ] = 0
 qc20: quad_aux_13319 + [ - pow_base_aux_13318 ^2 ] = 0
 qc21: quad_aux_13321 + [ - pow_base_aux_13320 ^2 ] = 0
 qc22: quad_aux_13323 + [ - pow_base_aux_13322 ^2 ] = 0
 qc23: quad_aux_13325 + [ - pow_base_aux_13324 ^2 ] = 0
 qc24: 0.0025 ppp + 0.0001 pow_aux_13327 + pow_base_aux_13330 + [
   0.00015 qq7 * pow_aux_13329 ] = 1
 qc25: quad_aux_13331 + [ - pow_base_aux_13330 ^2 ] = 0
 qc26: 0.0025 ppp + 0.0001 pow_aux_13333 + pow_base_aux_13336 + [
   0.00015 qq8 * pow_aux_13335 ] = 1
 qc27: quad_aux_13337 + [ - pow_base_aux_13336 ^2 ] = 0
 qc28: 0.0025 ppp + 0.0001 pow_aux_13339 + pow_base_aux_13342 + [
   0.00015 qq9 * pow_aux_13341 ] = 1
 qc29: quad_aux_13343 + [ - pow_base_aux_13342 ^2 ] = 0
Bounds
General Constraints
 GC0: pow_aux_13327 = POW ( pow_base_aux_13326 ^ 1.5 )
 GC1: pow_aux_13329 = POW ( pow_base_aux_13328 ^ 0.5 )
 GC2: pow_aux_13333 = POW ( pow_base_aux_13332 ^ 1.5 )
 GC3: pow_aux_13335 = POW ( pow_base_aux_13334 ^ 0.5 )
 GC4: pow_aux_13339 = POW ( pow_base_aux_13338 ^ 1.5 )
 GC5: pow_aux_13341 = POW ( pow_base_aux_13340 ^ 0.5 )
End
