\ Model positive_constraints_ab
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  - 0.4 qq1 - 4 ab1 - 0.4 qq2 - 6 ab2 - 0.4 qq3 - 8 ab3 - 0.4 qq4 - 10 ab4
   - 0.4 qq5 - 12 ab5 - 0.4 qq6 - 14 ab6 - 16 ab7 - 18 ab8 - 20 ab9
   - 33 pow_aux_305 - 30 pow_aux_307 - 15 pow_aux_309 - 12 pow_aux_311
   - 9 pow_aux_313 - 9 pow_aux_315 - 6 pow_aux_317 - 0.06 pow_aux_319
   - 0.3 pow_aux_321 - 0.6 pow_aux_323 + 1302 Constant + [
   - 0.06 qq7 * pow_aux_325 - 0.06 qq8 * pow_aux_327
   - 0.06 qq9 * pow_aux_329 ] / 2 
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
 R19: - ab1 + pow_base_aux_274 = 0
 R20: - 4 ab1 + ppp - 0.3 pow_aux_275 >= 10
 R21: - 0.1 qq1 - 0.2 qq2 - 0.1 qq3 - ppp >= -100
 R22: - ab2 + pow_base_aux_276 = 0
 R23: - 6 ab2 + ppp - 0.6 pow_aux_277 >= 11
 R24: - 0.1 qq1 - 0.1 qq2 - 0.2 qq3 - ppp >= -100
 R25: - ab3 + pow_base_aux_278 = 0
 R26: - 8 ab3 + ppp - 15 pow_aux_279 >= 5
 R27: - 0.2 qq4 - 0.1 qq5 - 0.1 qq6 - ppp >= -150
 R28: - ab4 + pow_base_aux_280 = 0
 R29: - 10 ab4 + ppp - 9 pow_aux_281 >= 7
 R30: - 0.1 qq4 - 0.2 qq5 - 0.1 qq6 - ppp >= -150
 R31: - ab5 + pow_base_aux_282 = 0
 R32: - 12 ab5 + ppp - 6 pow_aux_283 >= 1
 R33: - 0.1 qq4 - 0.1 qq5 - 0.2 qq6 - ppp >= -150
 R34: - ab6 + pow_base_aux_284 = 0
 R35: - 14 ab6 + ppp - 9 pow_aux_285 >= 2
 R36: - qq7 - qq8 - qq9 + pow_base_aux_286 = 0
 R37: - qq7 - qq8 - qq9 + pow_base_aux_288 = 0
 R38: - ab7 + pow_base_aux_290 = 0
 R39: - 16 ab7 + ppp - 12 pow_aux_291 >= 3
 R40: - qq7 - qq8 - qq9 + pow_base_aux_292 = 0
 R41: - qq7 - qq8 - qq9 + pow_base_aux_294 = 0
 R42: - ab8 + pow_base_aux_296 = 0
 R43: - 18 ab8 + ppp - 30 pow_aux_297 >= 4
 R44: - qq7 - qq8 - qq9 + pow_base_aux_298 = 0
 R45: - qq7 - qq8 - qq9 + pow_base_aux_300 = 0
 R46: - ab9 + pow_base_aux_302 = 0
 R47: - 20 ab9 + ppp - 33 pow_aux_303 >= 5
 R48: qq1 - ab1 + qq2 - ab2 + qq3 - ab3 + qq4 - ab4 + qq5 - ab5 + qq6 - ab6
   + qq7 - ab7 + qq8 - ab8 + qq9 - ab9 = 117.3289016365619
 R49: - ab9 + pow_base_aux_304 = 0
 R50: - ab8 + pow_base_aux_306 = 0
 R51: - ab3 + pow_base_aux_308 = 0
 R52: - ab7 + pow_base_aux_310 = 0
 R53: - ab4 + pow_base_aux_312 = 0
 R54: - ab6 + pow_base_aux_314 = 0
 R55: - ab5 + pow_base_aux_316 = 0
 R56: - qq7 - qq8 - qq9 + pow_base_aux_318 = 0
 R57: - ab1 + pow_base_aux_320 = 0
 R58: - ab2 + pow_base_aux_322 = 0
 R59: - qq7 - qq8 - qq9 + pow_base_aux_324 = 0
 R60: - qq7 - qq8 - qq9 + pow_base_aux_326 = 0
 R61: - qq7 - qq8 - qq9 + pow_base_aux_328 = 0
 qc0: - ppp - 0.02 pow_aux_287 + [ - 0.03 qq7 * pow_aux_289 ] >= -200
 qc1: - ppp - 0.02 pow_aux_293 + [ - 0.03 qq8 * pow_aux_295 ] >= -200
 qc2: - ppp - 0.02 pow_aux_299 + [ - 0.03 qq9 * pow_aux_301 ] >= -200
Bounds
 Constant = 1
General Constraints
 GC0: pow_aux_275 = POW ( pow_base_aux_274 ^ 2 )
 GC1: pow_aux_277 = POW ( pow_base_aux_276 ^ 2 )
 GC2: pow_aux_279 = POW ( pow_base_aux_278 ^ 2 )
 GC3: pow_aux_281 = POW ( pow_base_aux_280 ^ 2 )
 GC4: pow_aux_283 = POW ( pow_base_aux_282 ^ 2 )
 GC5: pow_aux_285 = POW ( pow_base_aux_284 ^ 2 )
 GC6: pow_aux_287 = POW ( pow_base_aux_286 ^ 1.5 )
 GC7: pow_aux_289 = POW ( pow_base_aux_288 ^ 0.5 )
 GC8: pow_aux_291 = POW ( pow_base_aux_290 ^ 2 )
 GC9: pow_aux_293 = POW ( pow_base_aux_292 ^ 1.5 )
 GC10: pow_aux_295 = POW ( pow_base_aux_294 ^ 0.5 )
 GC11: pow_aux_297 = POW ( pow_base_aux_296 ^ 2 )
 GC12: pow_aux_299 = POW ( pow_base_aux_298 ^ 1.5 )
 GC13: pow_aux_301 = POW ( pow_base_aux_300 ^ 0.5 )
 GC14: pow_aux_303 = POW ( pow_base_aux_302 ^ 2 )
 GC15: pow_aux_305 = POW ( pow_base_aux_304 ^ 2 )
 GC16: pow_aux_307 = POW ( pow_base_aux_306 ^ 2 )
 GC17: pow_aux_309 = POW ( pow_base_aux_308 ^ 2 )
 GC18: pow_aux_311 = POW ( pow_base_aux_310 ^ 2 )
 GC19: pow_aux_313 = POW ( pow_base_aux_312 ^ 2 )
 GC20: pow_aux_315 = POW ( pow_base_aux_314 ^ 2 )
 GC21: pow_aux_317 = POW ( pow_base_aux_316 ^ 2 )
 GC22: pow_aux_319 = POW ( pow_base_aux_318 ^ 1.5 )
 GC23: pow_aux_321 = POW ( pow_base_aux_320 ^ 2 )
 GC24: pow_aux_323 = POW ( pow_base_aux_322 ^ 2 )
 GC25: pow_aux_325 = POW ( pow_base_aux_324 ^ 0.5 )
 GC26: pow_aux_327 = POW ( pow_base_aux_326 ^ 0.5 )
 GC27: pow_aux_329 = POW ( pow_base_aux_328 ^ 0.5 )
End
