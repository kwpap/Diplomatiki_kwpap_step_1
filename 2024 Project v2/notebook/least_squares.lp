\ Model Least Squares
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  pow_aux_333 + pow_aux_337 + pow_aux_341 + pow_aux_345 + pow_aux_349
   + pow_aux_353 + pow_aux_357 + 100 pow_aux_361 + 121 pow_aux_365
   + 7921 pow_aux_369 + 8100 pow_aux_373 + 9025 pow_aux_377
   + 20449 pow_aux_381 + 21904 pow_aux_385 + 22201 pow_aux_389
   + 38025 pow_aux_397 + 38416 pow_aux_405 + 38809 pow_aux_413
   + 13766.07115924202 pow_aux_415
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
 R9: - qq5 + xx5 + pow_base_aux_330 = 0
 R10: - 12 qq5 + 12 xx5 + ppp - 6 pow_aux_331 + pow_base_aux_332 = 1
 R11: - qq6 + xx6 + pow_base_aux_334 = 0
 R12: - 14 qq6 + 14 xx6 + ppp - 9 pow_aux_335 + pow_base_aux_336 = 2
 R13: - qq7 + xx7 + pow_base_aux_338 = 0
 R14: - 16 qq7 + 16 xx7 + ppp - 12 pow_aux_339 + pow_base_aux_340 = 3
 R15: - qq8 + xx8 + pow_base_aux_342 = 0
 R16: - 18 qq8 + 18 xx8 + ppp - 30 pow_aux_343 + pow_base_aux_344 = 4
 R17: - qq9 + xx9 + pow_base_aux_346 = 0
 R18: - 20 qq9 + 20 xx9 + ppp - 33 pow_aux_347 + pow_base_aux_348 = 5
 R19: - qq3 + xx3 + pow_base_aux_350 = 0
 R20: - 8 qq3 + 8 xx3 + ppp - 15 pow_aux_351 + pow_base_aux_352 = 5
 R21: - qq4 + xx4 + pow_base_aux_354 = 0
 R22: - 10 qq4 + 10 xx4 + ppp - 9 pow_aux_355 + pow_base_aux_356 = 7
 R23: - qq1 + xx1 + pow_base_aux_358 = 0
 R24: - 0.4 qq1 + 0.4 xx1 + 0.1 ppp - 0.03 pow_aux_359 + pow_base_aux_360
   = 1
 R25: - qq2 + xx2 + pow_base_aux_362 = 0
 R26: - 0.5454545454545454 qq2 + 0.5454545454545454 xx2
   + 0.0909090909090909 ppp - 0.0545454545454546 pow_aux_363
   + pow_base_aux_364 = 1
 R27: - qq2 + xx2 + pow_base_aux_366 = 0
 R28: 0.00112359550561798 qq1 + 0.0696629213483146 qq2
   - 0.0674157303370786 xx2 + 0.00112359550561798 qq3
   + 0.00674157303370787 pow_aux_367 + pow_base_aux_368 = 1
 R29: - qq1 + xx1 + pow_base_aux_370 = 0
 R30: 0.0466666666666667 qq1 - 0.0444444444444444 xx1
   + 0.00111111111111111 qq2 + 0.00111111111111111 qq3
   + 0.00333333333333333 pow_aux_371 + pow_base_aux_372 = 1
 R31: - qq3 + xx3 + pow_base_aux_374 = 0
 R32: 0.00105263157894737 qq1 + 0.00105263157894737 qq2
   + 0.0863157894736842 qq3 - 0.0842105263157895 xx3
   + 0.1578947368421053 pow_aux_375 + pow_base_aux_376 = 1
 R33: - qq4 + xx4 + pow_base_aux_378 = 0
 R34: 0.0713286713286713 qq4 - 0.0699300699300699 xx4
   + 6.993006993006993e-04 qq5 + 6.993006993006993e-04 qq6
   + 0.0629370629370629 pow_aux_379 + pow_base_aux_380 = 1
 R35: - qq6 + xx6 + pow_base_aux_382 = 0
 R36: 6.7567567567567571e-04 qq4 + 6.7567567567567571e-04 qq5
   + 0.095945945945946 qq6 - 0.0945945945945946 xx6
   + 0.0608108108108108 pow_aux_383 + pow_base_aux_384 = 1
 R37: - qq5 + xx5 + pow_base_aux_386 = 0
 R38: 0.000671141 qq4 + 0.0818791946308725 qq5 - 0.0805369127516778 xx5
   + 0.000671141 qq6 + 0.0402684563758389 pow_aux_387 + pow_base_aux_388
   = 1
 R39: - qq9 + xx9 + pow_base_aux_390 = 0
 R40: - qq7 - qq8 - qq9 + pow_base_aux_392 = 0
 R41: - qq7 - qq8 - qq9 + pow_base_aux_394 = 0
 R42: - qq8 + xx8 + pow_base_aux_398 = 0
 R43: - qq7 - qq8 - qq9 + pow_base_aux_400 = 0
 R44: - qq7 - qq8 - qq9 + pow_base_aux_402 = 0
 R45: - qq7 + xx7 + pow_base_aux_406 = 0
 R46: - qq7 - qq8 - qq9 + pow_base_aux_408 = 0
 R47: - qq7 - qq8 - qq9 + pow_base_aux_410 = 0
 R48: 0.00852304918951343 xx1 + 0.00852304918951343 xx2
   + 0.00852304918951343 xx3 + 0.00852304918951343 xx4
   + 0.00852304918951343 xx5 + 0.00852304918951343 xx6
   + 0.00852304918951343 xx7 + 0.00852304918951343 xx8
   + 0.00852304918951343 xx9 + pow_base_aux_414 = 1
 qc0: 0.1025641025641026 qq9 - 0.1025641025641026 xx9
   + 0.1692307692307692 pow_aux_391 + 1.0256410256410256e-04 pow_aux_393
   + pow_base_aux_396 + [ 1.5384615384615385e-04 qq9 * pow_aux_395 ] = 1
 qc1: 0.0918367346938776 qq8 - 0.0918367346938776 xx8
   + 0.1530612244897959 pow_aux_399 + 1.0204081632653062e-04 pow_aux_401
   + pow_base_aux_404 + [ 1.5306122448979591e-04 qq8 * pow_aux_403 ] = 1
 qc2: 0.0812182741116751 qq7 - 0.0812182741116751 xx7
   + 0.0609137055837563 pow_aux_407 + 1.0152284263959391e-04 pow_aux_409
   + pow_base_aux_412 + [ 1.5228426395939087e-04 qq7 * pow_aux_411 ] = 1
Bounds
General Constraints
 GC0: pow_aux_331 = POW ( pow_base_aux_330 ^ 2 )
 GC1: pow_aux_333 = POW ( pow_base_aux_332 ^ 2 )
 GC2: pow_aux_335 = POW ( pow_base_aux_334 ^ 2 )
 GC3: pow_aux_337 = POW ( pow_base_aux_336 ^ 2 )
 GC4: pow_aux_339 = POW ( pow_base_aux_338 ^ 2 )
 GC5: pow_aux_341 = POW ( pow_base_aux_340 ^ 2 )
 GC6: pow_aux_343 = POW ( pow_base_aux_342 ^ 2 )
 GC7: pow_aux_345 = POW ( pow_base_aux_344 ^ 2 )
 GC8: pow_aux_347 = POW ( pow_base_aux_346 ^ 2 )
 GC9: pow_aux_349 = POW ( pow_base_aux_348 ^ 2 )
 GC10: pow_aux_351 = POW ( pow_base_aux_350 ^ 2 )
 GC11: pow_aux_353 = POW ( pow_base_aux_352 ^ 2 )
 GC12: pow_aux_355 = POW ( pow_base_aux_354 ^ 2 )
 GC13: pow_aux_357 = POW ( pow_base_aux_356 ^ 2 )
 GC14: pow_aux_359 = POW ( pow_base_aux_358 ^ 2 )
 GC15: pow_aux_361 = POW ( pow_base_aux_360 ^ 2 )
 GC16: pow_aux_363 = POW ( pow_base_aux_362 ^ 2 )
 GC17: pow_aux_365 = POW ( pow_base_aux_364 ^ 2 )
 GC18: pow_aux_367 = POW ( pow_base_aux_366 ^ 2 )
 GC19: pow_aux_369 = POW ( pow_base_aux_368 ^ 2 )
 GC20: pow_aux_371 = POW ( pow_base_aux_370 ^ 2 )
 GC21: pow_aux_373 = POW ( pow_base_aux_372 ^ 2 )
 GC22: pow_aux_375 = POW ( pow_base_aux_374 ^ 2 )
 GC23: pow_aux_377 = POW ( pow_base_aux_376 ^ 2 )
 GC24: pow_aux_379 = POW ( pow_base_aux_378 ^ 2 )
 GC25: pow_aux_381 = POW ( pow_base_aux_380 ^ 2 )
 GC26: pow_aux_383 = POW ( pow_base_aux_382 ^ 2 )
 GC27: pow_aux_385 = POW ( pow_base_aux_384 ^ 2 )
 GC28: pow_aux_387 = POW ( pow_base_aux_386 ^ 2 )
 GC29: pow_aux_389 = POW ( pow_base_aux_388 ^ 2 )
 GC30: pow_aux_391 = POW ( pow_base_aux_390 ^ 2 )
 GC31: pow_aux_393 = POW ( pow_base_aux_392 ^ 1.5 )
 GC32: pow_aux_395 = POW ( pow_base_aux_394 ^ 0.5 )
 GC33: pow_aux_397 = POW ( pow_base_aux_396 ^ 2 )
 GC34: pow_aux_399 = POW ( pow_base_aux_398 ^ 2 )
 GC35: pow_aux_401 = POW ( pow_base_aux_400 ^ 1.5 )
 GC36: pow_aux_403 = POW ( pow_base_aux_402 ^ 0.5 )
 GC37: pow_aux_405 = POW ( pow_base_aux_404 ^ 2 )
 GC38: pow_aux_407 = POW ( pow_base_aux_406 ^ 2 )
 GC39: pow_aux_409 = POW ( pow_base_aux_408 ^ 1.5 )
 GC40: pow_aux_411 = POW ( pow_base_aux_410 ^ 0.5 )
 GC41: pow_aux_413 = POW ( pow_base_aux_412 ^ 2 )
 GC42: pow_aux_415 = POW ( pow_base_aux_414 ^ 2 )
End
