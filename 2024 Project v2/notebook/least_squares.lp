\ Model Least Squares
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  pow_aux_10269 + pow_aux_10273 + pow_aux_10277 + pow_aux_10281
   + pow_aux_10285 + pow_aux_10289 + pow_aux_10293 + 100 pow_aux_10297
   + 121 pow_aux_10301 + 7921 pow_aux_10305 + 8100 pow_aux_10309
   + 9025 pow_aux_10313 + 20449 pow_aux_10317 + 21904 pow_aux_10321
   + 22201 pow_aux_10325 + 38025 pow_aux_10333 + 38416 pow_aux_10341
   + 38809 pow_aux_10349 + 2.4350803273583213e+06 pow_aux_10351
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
 R9: - qq5 + xx5 + pow_base_aux_10266 = 0
 R10: - 12 qq5 + 12 xx5 + ppp - 6 pow_aux_10267 + pow_base_aux_10268 = 1
 R11: - qq6 + xx6 + pow_base_aux_10270 = 0
 R12: - 14 qq6 + 14 xx6 + ppp - 9 pow_aux_10271 + pow_base_aux_10272 = 2
 R13: - qq7 + xx7 + pow_base_aux_10274 = 0
 R14: - 16 qq7 + 16 xx7 + ppp - 12 pow_aux_10275 + pow_base_aux_10276 = 3
 R15: - qq8 + xx8 + pow_base_aux_10278 = 0
 R16: - 18 qq8 + 18 xx8 + ppp - 30 pow_aux_10279 + pow_base_aux_10280 = 4
 R17: - qq9 + xx9 + pow_base_aux_10282 = 0
 R18: - 20 qq9 + 20 xx9 + ppp - 33 pow_aux_10283 + pow_base_aux_10284 = 5
 R19: - qq3 + xx3 + pow_base_aux_10286 = 0
 R20: - 8 qq3 + 8 xx3 + ppp - 15 pow_aux_10287 + pow_base_aux_10288 = 5
 R21: - qq4 + xx4 + pow_base_aux_10290 = 0
 R22: - 10 qq4 + 10 xx4 + ppp - 9 pow_aux_10291 + pow_base_aux_10292 = 7
 R23: - qq1 + xx1 + pow_base_aux_10294 = 0
 R24: - 0.4 qq1 + 0.4 xx1 + 0.1 ppp - 0.03 pow_aux_10295
   + pow_base_aux_10296 = 1
 R25: - qq2 + xx2 + pow_base_aux_10298 = 0
 R26: - 0.5454545454545454 qq2 + 0.5454545454545454 xx2
   + 0.0909090909090909 ppp - 0.0545454545454546 pow_aux_10299
   + pow_base_aux_10300 = 1
 R27: - qq2 + xx2 + pow_base_aux_10302 = 0
 R28: 0.00112359550561798 qq1 + 0.0696629213483146 qq2
   - 0.0674157303370786 xx2 + 0.00112359550561798 qq3
   - 0.00896606514584043 ppp + 0.00674157303370787 pow_aux_10303
   + pow_base_aux_10304 = 1
 R29: - qq1 + xx1 + pow_base_aux_10306 = 0
 R30: 0.0466666666666667 qq1 - 0.0444444444444444 xx1
   + 0.00111111111111111 qq2 + 0.00111111111111111 qq3
   - 0.00886644219977553 ppp + 0.00333333333333333 pow_aux_10307
   + pow_base_aux_10308 = 1
 R31: - qq3 + xx3 + pow_base_aux_10310 = 0
 R32: 0.00105263157894737 qq1 + 0.00105263157894737 qq2
   + 0.0863157894736842 qq3 - 0.0842105263157895 xx3
   - 0.00839978734715577 ppp + 0.1578947368421053 pow_aux_10311
   + pow_base_aux_10312 = 1
 R33: - qq4 + xx4 + pow_base_aux_10314 = 0
 R34: 0.0713286713286713 qq4 - 0.0699300699300699 xx4
   + 6.993006993006993e-04 qq5 + 6.993006993006993e-04 qq6
   + 0.0629370629370629 pow_aux_10315 + pow_base_aux_10316 = 1
 R35: - qq6 + xx6 + pow_base_aux_10318 = 0
 R36: 6.7567567567567571e-04 qq4 + 6.7567567567567571e-04 qq5
   + 0.095945945945946 qq6 - 0.0945945945945946 xx6
   + 0.0608108108108108 pow_aux_10319 + pow_base_aux_10320 = 1
 R37: - qq5 + xx5 + pow_base_aux_10322 = 0
 R38: 0.000671141 qq4 + 0.0818791946308725 qq5 - 0.0805369127516778 xx5
   + 0.000671141 qq6 + 0.0402684563758389 pow_aux_10323
   + pow_base_aux_10324 = 1
 R39: - qq9 + xx9 + pow_base_aux_10326 = 0
 R40: - qq7 - qq8 - qq9 + pow_base_aux_10328 = 0
 R41: - qq7 - qq8 - qq9 + pow_base_aux_10330 = 0
 R42: - qq8 + xx8 + pow_base_aux_10334 = 0
 R43: - qq7 - qq8 - qq9 + pow_base_aux_10336 = 0
 R44: - qq7 - qq8 - qq9 + pow_base_aux_10338 = 0
 R45: - qq7 + xx7 + pow_base_aux_10342 = 0
 R46: - qq7 - qq8 - qq9 + pow_base_aux_10344 = 0
 R47: - qq7 - qq8 - qq9 + pow_base_aux_10346 = 0
 R48: 6.4083076612882932e-04 xx1 + 6.4083076612882932e-04 xx2
   + 6.4083076612882932e-04 xx3 + 6.4083076612882932e-04 xx4
   + 6.4083076612882932e-04 xx5 + 6.4083076612882932e-04 xx6
   + 6.4083076612882932e-04 xx7 + 6.4083076612882932e-04 xx8
   + 6.4083076612882932e-04 xx9 + pow_base_aux_10350 = 1
 qc0: 0.1025641025641026 qq9 - 0.1025641025641026 xx9
   + 0.1692307692307692 pow_aux_10327
   + 1.0256410256410256e-04 pow_aux_10329 + pow_base_aux_10332 + [
   1.5384615384615385e-04 qq9 * pow_aux_10331 ] = 1
 qc1: 0.0918367346938776 qq8 - 0.0918367346938776 xx8
   + 0.1530612244897959 pow_aux_10335
   + 1.0204081632653062e-04 pow_aux_10337 + pow_base_aux_10340 + [
   1.5306122448979591e-04 qq8 * pow_aux_10339 ] = 1
 qc2: 0.0812182741116751 qq7 - 0.0812182741116751 xx7
   + 0.0609137055837563 pow_aux_10343
   + 1.0152284263959391e-04 pow_aux_10345 + pow_base_aux_10348 + [
   1.5228426395939087e-04 qq7 * pow_aux_10347 ] = 1
Bounds
General Constraints
 GC0: pow_aux_10267 = POW ( pow_base_aux_10266 ^ 2 )
 GC1: pow_aux_10269 = POW ( pow_base_aux_10268 ^ 2 )
 GC2: pow_aux_10271 = POW ( pow_base_aux_10270 ^ 2 )
 GC3: pow_aux_10273 = POW ( pow_base_aux_10272 ^ 2 )
 GC4: pow_aux_10275 = POW ( pow_base_aux_10274 ^ 2 )
 GC5: pow_aux_10277 = POW ( pow_base_aux_10276 ^ 2 )
 GC6: pow_aux_10279 = POW ( pow_base_aux_10278 ^ 2 )
 GC7: pow_aux_10281 = POW ( pow_base_aux_10280 ^ 2 )
 GC8: pow_aux_10283 = POW ( pow_base_aux_10282 ^ 2 )
 GC9: pow_aux_10285 = POW ( pow_base_aux_10284 ^ 2 )
 GC10: pow_aux_10287 = POW ( pow_base_aux_10286 ^ 2 )
 GC11: pow_aux_10289 = POW ( pow_base_aux_10288 ^ 2 )
 GC12: pow_aux_10291 = POW ( pow_base_aux_10290 ^ 2 )
 GC13: pow_aux_10293 = POW ( pow_base_aux_10292 ^ 2 )
 GC14: pow_aux_10295 = POW ( pow_base_aux_10294 ^ 2 )
 GC15: pow_aux_10297 = POW ( pow_base_aux_10296 ^ 2 )
 GC16: pow_aux_10299 = POW ( pow_base_aux_10298 ^ 2 )
 GC17: pow_aux_10301 = POW ( pow_base_aux_10300 ^ 2 )
 GC18: pow_aux_10303 = POW ( pow_base_aux_10302 ^ 2 )
 GC19: pow_aux_10305 = POW ( pow_base_aux_10304 ^ 2 )
 GC20: pow_aux_10307 = POW ( pow_base_aux_10306 ^ 2 )
 GC21: pow_aux_10309 = POW ( pow_base_aux_10308 ^ 2 )
 GC22: pow_aux_10311 = POW ( pow_base_aux_10310 ^ 2 )
 GC23: pow_aux_10313 = POW ( pow_base_aux_10312 ^ 2 )
 GC24: pow_aux_10315 = POW ( pow_base_aux_10314 ^ 2 )
 GC25: pow_aux_10317 = POW ( pow_base_aux_10316 ^ 2 )
 GC26: pow_aux_10319 = POW ( pow_base_aux_10318 ^ 2 )
 GC27: pow_aux_10321 = POW ( pow_base_aux_10320 ^ 2 )
 GC28: pow_aux_10323 = POW ( pow_base_aux_10322 ^ 2 )
 GC29: pow_aux_10325 = POW ( pow_base_aux_10324 ^ 2 )
 GC30: pow_aux_10327 = POW ( pow_base_aux_10326 ^ 2 )
 GC31: pow_aux_10329 = POW ( pow_base_aux_10328 ^ 1.5 )
 GC32: pow_aux_10331 = POW ( pow_base_aux_10330 ^ 0.5 )
 GC33: pow_aux_10333 = POW ( pow_base_aux_10332 ^ 2 )
 GC34: pow_aux_10335 = POW ( pow_base_aux_10334 ^ 2 )
 GC35: pow_aux_10337 = POW ( pow_base_aux_10336 ^ 1.5 )
 GC36: pow_aux_10339 = POW ( pow_base_aux_10338 ^ 0.5 )
 GC37: pow_aux_10341 = POW ( pow_base_aux_10340 ^ 2 )
 GC38: pow_aux_10343 = POW ( pow_base_aux_10342 ^ 2 )
 GC39: pow_aux_10345 = POW ( pow_base_aux_10344 ^ 1.5 )
 GC40: pow_aux_10347 = POW ( pow_base_aux_10346 ^ 0.5 )
 GC41: pow_aux_10349 = POW ( pow_base_aux_10348 ^ 2 )
 GC42: pow_aux_10351 = POW ( pow_base_aux_10350 ^ 2 )
End
