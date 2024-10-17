\ Model Least Squares e
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  pow_aux_211 + pow_aux_215 + pow_aux_219 + pow_aux_223 + pow_aux_227
   + pow_aux_231 + pow_aux_235 + 100 pow_aux_239 + 121 pow_aux_243
   + 10000 pow_aux_245 + 10000 pow_aux_247 + 10000 pow_aux_249
   + 22500 pow_aux_251 + 22500 pow_aux_253 + 22500 pow_aux_255
   + 40000 pow_aux_261 + 40000 pow_aux_267 + 40000 pow_aux_273
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
 R19: - ab5 + pow_base_aux_208 = 0
 R20: 12 ab5 - ppp + 6 pow_aux_209 + pow_base_aux_210 = -1
 R21: - ab4 + pow_base_aux_212 = 0
 R22: 10 ab4 - ppp + 9 pow_aux_213 + pow_base_aux_214 = -7
 R23: - ab9 + pow_base_aux_216 = 0
 R24: 20 ab9 - ppp + 33 pow_aux_217 + pow_base_aux_218 = -5
 R25: - ab3 + pow_base_aux_220 = 0
 R26: 8 ab3 - ppp + 15 pow_aux_221 + pow_base_aux_222 = -5
 R27: - ab8 + pow_base_aux_224 = 0
 R28: 18 ab8 - ppp + 30 pow_aux_225 + pow_base_aux_226 = -4
 R29: - ab7 + pow_base_aux_228 = 0
 R30: 16 ab7 - ppp + 12 pow_aux_229 + pow_base_aux_230 = -3
 R31: - ab6 + pow_base_aux_232 = 0
 R32: 14 ab6 - ppp + 9 pow_aux_233 + pow_base_aux_234 = -2
 R33: - ab1 + pow_base_aux_236 = 0
 R34: 0.4 ab1 - 0.1 ppp + 0.03 pow_aux_237 + pow_base_aux_238 = -1
 R35: - ab2 + pow_base_aux_240 = 0
 R36: 0.5454545454545454 ab2 - 0.0909090909090909 ppp
   + 0.0545454545454545 pow_aux_241 + pow_base_aux_242 = -1
 R37: 0.001 qq1 + 0.001 qq2 + 0.002 qq3 + 0.01 ppp + pow_base_aux_244 = 1
 R38: 0.001 qq1 + 0.002 qq2 + 0.001 qq3 + 0.01 ppp + pow_base_aux_246 = 1
 R39: 0.002 qq1 + 0.001 qq2 + 0.001 qq3 + 0.01 ppp + pow_base_aux_248 = 1
 R40: 6.6666666666666664e-04 qq4 + 6.6666666666666664e-04 qq5
   + 0.00133333333333333 qq6 + 0.00666666666666667 ppp + pow_base_aux_250
   = 1
 R41: 6.6666666666666664e-04 qq4 + 0.00133333333333333 qq5
   + 6.6666666666666664e-04 qq6 + 0.00666666666666667 ppp
   + pow_base_aux_252 = 1
 R42: 0.00133333333333333 qq4 + 6.6666666666666664e-04 qq5
   + 6.6666666666666664e-04 qq6 + 0.00666666666666667 ppp
   + pow_base_aux_254 = 1
 R43: - qq7 - qq8 - qq9 + pow_base_aux_256 = 0
 R44: - qq7 - qq8 - qq9 + pow_base_aux_258 = 0
 R45: - qq7 - qq8 - qq9 + pow_base_aux_262 = 0
 R46: - qq7 - qq8 - qq9 + pow_base_aux_264 = 0
 R47: - qq7 - qq8 - qq9 + pow_base_aux_268 = 0
 R48: - qq7 - qq8 - qq9 + pow_base_aux_270 = 0
 qc0: 0.005 ppp + 0.0001 pow_aux_257 + pow_base_aux_260 + [
   0.00015 qq7 * pow_aux_259 ] = 1
 qc1: 0.005 ppp + 0.0001 pow_aux_263 + pow_base_aux_266 + [
   0.00015 qq8 * pow_aux_265 ] = 1
 qc2: 0.005 ppp + 0.0001 pow_aux_269 + pow_base_aux_272 + [
   0.00015 qq9 * pow_aux_271 ] = 1
Bounds
General Constraints
 GC0: pow_aux_209 = POW ( pow_base_aux_208 ^ 2 )
 GC1: pow_aux_211 = POW ( pow_base_aux_210 ^ 2 )
 GC2: pow_aux_213 = POW ( pow_base_aux_212 ^ 2 )
 GC3: pow_aux_215 = POW ( pow_base_aux_214 ^ 2 )
 GC4: pow_aux_217 = POW ( pow_base_aux_216 ^ 2 )
 GC5: pow_aux_219 = POW ( pow_base_aux_218 ^ 2 )
 GC6: pow_aux_221 = POW ( pow_base_aux_220 ^ 2 )
 GC7: pow_aux_223 = POW ( pow_base_aux_222 ^ 2 )
 GC8: pow_aux_225 = POW ( pow_base_aux_224 ^ 2 )
 GC9: pow_aux_227 = POW ( pow_base_aux_226 ^ 2 )
 GC10: pow_aux_229 = POW ( pow_base_aux_228 ^ 2 )
 GC11: pow_aux_231 = POW ( pow_base_aux_230 ^ 2 )
 GC12: pow_aux_233 = POW ( pow_base_aux_232 ^ 2 )
 GC13: pow_aux_235 = POW ( pow_base_aux_234 ^ 2 )
 GC14: pow_aux_237 = POW ( pow_base_aux_236 ^ 2 )
 GC15: pow_aux_239 = POW ( pow_base_aux_238 ^ 2 )
 GC16: pow_aux_241 = POW ( pow_base_aux_240 ^ 2 )
 GC17: pow_aux_243 = POW ( pow_base_aux_242 ^ 2 )
 GC18: pow_aux_245 = POW ( pow_base_aux_244 ^ 2 )
 GC19: pow_aux_247 = POW ( pow_base_aux_246 ^ 2 )
 GC20: pow_aux_249 = POW ( pow_base_aux_248 ^ 2 )
 GC21: pow_aux_251 = POW ( pow_base_aux_250 ^ 2 )
 GC22: pow_aux_253 = POW ( pow_base_aux_252 ^ 2 )
 GC23: pow_aux_255 = POW ( pow_base_aux_254 ^ 2 )
 GC24: pow_aux_257 = POW ( pow_base_aux_256 ^ 1.5 )
 GC25: pow_aux_259 = POW ( pow_base_aux_258 ^ 0.5 )
 GC26: pow_aux_261 = POW ( pow_base_aux_260 ^ 2 )
 GC27: pow_aux_263 = POW ( pow_base_aux_262 ^ 1.5 )
 GC28: pow_aux_265 = POW ( pow_base_aux_264 ^ 0.5 )
 GC29: pow_aux_267 = POW ( pow_base_aux_266 ^ 2 )
 GC30: pow_aux_269 = POW ( pow_base_aux_268 ^ 1.5 )
 GC31: pow_aux_271 = POW ( pow_base_aux_270 ^ 0.5 )
 GC32: pow_aux_273 = POW ( pow_base_aux_272 ^ 2 )
End
