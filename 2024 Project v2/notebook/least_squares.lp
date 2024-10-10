\ Model Least Squares
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  pow_aux_5 + pow_aux_7 + 900 pow_aux_13 + 1600 pow_aux_19 + 1600 pow_aux_25
   + 3600 pow_aux_31 + 6400 pow_aux_37 + 6400 pow_aux_43 + 10000 pow_aux_49
   + 10000 pow_aux_53 + 10000 pow_aux_57 + 10000 pow_aux_61
   + 10000 pow_aux_65 + 10000 pow_aux_69 + 10000 pow_aux_73
   + 10000 pow_aux_77 + 19600 pow_aux_83 + 22500 pow_aux_87
   + 22500 pow_aux_91 + 22500 pow_aux_95 + 22500 pow_aux_99
   + 22500 pow_aux_103 + 22500 pow_aux_107 + 22500 pow_aux_111
   + 25600 pow_aux_117 + 32400 pow_aux_123 + 40000 pow_aux_129
   + 40000 pow_aux_135 + 40000 pow_aux_143 + 40000 pow_aux_151
   + 40000 pow_aux_159 + 40000 pow_aux_167 + 40000 pow_aux_175
   + 40000 pow_aux_183 + 40000 pow_aux_191 + 48400 pow_aux_197
   + 57600 pow_aux_203 + 67600 pow_aux_209 + 78400 pow_aux_215
   + 90000 pow_aux_221 + 90000 pow_aux_225 + 90000 pow_aux_229
   + 90000 pow_aux_233 + 102400 pow_aux_239 + 115600 pow_aux_245
   + 129600 pow_aux_251 + 144400 pow_aux_257 + 160000 pow_aux_263
   + 160000 pow_aux_267 + 160000 pow_aux_271 + 160000 pow_aux_275
   + 160000 pow_aux_279 + 160000 pow_aux_283 + 160000 pow_aux_287
   + 176400 pow_aux_293 + 193600 pow_aux_299 + 211600 pow_aux_305
   + 230400 pow_aux_311 + 250000 pow_aux_317 + 270400 pow_aux_323
   + 291600 pow_aux_329
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
 R9: qq10 - xx10 >= 0
 R10: qq11 - xx11 >= 0
 R11: qq12 - xx12 >= 0
 R12: qq13 - xx13 >= 0
 R13: qq14 - xx14 >= 0
 R14: qq15 - xx15 >= 0
 R15: qq16 - xx16 >= 0
 R16: qq17 - xx17 >= 0
 R17: qq18 - xx18 >= 0
 R18: qq19 - xx19 >= 0
 R19: qq20 - xx20 >= 0
 R20: qq21 - xx21 >= 0
 R21: qq22 - xx22 >= 0
 R22: qq23 - xx23 >= 0
 R23: qq24 - xx24 >= 0
 R24: qq25 - xx25 >= 0
 R25: qq26 - xx26 >= 0
 R26: qq27 - xx27 >= 0
 R27: qq28 - xx28 >= 0
 R28: qq29 - xx29 >= 0
 R29: qq30 - xx30 >= 0
 R30: - qq9 + xx9 + pow_base_aux_0 = 0
 R31: - xx9 + pow_base_aux_2 = 0
 R32: ppp - 5 pow_aux_1 - 120 pow_aux_3 + pow_base_aux_4 = 0
 R33: xx1 + xx2 + xx3 + xx4 + xx5 + xx6 + xx7 + xx8 + xx9 + xx10 + xx11
   + xx12 + xx13 + xx14 + xx15 + xx16 + xx17 + xx18 + xx19 + xx20 + xx21
   + xx22 + xx23 + xx24 + xx25 + xx26 + xx27 + xx28 + xx29 + xx30
   + pow_base_aux_6 = 2000
 R34: - xx6 + pow_base_aux_8 = 0
 R35: - qq6 + xx6 + pow_base_aux_10 = 0
 R36: 0.0333333333333333 ppp - pow_aux_9 - 0.1066666666666667 pow_aux_11
   + pow_base_aux_12 = 0
 R37: - xx4 + pow_base_aux_14 = 0
 R38: - qq4 + xx4 + pow_base_aux_16 = 0
 R39: 0.025 ppp - pow_aux_15 - 0.0825 pow_aux_17 + pow_base_aux_18 = 0
 R40: - xx5 + pow_base_aux_20 = 0
 R41: - qq5 + xx5 + pow_base_aux_22 = 0
 R42: 0.025 ppp - pow_aux_21 - 0.0575 pow_aux_23 + pow_base_aux_24 = 0
 R43: - xx3 + pow_base_aux_26 = 0
 R44: - qq3 + xx3 + pow_base_aux_28 = 0
 R45: 0.0166666666666667 ppp - pow_aux_27 - 0.0866666666666667 pow_aux_29
   + pow_base_aux_30 = 0
 R46: - xx2 + pow_base_aux_32 = 0
 R47: - qq2 + xx2 + pow_base_aux_34 = 0
 R48: 0.0125 ppp - pow_aux_33 - 0.2 pow_aux_35 + pow_base_aux_36 = 0
 R49: - xx7 + pow_base_aux_38 = 0
 R50: - qq7 + xx7 + pow_base_aux_40 = 0
 R51: 0.0125 ppp - pow_aux_39 - 0.05125 pow_aux_41 + pow_base_aux_42 = 0
 R52: - xx8 + pow_base_aux_44 = 0
 R53: - qq8 + xx8 + pow_base_aux_46 = 0
 R54: 0.01 ppp - pow_aux_45 - 0.051 pow_aux_47 + pow_base_aux_48 = 0
 R55: - qq16 + xx16 + pow_base_aux_50 = 0
 R56: 0.001 qq1 + 0.001 qq2 + 0.001 qq3 + 0.002 qq16 + 0.001 qq17
   + 0.001 qq18 + 0.001 qq19 + 0.031 pow_aux_51 + pow_base_aux_52 = 1
 R57: - qq3 + xx3 + pow_base_aux_54 = 0
 R58: 0.001 qq1 + 0.001 qq2 + 0.002 qq3 + 0.001 qq16 + 0.001 qq17
   + 0.001 qq18 + 0.001 qq19 + 0.052 pow_aux_55 + pow_base_aux_56 = 1
 R59: - qq2 + xx2 + pow_base_aux_58 = 0
 R60: 0.001 qq1 + 0.002 qq2 + 0.001 qq3 + 0.001 qq16 + 0.001 qq17
   + 0.001 qq18 + 0.001 qq19 + 0.16 pow_aux_59 + pow_base_aux_60 = 1
 R61: - qq19 + xx19 + pow_base_aux_62 = 0
 R62: 0.001 qq1 + 0.001 qq2 + 0.001 qq3 + 0.001 qq16 + 0.001 qq17
   + 0.001 qq18 + 0.002 qq19 + 0.051 pow_aux_63 + pow_base_aux_64 = 1
 R63: - qq18 + xx18 + pow_base_aux_66 = 0
 R64: 0.001 qq1 + 0.001 qq2 + 0.001 qq3 + 0.001 qq16 + 0.001 qq17
   + 0.002 qq18 + 0.001 qq19 + 0.051 pow_aux_67 + pow_base_aux_68 = 1
 R65: - qq17 + xx17 + pow_base_aux_70 = 0
 R66: 0.001 qq1 + 0.001 qq2 + 0.001 qq3 + 0.001 qq16 + 0.002 qq17
   + 0.001 qq18 + 0.001 qq19 + 0.041 pow_aux_71 + pow_base_aux_72 = 1
 R67: - qq1 + xx1 + pow_base_aux_74 = 0
 R68: 0.002 qq1 + 0.001 qq2 + 0.001 qq3 + 0.001 qq16 + 0.001 qq17
   + 0.001 qq18 + 0.001 qq19 + 0.003 pow_aux_75 + pow_base_aux_76 = 1
 R69: - xx10 + pow_base_aux_78 = 0
 R70: - qq10 + xx10 + pow_base_aux_80 = 0
 R71: 0.00714285714285714 ppp - pow_aux_79 - 0.03 pow_aux_81
   + pow_base_aux_82 = 0
 R72: - qq21 + xx21 + pow_base_aux_84 = 0
 R73: 6.6666666666666664e-04 qq4 + 6.6666666666666664e-04 qq5
   + 6.6666666666666664e-04 qq6 + 6.6666666666666664e-04 qq20
   + 0.00133333333333333 qq21 + 6.6666666666666664e-04 qq22
   + 6.6666666666666664e-04 qq23 + 0.0206666666666667 pow_aux_85
   + pow_base_aux_86 = 1
 R74: - qq23 + xx23 + pow_base_aux_88 = 0
 R75: 6.6666666666666664e-04 qq4 + 6.6666666666666664e-04 qq5
   + 6.6666666666666664e-04 qq6 + 6.6666666666666664e-04 qq20
   + 6.6666666666666664e-04 qq21 + 6.6666666666666664e-04 qq22
   + 0.00133333333333333 qq23 + 0.0206666666666667 pow_aux_89
   + pow_base_aux_90 = 1
 R76: - qq6 + xx6 + pow_base_aux_92 = 0
 R77: 6.6666666666666664e-04 qq4 + 6.6666666666666664e-04 qq5
   + 0.00133333333333333 qq6 + 6.6666666666666664e-04 qq20
   + 6.6666666666666664e-04 qq21 + 6.6666666666666664e-04 qq22
   + 6.6666666666666664e-04 qq23 + 0.0213333333333333 pow_aux_93
   + pow_base_aux_94 = 1
 R78: - qq5 + xx5 + pow_base_aux_96 = 0
 R79: 6.6666666666666664e-04 qq4 + 0.00133333333333333 qq5
   + 6.6666666666666664e-04 qq6 + 6.6666666666666664e-04 qq20
   + 6.6666666666666664e-04 qq21 + 6.6666666666666664e-04 qq22
   + 6.6666666666666664e-04 qq23 + 0.0153333333333333 pow_aux_97
   + pow_base_aux_98 = 1
 R80: - qq4 + xx4 + pow_base_aux_100 = 0
 R81: 0.00133333333333333 qq4 + 6.6666666666666664e-04 qq5
   + 6.6666666666666664e-04 qq6 + 6.6666666666666664e-04 qq20
   + 6.6666666666666664e-04 qq21 + 6.6666666666666664e-04 qq22
   + 6.6666666666666664e-04 qq23 + 0.022 pow_aux_101 + pow_base_aux_102
   = 1
 R82: - qq22 + xx22 + pow_base_aux_104 = 0
 R83: 6.6666666666666664e-04 qq4 + 6.6666666666666664e-04 qq5
   + 6.6666666666666664e-04 qq6 + 6.6666666666666664e-04 qq20
   + 6.6666666666666664e-04 qq21 + 0.00133333333333333 qq22
   + 6.6666666666666664e-04 qq23 + 0.014 pow_aux_105 + pow_base_aux_106
   = 1
 R84: - qq20 + xx20 + pow_base_aux_108 = 0
 R85: 6.6666666666666664e-04 qq4 + 6.6666666666666664e-04 qq5
   + 6.6666666666666664e-04 qq6 + 0.00133333333333333 qq20
   + 6.6666666666666664e-04 qq21 + 6.6666666666666664e-04 qq22
   + 6.6666666666666664e-04 qq23 + 0.0273333333333333 pow_aux_109
   + pow_base_aux_110 = 1
 R86: - xx11 + pow_base_aux_112 = 0
 R87: - qq11 + xx11 + pow_base_aux_114 = 0
 R88: 0.00625 ppp - pow_aux_113 - 0.019375 pow_aux_115 + pow_base_aux_116
   = 0
 R89: - xx12 + pow_base_aux_118 = 0
 R90: - qq12 + xx12 + pow_base_aux_120 = 0
 R91: 0.00555555555555556 ppp - pow_aux_119
   - 0.0116666666666667 pow_aux_121 + pow_base_aux_122 = 0
 R92: - xx1 + pow_base_aux_124 = 0
 R93: - qq1 + xx1 + pow_base_aux_126 = 0
 R94: 0.005 ppp - pow_aux_125 - 0.0015 pow_aux_127 + pow_base_aux_128 = 0
 R95: - xx13 + pow_base_aux_130 = 0
 R96: - qq13 + xx13 + pow_base_aux_132 = 0
 R97: 0.005 ppp - pow_aux_131 - 0.0105 pow_aux_133 + pow_base_aux_134 = 0
 R98: - qq9 + xx9 + pow_base_aux_136 = 0
 R99: - qq7 - qq8 - qq9 - qq24 - qq25 - qq26 - qq27 + pow_base_aux_138 = 0
 R100: - qq7 - qq8 - qq9 - qq24 - qq25 - qq26 - qq27 + pow_base_aux_140
   = 0
 R101: - qq27 + xx27 + pow_base_aux_144 = 0
 R102: - qq7 - qq8 - qq9 - qq24 - qq25 - qq26 - qq27 + pow_base_aux_146
   = 0
 R103: - qq7 - qq8 - qq9 - qq24 - qq25 - qq26 - qq27 + pow_base_aux_148
   = 0
 R104: - qq24 + xx24 + pow_base_aux_152 = 0
 R105: - qq7 - qq8 - qq9 - qq24 - qq25 - qq26 - qq27 + pow_base_aux_154
   = 0
 R106: - qq7 - qq8 - qq9 - qq24 - qq25 - qq26 - qq27 + pow_base_aux_156
   = 0
 R107: - qq26 + xx26 + pow_base_aux_160 = 0
 R108: - qq7 - qq8 - qq9 - qq24 - qq25 - qq26 - qq27 + pow_base_aux_162
   = 0
 R109: - qq7 - qq8 - qq9 - qq24 - qq25 - qq26 - qq27 + pow_base_aux_164
   = 0
 R110: - qq7 + xx7 + pow_base_aux_168 = 0
 R111: - qq7 - qq8 - qq9 - qq24 - qq25 - qq26 - qq27 + pow_base_aux_170
   = 0
 R112: - qq7 - qq8 - qq9 - qq24 - qq25 - qq26 - qq27 + pow_base_aux_172
   = 0
 R113: - qq25 + xx25 + pow_base_aux_176 = 0
 R114: - qq7 - qq8 - qq9 - qq24 - qq25 - qq26 - qq27 + pow_base_aux_178
   = 0
 R115: - qq7 - qq8 - qq9 - qq24 - qq25 - qq26 - qq27 + pow_base_aux_180
   = 0
 R116: - qq8 + xx8 + pow_base_aux_184 = 0
 R117: - qq7 - qq8 - qq9 - qq24 - qq25 - qq26 - qq27 + pow_base_aux_186
   = 0
 R118: - qq7 - qq8 - qq9 - qq24 - qq25 - qq26 - qq27 + pow_base_aux_188
   = 0
 R119: - xx14 + pow_base_aux_192 = 0
 R120: - qq14 + xx14 + pow_base_aux_194 = 0
 R121: 0.00454545454545455 ppp - pow_aux_193
   - 0.0140909090909091 pow_aux_195 + pow_base_aux_196 = 0
 R122: - xx15 + pow_base_aux_198 = 0
 R123: - qq15 + xx15 + pow_base_aux_200 = 0
 R124: 0.00416666666666667 ppp - pow_aux_199 - 0.00875 pow_aux_201
   + pow_base_aux_202 = 0
 R125: - xx16 + pow_base_aux_204 = 0
 R126: - qq16 + xx16 + pow_base_aux_206 = 0
 R127: 0.00384615384615385 ppp - pow_aux_205
   - 0.0119230769230769 pow_aux_207 + pow_base_aux_208 = 0
 R128: - xx17 + pow_base_aux_210 = 0
 R129: - qq17 + xx17 + pow_base_aux_212 = 0
 R130: 0.00357142857142857 ppp - pow_aux_211
   - 0.0146428571428571 pow_aux_213 + pow_base_aux_214 = 0
 R131: - xx18 + pow_base_aux_216 = 0
 R132: - qq18 + xx18 + pow_base_aux_218 = 0
 R133: 0.00333333333333333 ppp - pow_aux_217 - 0.017 pow_aux_219
   + pow_base_aux_220 = 0
 R134: - qq14 + xx14 + pow_base_aux_222 = 0
 R135: 0.00133333333333333 qq13 + 0.00266666666666667 qq14
   + 0.00133333333333333 qq15 + 0.0103333333333333 pow_aux_223
   + pow_base_aux_224 = 1
 R136: - qq15 + xx15 + pow_base_aux_226 = 0
 R137: 0.00133333333333333 qq13 + 0.00133333333333333 qq14
   + 0.00266666666666667 qq15 + 0.007 pow_aux_227 + pow_base_aux_228 = 1
 R138: - qq13 + xx13 + pow_base_aux_230 = 0
 R139: 0.00266666666666667 qq13 + 0.00133333333333333 qq14
   + 0.00133333333333333 qq15 + 0.007 pow_aux_231 + pow_base_aux_232 = 1
 R140: - xx19 + pow_base_aux_234 = 0
 R141: - qq19 + xx19 + pow_base_aux_236 = 0
 R142: 0.003125 ppp - pow_aux_235 - 0.0159375 pow_aux_237
   + pow_base_aux_238 = 0
 R143: - xx20 + pow_base_aux_240 = 0
 R144: - qq20 + xx20 + pow_base_aux_242 = 0
 R145: 0.00294117647058824 ppp - pow_aux_241
   - 0.0120588235294118 pow_aux_243 + pow_base_aux_244 = 0
 R146: - xx21 + pow_base_aux_246 = 0
 R147: - qq21 + xx21 + pow_base_aux_248 = 0
 R148: 0.00277777777777778 ppp - pow_aux_247
   - 0.00861111111111111 pow_aux_249 + pow_base_aux_250 = 0
 R149: - xx22 + pow_base_aux_252 = 0
 R150: - qq22 + xx22 + pow_base_aux_254 = 0
 R151: 0.00263157894736842 ppp - pow_aux_253
   - 0.00552631578947368 pow_aux_255 + pow_base_aux_256 = 0
 R152: - xx23 + pow_base_aux_258 = 0
 R153: - qq23 + xx23 + pow_base_aux_260 = 0
 R154: 0.0025 ppp - pow_aux_259 - 0.00775 pow_aux_261 + pow_base_aux_262
   = 0
 R155: - qq11 + xx11 + pow_base_aux_264 = 0
 R156: 0.00125 qq10 + 0.0025 qq11 + 0.00125 qq12 + 0.00125 qq28
   + 0.00125 qq29 + 0.00125 qq30 + 0.00775 pow_aux_265 + pow_base_aux_266
   = 1
 R157: - qq29 + xx29 + pow_base_aux_268 = 0
 R158: 0.00125 qq10 + 0.00125 qq11 + 0.00125 qq12 + 0.00125 qq28
   + 0.0025 qq29 + 0.00125 qq30 + 0.00775 pow_aux_269 + pow_base_aux_270
   = 1
 R159: - qq30 + xx30 + pow_base_aux_272 = 0
 R160: 0.00125 qq10 + 0.00125 qq11 + 0.00125 qq12 + 0.00125 qq28
   + 0.00125 qq29 + 0.0025 qq30 + 0.01025 pow_aux_273 + pow_base_aux_274
   = 1
 R161: - qq28 + xx28 + pow_base_aux_276 = 0
 R162: 0.00125 qq10 + 0.00125 qq11 + 0.00125 qq12 + 0.0025 qq28
   + 0.00125 qq29 + 0.00125 qq30 + 0.00525 pow_aux_277 + pow_base_aux_278
   = 1
 R163: - qq12 + xx12 + pow_base_aux_280 = 0
 R164: 0.00125 qq10 + 0.00125 qq11 + 0.0025 qq12 + 0.00125 qq28
   + 0.00125 qq29 + 0.00125 qq30 + 0.00525 pow_aux_281 + pow_base_aux_282
   = 1
 R165: - qq10 + xx10 + pow_base_aux_284 = 0
 R166: 0.0025 qq10 + 0.00125 qq11 + 0.00125 qq12 + 0.00125 qq28
   + 0.00125 qq29 + 0.00125 qq30 + 0.0105 pow_aux_285 + pow_base_aux_286
   = 1
 R167: - xx24 + pow_base_aux_288 = 0
 R168: - qq24 + xx24 + pow_base_aux_290 = 0
 R169: 0.00238095238095238 ppp - pow_aux_289
   - 0.00976190476190476 pow_aux_291 + pow_base_aux_292 = 0
 R170: - xx25 + pow_base_aux_294 = 0
 R171: - qq25 + xx25 + pow_base_aux_296 = 0
 R172: 0.00227272727272727 ppp - pow_aux_295
   - 0.0115909090909091 pow_aux_297 + pow_base_aux_298 = 0
 R173: - xx26 + pow_base_aux_300 = 0
 R174: - qq26 + xx26 + pow_base_aux_302 = 0
 R175: 0.00217391304347826 ppp - pow_aux_301
   - 0.00891304347826087 pow_aux_303 + pow_base_aux_304 = 0
 R176: - xx27 + pow_base_aux_306 = 0
 R177: - qq27 + xx27 + pow_base_aux_308 = 0
 R178: 0.00208333333333333 ppp - pow_aux_307
   - 0.00645833333333333 pow_aux_309 + pow_base_aux_310 = 0
 R179: - xx28 + pow_base_aux_312 = 0
 R180: - qq28 + xx28 + pow_base_aux_314 = 0
 R181: 0.002 ppp - pow_aux_313 - 0.0042 pow_aux_315 + pow_base_aux_316 = 0
 R182: - xx29 + pow_base_aux_318 = 0
 R183: - qq29 + xx29 + pow_base_aux_320 = 0
 R184: 0.00192307692307692 ppp - pow_aux_319
   - 0.00596153846153846 pow_aux_321 + pow_base_aux_322 = 0
 R185: - xx30 + pow_base_aux_324 = 0
 R186: - qq30 + xx30 + pow_base_aux_326 = 0
 R187: 0.00185185185185185 ppp - pow_aux_325
   - 0.00759259259259259 pow_aux_327 + pow_base_aux_328 = 0
 qc0: 0.025 pow_aux_137 + 0.0001 pow_aux_139 + pow_base_aux_142 + [
   0.00015 qq9 * pow_aux_141 ] = 1
 qc1: 0.0155 pow_aux_145 + 0.0001 pow_aux_147 + pow_base_aux_150 + [
   0.00015 qq27 * pow_aux_149 ] = 1
 qc2: 0.0205 pow_aux_153 + 0.0001 pow_aux_155 + pow_base_aux_158 + [
   0.00015 qq24 * pow_aux_157 ] = 1
 qc3: 0.0205 pow_aux_161 + 0.0001 pow_aux_163 + pow_base_aux_166 + [
   0.00015 qq26 * pow_aux_165 ] = 1
 qc4: 0.0205 pow_aux_169 + 0.0001 pow_aux_171 + pow_base_aux_174 + [
   0.00015 qq7 * pow_aux_173 ] = 1
 qc5: 0.0255 pow_aux_177 + 0.0001 pow_aux_179 + pow_base_aux_182 + [
   0.00015 qq25 * pow_aux_181 ] = 1
 qc6: 0.0255 pow_aux_185 + 0.0001 pow_aux_187 + pow_base_aux_190 + [
   0.00015 qq8 * pow_aux_189 ] = 1
Bounds
General Constraints
 GC0: pow_aux_1 = POW ( pow_base_aux_0 ^ 4 )
 GC1: pow_aux_3 = POW ( pow_base_aux_2 ^ -3 )
 GC2: pow_aux_5 = POW ( pow_base_aux_4 ^ 2 )
 GC3: pow_aux_7 = POW ( pow_base_aux_6 ^ 2 )
 GC4: pow_aux_9 = POW ( pow_base_aux_8 ^ -3 )
 GC5: pow_aux_11 = POW ( pow_base_aux_10 ^ 2.2 )
 GC6: pow_aux_13 = POW ( pow_base_aux_12 ^ 2 )
 GC7: pow_aux_15 = POW ( pow_base_aux_14 ^ -3 )
 GC8: pow_aux_17 = POW ( pow_base_aux_16 ^ 2.3 )
 GC9: pow_aux_19 = POW ( pow_base_aux_18 ^ 2 )
 GC10: pow_aux_21 = POW ( pow_base_aux_20 ^ -3 )
 GC11: pow_aux_23 = POW ( pow_base_aux_22 ^ 1.3 )
 GC12: pow_aux_25 = POW ( pow_base_aux_24 ^ 2 )
 GC13: pow_aux_27 = POW ( pow_base_aux_26 ^ -3 )
 GC14: pow_aux_29 = POW ( pow_base_aux_28 ^ 4.2 )
 GC15: pow_aux_31 = POW ( pow_base_aux_30 ^ 2 )
 GC16: pow_aux_33 = POW ( pow_base_aux_32 ^ -3 )
 GC17: pow_aux_35 = POW ( pow_base_aux_34 ^ 2.2 )
 GC18: pow_aux_37 = POW ( pow_base_aux_36 ^ 2 )
 GC19: pow_aux_39 = POW ( pow_base_aux_38 ^ -3 )
 GC20: pow_aux_41 = POW ( pow_base_aux_40 ^ 3.1 )
 GC21: pow_aux_43 = POW ( pow_base_aux_42 ^ 2 )
 GC22: pow_aux_45 = POW ( pow_base_aux_44 ^ -3 )
 GC23: pow_aux_47 = POW ( pow_base_aux_46 ^ 4.1 )
 GC24: pow_aux_49 = POW ( pow_base_aux_48 ^ 2 )
 GC25: pow_aux_51 = POW ( pow_base_aux_50 ^ 2.1 )
 GC26: pow_aux_53 = POW ( pow_base_aux_52 ^ 2 )
 GC27: pow_aux_55 = POW ( pow_base_aux_54 ^ 4.2 )
 GC28: pow_aux_57 = POW ( pow_base_aux_56 ^ 2 )
 GC29: pow_aux_59 = POW ( pow_base_aux_58 ^ 2.2 )
 GC30: pow_aux_61 = POW ( pow_base_aux_60 ^ 2 )
 GC31: pow_aux_63 = POW ( pow_base_aux_62 ^ 4.1 )
 GC32: pow_aux_65 = POW ( pow_base_aux_64 ^ 2 )
 GC33: pow_aux_67 = POW ( pow_base_aux_66 ^ 4.1 )
 GC34: pow_aux_69 = POW ( pow_base_aux_68 ^ 2 )
 GC35: pow_aux_71 = POW ( pow_base_aux_70 ^ 3.1 )
 GC36: pow_aux_73 = POW ( pow_base_aux_72 ^ 2 )
 GC37: pow_aux_75 = POW ( pow_base_aux_74 ^ 2 )
 GC38: pow_aux_77 = POW ( pow_base_aux_76 ^ 2 )
 GC39: pow_aux_79 = POW ( pow_base_aux_78 ^ -3 )
 GC40: pow_aux_81 = POW ( pow_base_aux_80 ^ 3.2 )
 GC41: pow_aux_83 = POW ( pow_base_aux_82 ^ 2 )
 GC42: pow_aux_85 = POW ( pow_base_aux_84 ^ 2.1 )
 GC43: pow_aux_87 = POW ( pow_base_aux_86 ^ 2 )
 GC44: pow_aux_89 = POW ( pow_base_aux_88 ^ 2.1 )
 GC45: pow_aux_91 = POW ( pow_base_aux_90 ^ 2 )
 GC46: pow_aux_93 = POW ( pow_base_aux_92 ^ 2.2 )
 GC47: pow_aux_95 = POW ( pow_base_aux_94 ^ 2 )
 GC48: pow_aux_97 = POW ( pow_base_aux_96 ^ 1.3 )
 GC49: pow_aux_99 = POW ( pow_base_aux_98 ^ 2 )
 GC50: pow_aux_101 = POW ( pow_base_aux_100 ^ 2.3 )
 GC51: pow_aux_103 = POW ( pow_base_aux_102 ^ 2 )
 GC52: pow_aux_105 = POW ( pow_base_aux_104 ^ 1.1 )
 GC53: pow_aux_107 = POW ( pow_base_aux_106 ^ 2 )
 GC54: pow_aux_109 = POW ( pow_base_aux_108 ^ 3.1 )
 GC55: pow_aux_111 = POW ( pow_base_aux_110 ^ 2 )
 GC56: pow_aux_113 = POW ( pow_base_aux_112 ^ -3 )
 GC57: pow_aux_115 = POW ( pow_base_aux_114 ^ 2.1 )
 GC58: pow_aux_117 = POW ( pow_base_aux_116 ^ 2 )
 GC59: pow_aux_119 = POW ( pow_base_aux_118 ^ -3 )
 GC60: pow_aux_121 = POW ( pow_base_aux_120 ^ 1.1 )
 GC61: pow_aux_123 = POW ( pow_base_aux_122 ^ 2 )
 GC62: pow_aux_125 = POW ( pow_base_aux_124 ^ -3 )
 GC63: pow_aux_127 = POW ( pow_base_aux_126 ^ 2 )
 GC64: pow_aux_129 = POW ( pow_base_aux_128 ^ 2 )
 GC65: pow_aux_131 = POW ( pow_base_aux_130 ^ -3 )
 GC66: pow_aux_133 = POW ( pow_base_aux_132 ^ 1.1 )
 GC67: pow_aux_135 = POW ( pow_base_aux_134 ^ 2 )
 GC68: pow_aux_137 = POW ( pow_base_aux_136 ^ 4 )
 GC69: pow_aux_139 = POW ( pow_base_aux_138 ^ 1.5 )
 GC70: pow_aux_141 = POW ( pow_base_aux_140 ^ 0.5 )
 GC71: pow_aux_143 = POW ( pow_base_aux_142 ^ 2 )
 GC72: pow_aux_145 = POW ( pow_base_aux_144 ^ 2.1 )
 GC73: pow_aux_147 = POW ( pow_base_aux_146 ^ 1.5 )
 GC74: pow_aux_149 = POW ( pow_base_aux_148 ^ 0.5 )
 GC75: pow_aux_151 = POW ( pow_base_aux_150 ^ 2 )
 GC76: pow_aux_153 = POW ( pow_base_aux_152 ^ 3.1 )
 GC77: pow_aux_155 = POW ( pow_base_aux_154 ^ 1.5 )
 GC78: pow_aux_157 = POW ( pow_base_aux_156 ^ 0.5 )
 GC79: pow_aux_159 = POW ( pow_base_aux_158 ^ 2 )
 GC80: pow_aux_161 = POW ( pow_base_aux_160 ^ 3.1 )
 GC81: pow_aux_163 = POW ( pow_base_aux_162 ^ 1.5 )
 GC82: pow_aux_165 = POW ( pow_base_aux_164 ^ 0.5 )
 GC83: pow_aux_167 = POW ( pow_base_aux_166 ^ 2 )
 GC84: pow_aux_169 = POW ( pow_base_aux_168 ^ 3.1 )
 GC85: pow_aux_171 = POW ( pow_base_aux_170 ^ 1.5 )
 GC86: pow_aux_173 = POW ( pow_base_aux_172 ^ 0.5 )
 GC87: pow_aux_175 = POW ( pow_base_aux_174 ^ 2 )
 GC88: pow_aux_177 = POW ( pow_base_aux_176 ^ 4.1 )
 GC89: pow_aux_179 = POW ( pow_base_aux_178 ^ 1.5 )
 GC90: pow_aux_181 = POW ( pow_base_aux_180 ^ 0.5 )
 GC91: pow_aux_183 = POW ( pow_base_aux_182 ^ 2 )
 GC92: pow_aux_185 = POW ( pow_base_aux_184 ^ 4.1 )
 GC93: pow_aux_187 = POW ( pow_base_aux_186 ^ 1.5 )
 GC94: pow_aux_189 = POW ( pow_base_aux_188 ^ 0.5 )
 GC95: pow_aux_191 = POW ( pow_base_aux_190 ^ 2 )
 GC96: pow_aux_193 = POW ( pow_base_aux_192 ^ -3 )
 GC97: pow_aux_195 = POW ( pow_base_aux_194 ^ 2.1 )
 GC98: pow_aux_197 = POW ( pow_base_aux_196 ^ 2 )
 GC99: pow_aux_199 = POW ( pow_base_aux_198 ^ -3 )
 GC100: pow_aux_201 = POW ( pow_base_aux_200 ^ 1.1 )
 GC101: pow_aux_203 = POW ( pow_base_aux_202 ^ 2 )
 GC102: pow_aux_205 = POW ( pow_base_aux_204 ^ -3 )
 GC103: pow_aux_207 = POW ( pow_base_aux_206 ^ 2.1 )
 GC104: pow_aux_209 = POW ( pow_base_aux_208 ^ 2 )
 GC105: pow_aux_211 = POW ( pow_base_aux_210 ^ -3 )
 GC106: pow_aux_213 = POW ( pow_base_aux_212 ^ 3.1 )
 GC107: pow_aux_215 = POW ( pow_base_aux_214 ^ 2 )
 GC108: pow_aux_217 = POW ( pow_base_aux_216 ^ -3 )
 GC109: pow_aux_219 = POW ( pow_base_aux_218 ^ 4.1 )
 GC110: pow_aux_221 = POW ( pow_base_aux_220 ^ 2 )
 GC111: pow_aux_223 = POW ( pow_base_aux_222 ^ 2.1 )
 GC112: pow_aux_225 = POW ( pow_base_aux_224 ^ 2 )
 GC113: pow_aux_227 = POW ( pow_base_aux_226 ^ 1.1 )
 GC114: pow_aux_229 = POW ( pow_base_aux_228 ^ 2 )
 GC115: pow_aux_231 = POW ( pow_base_aux_230 ^ 1.1 )
 GC116: pow_aux_233 = POW ( pow_base_aux_232 ^ 2 )
 GC117: pow_aux_235 = POW ( pow_base_aux_234 ^ -3 )
 GC118: pow_aux_237 = POW ( pow_base_aux_236 ^ 4.1 )
 GC119: pow_aux_239 = POW ( pow_base_aux_238 ^ 2 )
 GC120: pow_aux_241 = POW ( pow_base_aux_240 ^ -3 )
 GC121: pow_aux_243 = POW ( pow_base_aux_242 ^ 3.1 )
 GC122: pow_aux_245 = POW ( pow_base_aux_244 ^ 2 )
 GC123: pow_aux_247 = POW ( pow_base_aux_246 ^ -3 )
 GC124: pow_aux_249 = POW ( pow_base_aux_248 ^ 2.1 )
 GC125: pow_aux_251 = POW ( pow_base_aux_250 ^ 2 )
 GC126: pow_aux_253 = POW ( pow_base_aux_252 ^ -3 )
 GC127: pow_aux_255 = POW ( pow_base_aux_254 ^ 1.1 )
 GC128: pow_aux_257 = POW ( pow_base_aux_256 ^ 2 )
 GC129: pow_aux_259 = POW ( pow_base_aux_258 ^ -3 )
 GC130: pow_aux_261 = POW ( pow_base_aux_260 ^ 2.1 )
 GC131: pow_aux_263 = POW ( pow_base_aux_262 ^ 2 )
 GC132: pow_aux_265 = POW ( pow_base_aux_264 ^ 2.1 )
 GC133: pow_aux_267 = POW ( pow_base_aux_266 ^ 2 )
 GC134: pow_aux_269 = POW ( pow_base_aux_268 ^ 2.1 )
 GC135: pow_aux_271 = POW ( pow_base_aux_270 ^ 2 )
 GC136: pow_aux_273 = POW ( pow_base_aux_272 ^ 3.1 )
 GC137: pow_aux_275 = POW ( pow_base_aux_274 ^ 2 )
 GC138: pow_aux_277 = POW ( pow_base_aux_276 ^ 1.1 )
 GC139: pow_aux_279 = POW ( pow_base_aux_278 ^ 2 )
 GC140: pow_aux_281 = POW ( pow_base_aux_280 ^ 1.1 )
 GC141: pow_aux_283 = POW ( pow_base_aux_282 ^ 2 )
 GC142: pow_aux_285 = POW ( pow_base_aux_284 ^ 3.2 )
 GC143: pow_aux_287 = POW ( pow_base_aux_286 ^ 2 )
 GC144: pow_aux_289 = POW ( pow_base_aux_288 ^ -3 )
 GC145: pow_aux_291 = POW ( pow_base_aux_290 ^ 3.1 )
 GC146: pow_aux_293 = POW ( pow_base_aux_292 ^ 2 )
 GC147: pow_aux_295 = POW ( pow_base_aux_294 ^ -3 )
 GC148: pow_aux_297 = POW ( pow_base_aux_296 ^ 4.1 )
 GC149: pow_aux_299 = POW ( pow_base_aux_298 ^ 2 )
 GC150: pow_aux_301 = POW ( pow_base_aux_300 ^ -3 )
 GC151: pow_aux_303 = POW ( pow_base_aux_302 ^ 3.1 )
 GC152: pow_aux_305 = POW ( pow_base_aux_304 ^ 2 )
 GC153: pow_aux_307 = POW ( pow_base_aux_306 ^ -3 )
 GC154: pow_aux_309 = POW ( pow_base_aux_308 ^ 2.1 )
 GC155: pow_aux_311 = POW ( pow_base_aux_310 ^ 2 )
 GC156: pow_aux_313 = POW ( pow_base_aux_312 ^ -3 )
 GC157: pow_aux_315 = POW ( pow_base_aux_314 ^ 1.1 )
 GC158: pow_aux_317 = POW ( pow_base_aux_316 ^ 2 )
 GC159: pow_aux_319 = POW ( pow_base_aux_318 ^ -3 )
 GC160: pow_aux_321 = POW ( pow_base_aux_320 ^ 2.1 )
 GC161: pow_aux_323 = POW ( pow_base_aux_322 ^ 2 )
 GC162: pow_aux_325 = POW ( pow_base_aux_324 ^ -3 )
 GC163: pow_aux_327 = POW ( pow_base_aux_326 ^ 3.1 )
 GC164: pow_aux_329 = POW ( pow_base_aux_328 ^ 2 )
End
