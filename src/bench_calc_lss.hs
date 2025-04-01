
import Data.Bits (Bits, (.&.), (.|.), shiftL, shiftR, xor)
import Data.Char as C
import Data.Int (Int64, Int8)
import Data.Vector.Persistent (Vector(..))
import Data.Word (Word8)
import Distribution.Simple.Utils (xargs)
import qualified Data.Vector.Persistent as V
import System.IO.Unsafe (unsafePerformIO)

intrinsic_ByteToIntSigned :: Word8 -> Int64
intrinsic_ByteToIntSigned x = fromIntegral (fromIntegral x :: Int8)

intrinsicGet :: Vector a -> Int64 -> a
intrinsicGet xs = V.unsafeIndex xs . fromIntegral

intrinsicExtract :: Vector a -> Int64 -> (a, a -> Vector a)
intrinsicExtract xs i = (V.unsafeIndex xs i', f)
  where
    i' = fromIntegral i
    f x = V.update i' x xs

intrinsicLen :: Vector a -> Int64
intrinsicLen = fromIntegral . V.length

intrinsicPush :: Vector a -> a -> Vector a
intrinsicPush = V.snoc

intrinsicPop :: Vector a -> (Vector a, a)
intrinsicPop xs = (V.drop 1 xs, last)
  where
    last = xs `V.unsafeIndex` (V.length xs - 1)

intrinsicReserve :: Vector a -> Int64 -> Vector a
intrinsicReserve = const

intrinsicReplace :: (a -> Vector a) -> a -> Vector a
intrinsicReplace = id

shiftL_ :: Bits a => a -> Int64 -> a
shiftL_ x i = x `shiftL` fromIntegral i
shiftR_ :: Bits a => a -> Int64 -> a
shiftR_ x i = x `shiftR` fromIntegral i
_pv_string :: Vector Word8 -> String
_pv_string = V.foldr (:) [] . fmap (C.chr . fromIntegral)
_string_pv :: String -> Vector Word8
_string_pv = V.fromList . map (fromIntegral . C.ord)

class Divisible a where
  divv :: a -> a -> a
instance Divisible Int64 where
  divv = div
instance Divisible Double where
  divv = (/)

-- IO
input :: () -> Vector Word8
input _ = unsafePerformIO $ do
  line <- getLine
  return $ _string_pv line
output :: Vector Word8 -> ()
output s = unsafePerformIO $ putStr $ _pv_string s
panic :: Vector Word8 -> a
panic = error . _pv_string

data Option0
  = Some0_0 Int64
  | None0_1

data Expr9
  = Const9_0 Int64
  | Add9_1 (Expr9, Expr9)
  | Sub9_2 (Expr9, Expr9)
  | Mul9_3 (Expr9, Expr9)
  | Div9_4 (Expr9, Expr9)

data Option10
  = Some10_0 Expr9
  | None10_1

data Option11
  = Some11_0 (Int64, Word8)
  | None11_1

data Option17
  = Some17_0 (Int64, ())
  | None17_1

data Option20
  = Some20_0 ()
  | None20_1

data Option21
  = Some21_0 (Int64, Option20)
  | None21_1

data Option28
  = Some28_0 (Int64, Expr9)
  | None28_1

data Option69
  = Some69_0 (Int64, Int64)
  | None69_1

data Option72
  = Some72_0 (Int64, Option0)
  | None72_1

data Option85
  = Some85_0 (Vector Int64) 
  | None85_1

data Closure90
  = Variant90_0 (Int64, Int64)

data Iter1
  = Iter1_0 Closure90

data Option2
  = Some2_0 (Int64, Iter1)
  | None2_1

data Closure92
  = Variant92_0 (Vector Word8) 

data Closure91
  = Variant91_0 (Iter1, Closure92)

data Iter3
  = Iter3_0 Closure91

data Option4
  = Some4_0 (Word8, Iter3)
  | None4_1

data Closure93
  = Variant93_0 (Int64, Int64)

data Iter5
  = Iter5_0 Closure93

data Option6
  = Some6_0 (Int64, Iter5)
  | None6_1

data Closure95
  = Variant95_0 (Vector ((Vector Word8) )) 

data Closure94
  = Variant94_0 (Iter5, Closure95)

data Iter7
  = Iter7_0 Closure94

data Option8
  = Some8_0 ((Vector Word8) , Iter7)
  | None8_1

data Closure96
  = Variant96_0 Word8
  | Variant96_1 ()

data Parse12
  = Parse12_0 Closure96

data Closure97
  = Variant97_0 ()

data Parse13
  = Parse13_0 Closure97

data Closure100
  = Variant100_0 Word8

data Closure99
  = Variant99_0 Closure100

data Closure98
  = Variant98_0 (Parse13, Closure99)

data Parse14
  = Parse14_0 Closure98

data Closure101
  = Variant101_0 (Parse14, Parse14)

data Parse15
  = Parse15_0 Closure101

data Closure102
  = Variant102_0 (Parse14, Parse15)

data Parse16
  = Parse16_0 Closure102

data Closure104
  = Variant104_0 ()

data Closure103
  = Variant103_0 (Parse16, Closure104)

data Parse18
  = Parse18_0 Closure103

data Closure107
  = Variant107_0 ()

data Closure106
  = Variant106_0 ((), Parse18, Closure107)

data Closure108
  = Variant108_0 Option20

data Parse22
  = Parse22_0 Closure108

data Closure110
  = Variant110_0 ()

data Closure109
  = Variant109_0 (Parse18, Closure110)

data Parse23
  = Parse23_0 Closure109

data Closure112
  = Variant112_0 ()

data Closure111
  = Variant111_0 Closure112

data Parse24
  = Parse24_0 Closure111

data Closure113
  = Variant113_0 (Parse23, Parse24)

data Parse25
  = Parse25_0 Closure113

data Closure105
  = Variant105_0 ()
  | Variant105_1 (Parse25, Closure106)

data Parse19
  = Parse19_0 Closure105

data Closure116
  = Variant116_0 Parse18

data Closure115
  = Variant115_0 Closure116

data Parse27
  = Parse27_0 Closure115

data Closure120
  = Variant120_0 ()
  | Variant120_1 ()

data Option32
  = Some32_0 (Int64, Closure120)
  | None32_1

data Option37
  = Some37_0 (Int64, (Closure120, Expr9))
  | None37_1

data Option44
  = Some44_0 (Closure120, Expr9)
  | None44_1

data Option45
  = Some45_0 (Int64, Option44)
  | None45_1

data Closure122
  = Variant122_0 ()

data Closure121
  = Variant121_0 (Parse14, Closure122)

data Parse33
  = Parse33_0 Closure121

data Closure124
  = Variant124_0 ()

data Closure123
  = Variant123_0 (Parse14, Closure124)

data Parse34
  = Parse34_0 Closure123

data Closure125
  = Variant125_0 (Parse33, Parse34)

data Parse35
  = Parse35_0 Closure125

data Closure126
  = Variant126_0 (Parse35, Parse27)

data Parse36
  = Parse36_0 Closure126

data Closure127
  = Variant127_0 (Closure120, Expr9)

data Parse38
  = Parse38_0 Closure127

data Closure130
  = Variant130_0 Closure120

data Closure131
  = Variant131_0 (Parse27, Parse36)

data Parse41
  = Parse41_0 Closure131

data Closure136
  = Variant136_0 ()

data Closure137
  = Variant137_0 Option44

data Parse46
  = Parse46_0 Closure137

data Closure139
  = Variant139_0 ()

data Closure141
  = Variant141_0 ()

data Closure140
  = Variant140_0 Closure141

data Parse48
  = Parse48_0 Closure140

data Closure143
  = Variant143_0 ()
  | Variant143_1 ()

data Option50
  = Some50_0 (Int64, Closure143)
  | None50_1

data Option55
  = Some55_0 (Int64, (Closure143, Expr9))
  | None55_1

data Option62
  = Some62_0 (Closure143, Expr9)
  | None62_1

data Option63
  = Some63_0 (Int64, Option62)
  | None63_1

data Closure145
  = Variant145_0 ()

data Closure144
  = Variant144_0 (Parse14, Closure145)

data Parse51
  = Parse51_0 Closure144

data Closure147
  = Variant147_0 ()

data Closure146
  = Variant146_0 (Parse14, Closure147)

data Parse52
  = Parse52_0 Closure146

data Closure148
  = Variant148_0 (Parse51, Parse52)

data Parse53
  = Parse53_0 Closure148

data Closure149
  = Variant149_0 (Parse53, Parse27)

data Parse54
  = Parse54_0 Closure149

data Closure150
  = Variant150_0 (Closure143, Expr9)

data Parse56
  = Parse56_0 Closure150

data Closure154
  = Variant154_0 Closure143

data Closure155
  = Variant155_0 (Parse27, Parse54)

data Parse59
  = Parse59_0 Closure155

data Closure160
  = Variant160_0 ()

data Closure161
  = Variant161_0 Option62

data Parse64
  = Parse64_0 Closure161

data Closure163
  = Variant163_0 ()

data Closure165
  = Variant165_0 ()

data Closure164
  = Variant164_0 Closure165

data Parse66
  = Parse66_0 Closure164

data Closure169
  = Variant169_0 (Word8, Word8)

data Closure168
  = Variant168_0 Closure169

data Closure167
  = Variant167_0 (Parse13, Closure168)

data Parse68
  = Parse68_0 Closure167

data Closure171
  = Variant171_0 ()

data Closure170
  = Variant170_0 (Parse68, Closure171)

data Parse70
  = Parse70_0 Closure170

data Closure174
  = Variant174_0 ()

data Closure173
  = Variant173_0 (Int64, Parse70, Closure174)

data Closure175
  = Variant175_0 Option0

data Parse73
  = Parse73_0 Closure175

data Closure177
  = Variant177_0 ()

data Closure176
  = Variant176_0 (Parse70, Closure177)

data Parse74
  = Parse74_0 Closure176

data Closure179
  = Variant179_0 ()

data Closure178
  = Variant178_0 Closure179

data Parse75
  = Parse75_0 Closure178

data Closure180
  = Variant180_0 (Parse74, Parse75)

data Parse76
  = Parse76_0 Closure180

data Closure172
  = Variant172_0 Int64
  | Variant172_1 (Parse76, Closure173)

data Parse71
  = Parse71_0 Closure172

data Closure182
  = Variant182_0 (Parse70, Closure174, Int64)

data Closure181
  = Variant181_0 (Parse70, Closure182)

data Parse77
  = Parse77_0 Closure181

data Closure188
  = Variant188_0 ()

data Closure187
  = Variant187_0 (Parse77, Closure188)

data Parse81
  = Parse81_0 Closure187

data Closure190
  = Variant190_0 ()

data Closure189
  = Variant189_0 Closure190

data Parse82
  = Parse82_0 Closure189

data Closure128
  = Variant128_0 (Parse81, Parse82)

data Parse39
  = Parse39_0 Closure128

data Closure129
  = Variant129_0 (Parse39, Closure130)

data Parse40
  = Parse40_0 Closure129

data Closure133
  = Variant133_0 Parse39

data Closure132
  = Variant132_0 (Parse41, Closure133)

data Parse42
  = Parse42_0 Closure132

data Closure135
  = Variant135_0 (Expr9, Parse42, Closure136)

data Closure138
  = Variant138_0 (Parse42, Closure139)

data Parse47
  = Parse47_0 Closure138

data Closure142
  = Variant142_0 (Parse47, Parse48)

data Parse49
  = Parse49_0 Closure142

data Closure134
  = Variant134_0 Expr9
  | Variant134_1 (Parse49, Closure135)

data Parse43
  = Parse43_0 Closure134

data Closure152
  = Variant152_0 (Parse35, Parse39)

data Closure151
  = Variant151_0 (Parse39, Closure152)

data Parse57
  = Parse57_0 Closure151

data Closure153
  = Variant153_0 (Parse57, Closure154)

data Parse58
  = Parse58_0 Closure153

data Closure157
  = Variant157_0 Parse57

data Closure156
  = Variant156_0 (Parse59, Closure157)

data Parse60
  = Parse60_0 Closure156

data Closure159
  = Variant159_0 (Expr9, Parse60, Closure160)

data Closure162
  = Variant162_0 (Parse60, Closure163)

data Parse65
  = Parse65_0 Closure162

data Closure166
  = Variant166_0 (Parse65, Parse66)

data Parse67
  = Parse67_0 Closure166

data Closure158
  = Variant158_0 Expr9
  | Variant158_1 (Parse67, Closure159)

data Parse61
  = Parse61_0 Closure158

data Closure185
  = Variant185_0 (Parse53, Parse57)

data Closure184
  = Variant184_0 (Parse57, Closure185)

data Parse79
  = Parse79_0 Closure184

data Closure186
  = Variant186_0 (Parse79, Parse27)

data Parse80
  = Parse80_0 Closure186

data Closure114
  = Variant114_0 (Parse27, Parse80)

data Parse26
  = Parse26_0 Closure114

data Closure117
  = Variant117_0 (Parse26, Parse27)

data Parse29
  = Parse29_0 Closure117

data Closure118
  = Variant118_0 (Parse27, Parse29)

data Parse30
  = Parse30_0 Closure118

data Closure119
  = Variant119_0 (Parse30, Parse14)

data Parse31
  = Parse31_0 Closure119

data Closure183
  = Variant183_0 (Parse14, Parse31)

data Parse78
  = Parse78_0 Closure183

data Closure192
  = Variant192_0 ()

data Closure191
  = Variant191_0 (Iter7, Closure192)

data Iter83
  = Iter83_0 Closure191

data Option84
  = Some84_0 (Int64, Iter83)
  | None84_1

data Closure193
  = Variant193_0 (Int64, Int64)

data Iter86
  = Iter86_0 Closure193

data Option87
  = Some87_0 (Int64, Iter86)
  | None87_1

data Closure195
  = Variant195_0 (Vector Int64) 

data Closure194
  = Variant194_0 (Iter86, Closure195)

data Iter88
  = Iter88_0 Closure194

data Option89
  = Some89_0 (Int64, Iter88)
  | None89_1

data Closure196
  = Variant196_0 ()

data Closure197
  = Variant197_0 ()

data Closure198
  = Variant198_0 ()

data Closure199
  = Variant199_0 ()

data Closure200
  = Variant200_0 ()

data Closure201
  = Variant201_0 ()

data Closure202
  = Variant202_0 ()

data Closure203
  = Variant203_0 ()

data Closure204
  = Variant204_0 ()

data Closure205
  = Variant205_0 ()

data Closure206
  = Variant206_0 ()

data Closure207
  = Variant207_0 ()

data Closure208
  = Variant208_0 ()

data Closure209
  = Variant209_0 ()

data Closure210
  = Variant210_0

data Closure211
  = Variant211_0 ()

data Closure212
  = Variant212_0

data Closure213
  = Variant213_0 ()

data Closure214
  = Variant214_0 ()

data Closure215
  = Variant215_0 ()

data Closure216
  = Variant216_0 ()

data Closure217
  = Variant217_0 ()

data Closure218
  = Variant218_0

data Closure219
  = Variant219_0 ()

data Closure220
  = Variant220_0 ()

data Closure221
  = Variant221_0 ()

data Closure222
  = Variant222_0 ()

data Closure223
  = Variant223_0 ()

data Closure224
  = Variant224_0 ()

data Closure225
  = Variant225_0 ()

data Closure226
  = Variant226_0 ()

data Closure227
  = Variant227_0 ()

data Closure228
  = Variant228_0 ()

data Closure229
  = Variant229_0 ()

data Closure230
  = Variant230_0 ()

data Closure231
  = Variant231_0 ()

data Closure232
  = Variant232_0 ()

data Closure233
  = Variant233_0 ()

data Closure234
  = Variant234_0 ()

data Closure235
  = Variant235_0 ()

data Closure236
  = Variant236_0 ()

data Closure237
  = Variant237_0 ()

data Closure238
  = Variant238_0 ()

data Closure239
  = Variant239_0 ()

data Closure240
  = Variant240_0 ()

data Closure241
  = Variant241_0 ()

data Closure242
  = Variant242_0 ()

data Closure243
  = Variant243_0 ()

data Closure244
  = Variant244_0 ()

data Closure245
  = Variant245_0 ()

data Closure246
  = Variant246_0 ()

data Closure247
  = Variant247_0 ()

data Closure248
  = Variant248_0 ()

data Closure249
  = Variant249_0 ()

data Closure250
  = Variant250_0 ()

data Closure251
  = Variant251_0 ()

data Closure252
  = Variant252_0 ()

data Closure253
  = Variant253_0 ()

data Closure254
  = Variant254_0 ()

data Closure255
  = Variant255_0 ()

data Closure256
  = Variant256_0 ()

data Closure257
  = Variant257_0 ()

data Closure258
  = Variant258_0 ()

data Closure259
  = Variant259_0 ()

data Closure260
  = Variant260_0 ()

data Closure261
  = Variant261_0 ()

data Closure262
  = Variant262_0 ()

data Closure263
  = Variant263_0 ()

data Closure264
  = Variant264_0 ()

data Closure265
  = Variant265_0 ()

data Closure266
  = Variant266_0 ()

data Closure267
  = Variant267_0 ()

data Closure268
  = Variant268_0 ()

data Closure269
  = Variant269_0 ()

data Closure270
  = Variant270_0 ()

data Closure271
  = Variant271_0 ()

data Closure272
  = Variant272_0 ()

data Closure273
  = Variant273_0 ()

data Closure274
  = Variant274_0 ()

data Closure275
  = Variant275_0 ()

data Closure276
  = Variant276_0 ()

data Closure277
  = Variant277_0 ()

data Closure278
  = Variant278_0 ()

data Closure279
  = Variant279_0 ()

data Closure280
  = Variant280_0 ()

data Closure281
  = Variant281_0 ()

data Closure282
  = Variant282_0 ()

data Closure283
  = Variant283_0 ()

data Closure284
  = Variant284_0 ()

data Closure285
  = Variant285_0 ()

data Closure286
  = Variant286_0 ()

data Closure287
  = Variant287_0 ()

data Closure288
  = Variant288_0 ()

data Closure289
  = Variant289_0 ()

data Closure290
  = Variant290_0 ()

data Closure291
  = Variant291_0 ()

data Closure292
  = Variant292_0 ()

data Closure293
  = Variant293_0 ()

data Closure294
  = Variant294_0 ()

data Closure295
  = Variant295_0 ()

data Closure296
  = Variant296_0 ()

data Closure297
  = Variant297_0 ()

data Closure298
  = Variant298_0 ()

data Closure299
  = Variant299_0 ()

data Closure300
  = Variant300_0 ()

data Closure301
  = Variant301_0 ()

data Closure302
  = Variant302_0 ()

data Closure303
  = Variant303_0 ()

data Closure304
  = Variant304_0 ()

data Closure305
  = Variant305_0 ()

data Closure306
  = Variant306_0 ()

data Closure307
  = Variant307_0 ()

data Closure308
  = Variant308_0 ()

data Closure309
  = Variant309_0 ()

data Closure310
  = Variant310_0 ()

data Closure311
  = Variant311_0 ()

data Closure312
  = Variant312_0 ()

data Closure313
  = Variant313_0 ()

data Closure314
  = Variant314_0 ()

data Closure315
  = Variant315_0 ()

data Closure316
  = Variant316_0 ()

data Closure317
  = Variant317_0 ()

data Closure318
  = Variant318_0 ()

data Closure319
  = Variant319_0 ()

data Closure320
  = Variant320_0 ()

data Closure321
  = Variant321_0 ()

data Closure322
  = Variant322_0 ()

data Closure323
  = Variant323_0 ()

data Closure324
  = Variant324_0 ()

data Closure325
  = Variant325_0 ()

data Closure326
  = Variant326_0 ()

data Closure327
  = Variant327_0 ()

data Closure328
  = Variant328_0 ()

data Closure329
  = Variant329_0 ()

data Closure330
  = Variant330_0 ()

data Closure331
  = Variant331_0 ()

data Closure332
  = Variant332_0 ()

data Closure333
  = Variant333_0 ()

data Closure334
  = Variant334_0 ()

data Closure335
  = Variant335_0 ()

data Closure336
  = Variant336_0 ()

data Closure337
  = Variant337_0 ()

data Closure338
  = Variant338_0 ()

data Closure339
  = Variant339_0 ()

data Closure340
  = Variant340_0 ()

data Closure341
  = Variant341_0 ()

data Closure342
  = Variant342_0 ()

data Closure343
  = Variant343_0 ()

data Closure344
  = Variant344_0 ()

data Closure345
  = Variant345_0 ()

data Closure346
  = Variant346_0 ()

data Closure347
  = Variant347_0 ()

data Closure348
  = Variant348_0 ()

data Closure349
  = Variant349_0 ()

data Closure350
  = Variant350_0 ()

data Closure351
  = Variant351_0 ()

data Closure352
  = Variant352_0 ()

data Closure353
  = Variant353_0 ()

data Closure354
  = Variant354_0 ()

data Closure355
  = Variant355_0 ()

data Closure356
  = Variant356_0 ()

data Closure357
  = Variant357_0 ()

data Closure358
  = Variant358_0 ()

data Closure359
  = Variant359_0 ()

data Closure360
  = Variant360_0 ()

data Closure361
  = Variant361_0 ()

data Closure362
  = Variant362_0 ()

data Closure363
  = Variant363_0 ()

data Closure364
  = Variant364_0 ()

data Closure365
  = Variant365_0 ()

data Closure366
  = Variant366_0 ()

data Closure367
  = Variant367_0 ()

data Closure368
  = Variant368_0 ()

data Closure369
  = Variant369_0 ()

data Closure370
  = Variant370_0 ()

data Closure371
  = Variant371_0 ()

data Closure372
  = Variant372_0 ()

data Closure373
  = Variant373_0 ()

data Closure374
  = Variant374_0 ()

data Closure375
  = Variant375_0 ()

data Closure376
  = Variant376_0 ()

data Closure377
  = Variant377_0 ()

data Closure378
  = Variant378_0 ()

data Closure379
  = Variant379_0 ()

data Closure380
  = Variant380_0 ()

data Closure381
  = Variant381_0 ()

data Closure382
  = Variant382_0 ()

data Closure383
  = Variant383_0 ()

data Closure384
  = Variant384_0 ()

data Closure385
  = Variant385_0 ()

data Closure386
  = Variant386_0 ()

data Closure387
  = Variant387_0 ()

data Closure388
  = Variant388_0 ()

data Closure389
  = Variant389_0 ()

data Closure390
  = Variant390_0 ()

data Closure391
  = Variant391_0 ()

data Closure392
  = Variant392_0 ()

data Closure393
  = Variant393_0 ()

data Closure394
  = Variant394_0 ()

data Closure395
  = Variant395_0 ()

data Closure396
  = Variant396_0 ()

data Closure397
  = Variant397_0 ()

data Closure398
  = Variant398_0 ()

data Closure399
  = Variant399_0 ()

data Closure400
  = Variant400_0 ()

data Closure401
  = Variant401_0 ()

data Closure402
  = Variant402_0 ()

data Closure403
  = Variant403_0 ()

data Closure404
  = Variant404_0 ()

data Closure405
  = Variant405_0 ()

data Closure406
  = Variant406_0 ()

data Closure407
  = Variant407_0 ()

data Closure408
  = Variant408_0 ()

data Closure409
  = Variant409_0 ()

data Closure410
  = Variant410_0 ()

data Closure411
  = Variant411_0 ()

data Closure412
  = Variant412_0 ()

data Closure413
  = Variant413_0 ()

data Closure414
  = Variant414_0 ()

data Closure415
  = Variant415_0 ()

data Closure416
  = Variant416_0 ()

data Closure417
  = Variant417_0 ()

data Closure418
  = Variant418_0

data Closure419
  = Variant419_0 ()

data Closure420
  = Variant420_0 ()

data Closure421
  = Variant421_0 ()

data Closure422
  = Variant422_0 ()

data Closure423
  = Variant423_0 ()

data Closure424
  = Variant424_0 ()

data Closure425
  = Variant425_0 ()

data Closure426
  = Variant426_0 ()

data Closure427
  = Variant427_0 ()

data Closure428
  = Variant428_0 ()

data Closure429
  = Variant429_0 ()

data Closure430
  = Variant430_0 ()

data Closure431
  = Variant431_0 ()

data Closure432
  = Variant432_0 ()

data Closure433
  = Variant433_0 ()

data Closure434
  = Variant434_0 ()

data Closure435
  = Variant435_0

data Closure436
  = Variant436_0 ()

data Closure437
  = Variant437_0 ()

data Closure438
  = Variant438_0 ()

data Closure439
  = Variant439_0 ()

data Closure440
  = Variant440_0 ()

data Closure441
  = Variant441_0 ()

data Closure442
  = Variant442_0 ()

data Closure443
  = Variant443_0 ()

data Closure444
  = Variant444_0 ()

data Closure445
  = Variant445_0 (Vector ((Vector Word8) )) 

data Closure446
  = Variant446_0 ()

data Closure447
  = Variant447_0 ()

data Closure448
  = Variant448_0 Word8

data Closure449
  = Variant449_0 Int64

data Closure450
  = Variant450_0 ((Vector Word8) , Closure99)

data Closure451
  = Variant451_0 (Int64, (Vector Word8) , Parse14)

data Closure452
  = Variant452_0 (Int64, (Vector Word8) , Parse15)

data Closure453
  = Variant453_0 Closure104

data Closure454
  = Variant454_0 Closure110

data Closure455
  = Variant455_0 (Int64, (Vector Word8) , Parse24)

data Closure456
  = Variant456_0 ((Vector Word8) , Closure106)

data Closure457
  = Variant457_0 ((Vector Word8) , Parse27)

data Closure458
  = Variant458_0 Expr9

data Closure459
  = Variant459_0 ((Vector Word8) , Parse29)

data Closure460
  = Variant460_0 ((Vector Word8) , Parse14)

data Closure461
  = Variant461_0 Expr9

data Closure462
  = Variant462_0 ((Vector Word8) , Parse31)

data Closure463
  = Variant463_0 Closure122

data Closure464
  = Variant464_0 Closure124

data Closure465
  = Variant465_0 (Int64, (Vector Word8) , Parse34)

data Closure466
  = Variant466_0 ((Vector Word8) , Parse27)

data Closure467
  = Variant467_0 Closure120

data Closure468
  = Variant468_0 ((Vector Word8) , Parse36)

data Closure469
  = Variant469_0 ((Vector Word8) , Closure130)

data Closure470
  = Variant470_0 ((Vector Word8) , Closure133)

data Closure471
  = Variant471_0 Closure139

data Closure472
  = Variant472_0 (Int64, (Vector Word8) , Parse48)

data Closure473
  = Variant473_0 ((Vector Word8) , Closure135)

data Closure474
  = Variant474_0 ((Vector Word8) , Closure152)

data Closure475
  = Variant475_0 Closure145

data Closure476
  = Variant476_0 Closure147

data Closure477
  = Variant477_0 (Int64, (Vector Word8) , Parse52)

data Closure478
  = Variant478_0 ((Vector Word8) , Parse27)

data Closure479
  = Variant479_0 Closure143

data Closure480
  = Variant480_0 ((Vector Word8) , Parse54)

data Closure481
  = Variant481_0 ((Vector Word8) , Closure154)

data Closure482
  = Variant482_0 ((Vector Word8) , Closure157)

data Closure483
  = Variant483_0 Closure163

data Closure484
  = Variant484_0 (Int64, (Vector Word8) , Parse66)

data Closure485
  = Variant485_0 ((Vector Word8) , Closure159)

data Closure486
  = Variant486_0 ((Vector Word8) , Closure185)

data Closure487
  = Variant487_0 ((Vector Word8) , Closure168)

data Closure488
  = Variant488_0 Closure171

data Closure489
  = Variant489_0 Closure177

data Closure490
  = Variant490_0 (Int64, (Vector Word8) , Parse75)

data Closure491
  = Variant491_0 ((Vector Word8) , Closure173)

data Closure492
  = Variant492_0 ((Vector Word8) , Closure182)

data Closure493
  = Variant493_0 Closure188

data Closure494
  = Variant494_0 ((Vector Word8) , Parse80)

data Closure495
  = Variant495_0 (Int64, (Vector Word8) , Parse82)

data Closure496
  = Variant496_0 (Vector Word8) 

data Closure497
  = Variant497_0 ()

range_0 :: () -> Closure196
range_0 () =
  Variant196_0 ()

next_1 :: () -> Closure197
next_1 () =
  Variant197_0 ()

map_2 :: () -> Closure198
map_2 () =
  Variant198_0 ()

next_3 :: () -> Closure199
next_3 () =
  Variant199_0 ()

map_4 :: () -> Closure200
map_4 () =
  Variant200_0 ()

digit_to_nat_5 :: () -> Closure201
digit_to_nat_5 () =
  Variant201_0 ()

and_then_6 :: () -> Closure202
and_then_6 () =
  Variant202_0 ()

foldl_7 :: () -> Closure203
foldl_7 () =
  Variant203_0 ()

wrapped_foldl_8 :: () -> Closure204
wrapped_foldl_8 () =
  Variant204_0 ()

chars_to_nat_9 :: () -> Closure205
chars_to_nat_9 () =
  Variant205_0 ()

wrapped_map_10 :: () -> Closure206
wrapped_map_10 () =
  Variant206_0 ()

wrapped_range_11 :: () -> Closure207
wrapped_range_11 () =
  Variant207_0 ()

items_12 :: () -> Closure208
items_12 () =
  Variant208_0 ()

string_to_nat_13 :: () -> Closure209
string_to_nat_13 () =
  Variant209_0 ()

len__15 :: () -> Closure210
len__15 () =
  Variant210_0

len_14 :: () -> Closure210
len_14 () =
  len__15 ()

read_input_rec_16 :: () -> Closure211
read_input_rec_16 () =
  Variant211_0 ()

push__18 :: () -> Closure212
push__18 () =
  Variant212_0

push_17 :: () -> Closure212
push_17 () =
  push__18 ()

wrapped_read_input_rec_19 :: () -> Closure213
wrapped_read_input_rec_19 () =
  Variant213_0 ()

read_input_20 :: () -> Closure214
read_input_20 () =
  Variant214_0 ()

range_21 :: () -> Closure215
range_21 () =
  Variant215_0 ()

next_22 :: () -> Closure216
next_22 () =
  Variant216_0 ()

map_23 :: () -> Closure217
map_23 () =
  Variant217_0 ()

get__25 :: () -> Closure218
get__25 () =
  Variant218_0

get_24 :: () -> Closure218
get_24 () =
  get__25 ()

pure_26 :: () -> Closure219
pure_26 () =
  Variant219_0 ()

fail_27 :: () -> Parse12
fail_27 () =
  Parse12_0 (Variant96_1 ())

parse_from_28 :: () -> Closure220
parse_from_28 () =
  Variant220_0 ()

parse_from_29 :: () -> Closure221
parse_from_29 () =
  Variant221_0 ()

and_then_30 :: () -> Closure222
and_then_30 () =
  Variant222_0 ()

parse_from_31 :: () -> Closure223
parse_from_31 () =
  Variant223_0 ()

or_else_32 :: () -> Closure224
or_else_32 () =
  Variant224_0 ()

parse_from_33 :: () -> Closure225
parse_from_33 () =
  Variant225_0 ()

or_else_34 :: () -> Closure226
or_else_34 () =
  Variant226_0 ()

parse_from_35 :: () -> Closure227
parse_from_35 () =
  Variant227_0 ()

map_36 :: () -> Closure228
map_36 () =
  Variant228_0 ()

parse_from_37 :: () -> Closure229
parse_from_37 () =
  Variant229_0 ()

map_38 :: () -> Closure230
map_38 () =
  Variant230_0 ()

pure_39 :: () -> Closure231
pure_39 () =
  Variant231_0 ()

parse_from_40 :: () -> Closure232
parse_from_40 () =
  Variant232_0 ()

parse_from_41 :: () -> Closure233
parse_from_41 () =
  Variant233_0 ()

parse_from_42 :: () -> Closure234
parse_from_42 () =
  Variant234_0 ()

or_else_43 :: () -> Closure235
or_else_43 () =
  Variant235_0 ()

pure_44 :: () -> Closure236
pure_44 () =
  Variant236_0 ()

many0_fold_45 :: () -> Closure237
many0_fold_45 () =
  Variant237_0 ()

parse_from_46 :: () -> Closure238
parse_from_46 () =
  Variant238_0 ()

parse_from_47 :: () -> Closure239
parse_from_47 () =
  Variant239_0 ()

and_then_48 :: () -> Closure240
and_then_48 () =
  Variant240_0 ()

and_then_49 :: () -> Closure241
and_then_49 () =
  Variant241_0 ()

or_50 :: () -> Closure242
or_50 () =
  Variant242_0 ()

map_51 :: () -> Closure243
map_51 () =
  Variant243_0 ()

lazy_52 :: () -> Closure244
lazy_52 () =
  Variant244_0 ()

optional_53 :: () -> Closure245
optional_53 () =
  Variant245_0 ()

wrapped_many0_fold_54 :: () -> Closure246
wrapped_many0_fold_54 () =
  Variant246_0 ()

parse_from_55 :: () -> Closure247
parse_from_55 () =
  Variant247_0 ()

parse_from_56 :: () -> Closure248
parse_from_56 () =
  Variant248_0 ()

map_57 :: () -> Closure249
map_57 () =
  Variant249_0 ()

and_then_58 :: () -> Closure250
and_then_58 () =
  Variant250_0 ()

parse_from_59 :: () -> Closure251
parse_from_59 () =
  Variant251_0 ()

and_then_60 :: () -> Closure252
and_then_60 () =
  Variant252_0 ()

parse_from_61 :: () -> Closure253
parse_from_61 () =
  Variant253_0 ()

map_62 :: () -> Closure254
map_62 () =
  Variant254_0 ()

and_then_63 :: () -> Closure255
and_then_63 () =
  Variant255_0 ()

parse_from_64 :: () -> Closure256
parse_from_64 () =
  Variant256_0 ()

and_then_65 :: () -> Closure257
and_then_65 () =
  Variant257_0 ()

map_66 :: () -> Closure258
map_66 () =
  Variant258_0 ()

map_67 :: () -> Closure259
map_67 () =
  Variant259_0 ()

parse_from_68 :: () -> Closure260
parse_from_68 () =
  Variant260_0 ()

parse_from_69 :: () -> Closure261
parse_from_69 () =
  Variant261_0 ()

or_else_70 :: () -> Closure262
or_else_70 () =
  Variant262_0 ()

parse_from_71 :: () -> Closure263
parse_from_71 () =
  Variant263_0 ()

map_72 :: () -> Closure264
map_72 () =
  Variant264_0 ()

and_then_73 :: () -> Closure265
and_then_73 () =
  Variant265_0 ()

parse_from_74 :: () -> Closure266
parse_from_74 () =
  Variant266_0 ()

and_then_75 :: () -> Closure267
and_then_75 () =
  Variant267_0 ()

pure_76 :: () -> Closure268
pure_76 () =
  Variant268_0 ()

parse_from_77 :: () -> Closure269
parse_from_77 () =
  Variant269_0 ()

parse_from_78 :: () -> Closure270
parse_from_78 () =
  Variant270_0 ()

and_then_79 :: () -> Closure271
and_then_79 () =
  Variant271_0 ()

and_then_80 :: () -> Closure272
and_then_80 () =
  Variant272_0 ()

parse_from_81 :: () -> Closure273
parse_from_81 () =
  Variant273_0 ()

parse_from_82 :: () -> Closure274
parse_from_82 () =
  Variant274_0 ()

and_then_83 :: () -> Closure275
and_then_83 () =
  Variant275_0 ()

parse_from_84 :: () -> Closure276
parse_from_84 () =
  Variant276_0 ()

map_85 :: () -> Closure277
map_85 () =
  Variant277_0 ()

pure_86 :: () -> Closure278
pure_86 () =
  Variant278_0 ()

parse_from_87 :: () -> Closure279
parse_from_87 () =
  Variant279_0 ()

parse_from_88 :: () -> Closure280
parse_from_88 () =
  Variant280_0 ()

parse_from_89 :: () -> Closure281
parse_from_89 () =
  Variant281_0 ()

or_else_90 :: () -> Closure282
or_else_90 () =
  Variant282_0 ()

pure_91 :: () -> Closure283
pure_91 () =
  Variant283_0 ()

many0_fold_92 :: () -> Closure284
many0_fold_92 () =
  Variant284_0 ()

parse_from_93 :: () -> Closure285
parse_from_93 () =
  Variant285_0 ()

parse_from_94 :: () -> Closure286
parse_from_94 () =
  Variant286_0 ()

and_then_95 :: () -> Closure287
and_then_95 () =
  Variant287_0 ()

and_then_96 :: () -> Closure288
and_then_96 () =
  Variant288_0 ()

or_97 :: () -> Closure289
or_97 () =
  Variant289_0 ()

map_98 :: () -> Closure290
map_98 () =
  Variant290_0 ()

lazy_99 :: () -> Closure291
lazy_99 () =
  Variant291_0 ()

optional_100 :: () -> Closure292
optional_100 () =
  Variant292_0 ()

wrapped_many0_fold_101 :: () -> Closure293
wrapped_many0_fold_101 () =
  Variant293_0 ()

and_then_102 :: () -> Closure294
and_then_102 () =
  Variant294_0 ()

skip_pre_103 :: () -> Closure295
skip_pre_103 () =
  Variant295_0 ()

lazy_105 :: () -> Closure296
lazy_105 () =
  Variant296_0 ()

skip_many0_106 :: () -> Closure297
skip_many0_106 () =
  Variant297_0 ()

or_108 :: () -> Closure298
or_108 () =
  Variant298_0 ()

and_then_109 :: () -> Closure299
and_then_109 () =
  Variant299_0 ()

guard_110 :: () -> Closure300
guard_110 () =
  Variant300_0 ()

byte_111 :: () -> Parse13
byte_111 () =
  Parse13_0 (Variant97_0 ())

byte_eq_112 :: () -> Closure301
byte_eq_112 () =
  Variant301_0 ()

ascii_space_113 :: () -> Word8
ascii_space_113 () =
  32

or_114 :: () -> Closure302
or_114 () =
  Variant302_0 ()

ascii_newline_115 :: () -> Word8
ascii_newline_115 () =
  10

ascii_tab_116 :: () -> Word8
ascii_tab_116 () =
  9

map_117 :: () -> Closure303
map_117 () =
  Variant303_0 ()

skip_post_118 :: () -> Closure304
skip_post_118 () =
  Variant304_0 ()

spaced_119 :: () -> Closure305
spaced_119 () =
  Variant305_0 ()

and_then_120 :: () -> Closure306
and_then_120 () =
  Variant306_0 ()

map_121 :: () -> Closure307
map_121 () =
  Variant307_0 ()

map_122 :: () -> Closure308
map_122 () =
  Variant308_0 ()

parse_from_123 :: () -> Closure309
parse_from_123 () =
  Variant309_0 ()

parse_from_124 :: () -> Closure310
parse_from_124 () =
  Variant310_0 ()

or_else_125 :: () -> Closure311
or_else_125 () =
  Variant311_0 ()

parse_from_126 :: () -> Closure312
parse_from_126 () =
  Variant312_0 ()

map_127 :: () -> Closure313
map_127 () =
  Variant313_0 ()

and_then_128 :: () -> Closure314
and_then_128 () =
  Variant314_0 ()

parse_from_129 :: () -> Closure315
parse_from_129 () =
  Variant315_0 ()

and_then_130 :: () -> Closure316
and_then_130 () =
  Variant316_0 ()

pure_131 :: () -> Closure317
pure_131 () =
  Variant317_0 ()

parse_from_132 :: () -> Closure318
parse_from_132 () =
  Variant318_0 ()

parse_from_133 :: () -> Closure319
parse_from_133 () =
  Variant319_0 ()

and_then_134 :: () -> Closure320
and_then_134 () =
  Variant320_0 ()

and_then_135 :: () -> Closure321
and_then_135 () =
  Variant321_0 ()

parse_from_136 :: () -> Closure322
parse_from_136 () =
  Variant322_0 ()

parse_from_137 :: () -> Closure323
parse_from_137 () =
  Variant323_0 ()

and_then_138 :: () -> Closure324
and_then_138 () =
  Variant324_0 ()

parse_from_139 :: () -> Closure325
parse_from_139 () =
  Variant325_0 ()

map_140 :: () -> Closure326
map_140 () =
  Variant326_0 ()

pure_141 :: () -> Closure327
pure_141 () =
  Variant327_0 ()

parse_from_142 :: () -> Closure328
parse_from_142 () =
  Variant328_0 ()

parse_from_143 :: () -> Closure329
parse_from_143 () =
  Variant329_0 ()

parse_from_144 :: () -> Closure330
parse_from_144 () =
  Variant330_0 ()

or_else_145 :: () -> Closure331
or_else_145 () =
  Variant331_0 ()

pure_146 :: () -> Closure332
pure_146 () =
  Variant332_0 ()

many0_fold_147 :: () -> Closure333
many0_fold_147 () =
  Variant333_0 ()

parse_from_148 :: () -> Closure334
parse_from_148 () =
  Variant334_0 ()

parse_from_149 :: () -> Closure335
parse_from_149 () =
  Variant335_0 ()

and_then_150 :: () -> Closure336
and_then_150 () =
  Variant336_0 ()

and_then_151 :: () -> Closure337
and_then_151 () =
  Variant337_0 ()

or_152 :: () -> Closure338
or_152 () =
  Variant338_0 ()

map_153 :: () -> Closure339
map_153 () =
  Variant339_0 ()

lazy_154 :: () -> Closure340
lazy_154 () =
  Variant340_0 ()

optional_155 :: () -> Closure341
optional_155 () =
  Variant341_0 ()

wrapped_many0_fold_156 :: () -> Closure342
wrapped_many0_fold_156 () =
  Variant342_0 ()

and_then_157 :: () -> Closure343
and_then_157 () =
  Variant343_0 ()

skip_pre_158 :: () -> Closure344
skip_pre_158 () =
  Variant344_0 ()

skip_post_159 :: () -> Closure345
skip_post_159 () =
  Variant345_0 ()

spaced_160 :: () -> Closure346
spaced_160 () =
  Variant346_0 ()

and_then_161 :: () -> Closure347
and_then_161 () =
  Variant347_0 ()

and_then_162 :: () -> Closure348
and_then_162 () =
  Variant348_0 ()

ascii_zero_163 :: () -> Word8
ascii_zero_163 () =
  48

parse_from_164 :: () -> Closure349
parse_from_164 () =
  Variant349_0 ()

map_165 :: () -> Closure350
map_165 () =
  Variant350_0 ()

parse_from_166 :: () -> Closure351
parse_from_166 () =
  Variant351_0 ()

map_167 :: () -> Closure352
map_167 () =
  Variant352_0 ()

pure_168 :: () -> Closure353
pure_168 () =
  Variant353_0 ()

parse_from_169 :: () -> Closure354
parse_from_169 () =
  Variant354_0 ()

parse_from_170 :: () -> Closure355
parse_from_170 () =
  Variant355_0 ()

parse_from_171 :: () -> Closure356
parse_from_171 () =
  Variant356_0 ()

or_else_172 :: () -> Closure357
or_else_172 () =
  Variant357_0 ()

pure_173 :: () -> Closure358
pure_173 () =
  Variant358_0 ()

many0_fold_174 :: () -> Closure359
many0_fold_174 () =
  Variant359_0 ()

parse_from_175 :: () -> Closure360
parse_from_175 () =
  Variant360_0 ()

parse_from_176 :: () -> Closure361
parse_from_176 () =
  Variant361_0 ()

and_then_177 :: () -> Closure362
and_then_177 () =
  Variant362_0 ()

and_then_178 :: () -> Closure363
and_then_178 () =
  Variant363_0 ()

or_179 :: () -> Closure364
or_179 () =
  Variant364_0 ()

map_180 :: () -> Closure365
map_180 () =
  Variant365_0 ()

lazy_181 :: () -> Closure366
lazy_181 () =
  Variant366_0 ()

optional_182 :: () -> Closure367
optional_182 () =
  Variant367_0 ()

wrapped_many0_fold_183 :: () -> Closure368
wrapped_many0_fold_183 () =
  Variant368_0 ()

and_then_184 :: () -> Closure369
and_then_184 () =
  Variant369_0 ()

parse_from_185 :: () -> Closure370
parse_from_185 () =
  Variant370_0 ()

map_186 :: () -> Closure371
map_186 () =
  Variant371_0 ()

skip_pre_187 :: () -> Closure372
skip_pre_187 () =
  Variant372_0 ()

ascii_open_paren_188 :: () -> Word8
ascii_open_paren_188 () =
  40

skip_post_189 :: () -> Closure373
skip_post_189 () =
  Variant373_0 ()

skip_pre_190 :: () -> Closure374
skip_pre_190 () =
  Variant374_0 ()

skip_post_191 :: () -> Closure375
skip_post_191 () =
  Variant375_0 ()

spaced_192 :: () -> Closure376
spaced_192 () =
  Variant376_0 ()

parse_from_194 :: () -> Closure377
parse_from_194 () =
  Variant377_0 ()

parse_from_195 :: () -> Closure378
parse_from_195 () =
  Variant378_0 ()

and_then_196 :: () -> Closure379
and_then_196 () =
  Variant379_0 ()

skip_pre_197 :: () -> Closure380
skip_pre_197 () =
  Variant380_0 ()

skip_post_198 :: () -> Closure381
skip_post_198 () =
  Variant381_0 ()

spaced_199 :: () -> Closure382
spaced_199 () =
  Variant382_0 ()

and_then_201 :: () -> Closure383
and_then_201 () =
  Variant383_0 ()

operator_chain_202 :: () -> Closure384
operator_chain_202 () =
  Variant384_0 ()

and_then_204 :: () -> Closure385
and_then_204 () =
  Variant385_0 ()

operator_chain_205 :: () -> Closure386
operator_chain_205 () =
  Variant386_0 ()

parse_from_207 :: () -> Closure387
parse_from_207 () =
  Variant387_0 ()

parse_from_208 :: () -> Closure388
parse_from_208 () =
  Variant388_0 ()

or_else_209 :: () -> Closure389
or_else_209 () =
  Variant389_0 ()

or_210 :: () -> Closure390
or_210 () =
  Variant390_0 ()

map_211 :: () -> Closure391
map_211 () =
  Variant391_0 ()

and_then_213 :: () -> Closure392
and_then_213 () =
  Variant392_0 ()

many1_fold_214 :: () -> Closure393
many1_fold_214 () =
  Variant393_0 ()

and_then_216 :: () -> Closure394
and_then_216 () =
  Variant394_0 ()

guard_217 :: () -> Closure395
guard_217 () =
  Variant395_0 ()

byte_range_218 :: () -> Closure396
byte_range_218 () =
  Variant396_0 ()

ascii_nine_219 :: () -> Word8
ascii_nine_219 () =
  57

map_220 :: () -> Closure397
map_220 () =
  Variant397_0 ()

parse_from_221 :: () -> Closure398
parse_from_221 () =
  Variant398_0 ()

lazy_222 :: () -> Closure399
lazy_222 () =
  Variant399_0 ()

or_224 :: () -> Closure400
or_224 () =
  Variant400_0 ()

ascii_asterisk_225 :: () -> Word8
ascii_asterisk_225 () =
  42

map_226 :: () -> Closure401
map_226 () =
  Variant401_0 ()

ascii_slash_227 :: () -> Word8
ascii_slash_227 () =
  47

map_228 :: () -> Closure402
map_228 () =
  Variant402_0 ()

or_230 :: () -> Closure403
or_230 () =
  Variant403_0 ()

ascii_plus_231 :: () -> Word8
ascii_plus_231 () =
  43

map_232 :: () -> Closure404
map_232 () =
  Variant404_0 ()

ascii_minus_233 :: () -> Word8
ascii_minus_233 () =
  45

map_234 :: () -> Closure405
map_234 () =
  Variant405_0 ()

ascii_close_paren_235 :: () -> Word8
ascii_close_paren_235 () =
  41

parse_prefix_236 :: () -> Closure406
parse_prefix_236 () =
  Variant406_0 ()

and_then_237 :: () -> Closure407
and_then_237 () =
  Variant407_0 ()

parse_all_238 :: () -> Closure408
parse_all_238 () =
  Variant408_0 ()

unwrap_239 :: () -> Closure409
unwrap_239 () =
  Variant409_0 ()

eval_240 :: () -> Closure410
eval_240 () =
  Variant410_0 ()

wrapped_eval_241 :: () -> Closure411
wrapped_eval_241 () =
  Variant411_0 ()

next_242 :: () -> Closure412
next_242 () =
  Variant412_0 ()

map_243 :: () -> Closure413
map_243 () =
  Variant413_0 ()

wrapped_map_244 :: () -> Closure414
wrapped_map_244 () =
  Variant414_0 ()

next_245 :: () -> Closure415
next_245 () =
  Variant415_0 ()

foldl_246 :: () -> Closure416
foldl_246 () =
  Variant416_0 ()

wrapped_foldl_247 :: () -> Closure417
wrapped_foldl_247 () =
  Variant417_0 ()

push__249 :: () -> Closure418
push__249 () =
  Variant418_0

push_248 :: () -> Closure418
push_248 () =
  push__249 ()

from_iter_with_capacity_250 :: () -> Closure419
from_iter_with_capacity_250 () =
  Variant419_0 ()

from_iter_251 :: () -> Closure420
from_iter_251 () =
  Variant420_0 ()

eval_exprs_252 :: () -> Closure421
eval_exprs_252 () =
  Variant421_0 ()

wrapped_map_253 :: () -> Closure422
wrapped_map_253 () =
  Variant422_0 ()

wrapped_range_254 :: () -> Closure423
wrapped_range_254 () =
  Variant423_0 ()

items_255 :: () -> Closure424
items_255 () =
  Variant424_0 ()

repeat_256 :: () -> Closure425
repeat_256 () =
  Variant425_0 ()

wrapped_repeat_257 :: () -> Closure426
wrapped_repeat_257 () =
  Variant426_0 ()

range_258 :: () -> Closure427
range_258 () =
  Variant427_0 ()

next_259 :: () -> Closure428
next_259 () =
  Variant428_0 ()

map_260 :: () -> Closure429
map_260 () =
  Variant429_0 ()

wrapped_map_261 :: () -> Closure430
wrapped_map_261 () =
  Variant430_0 ()

wrapped_range_262 :: () -> Closure431
wrapped_range_262 () =
  Variant431_0 ()

items_263 :: () -> Closure432
items_263 () =
  Variant432_0 ()

writeln_264 :: () -> Closure433
writeln_264 () =
  Variant433_0 ()

concat_from_265 :: () -> Closure434
concat_from_265 () =
  Variant434_0 ()

push__267 :: () -> Closure435
push__267 () =
  Variant435_0

push_266 :: () -> Closure435
push_266 () =
  push__267 ()

wrapped_concat_from_268 :: () -> Closure436
wrapped_concat_from_268 () =
  Variant436_0 ()

concat_269 :: () -> Closure437
concat_269 () =
  Variant437_0 ()

nat_to_string_270 :: () -> Closure438
nat_to_string_270 () =
  Variant438_0 ()

wrapped_nat_to_string_271 :: () -> Closure439
wrapped_nat_to_string_271 () =
  Variant439_0 ()

int_to_string_272 :: () -> Closure440
int_to_string_272 () =
  Variant440_0 ()

next_273 :: () -> Closure441
next_273 () =
  Variant441_0 ()

for_each_274 :: () -> Closure442
for_each_274 () =
  Variant442_0 ()

wrapped_for_each_275 :: () -> Closure443
wrapped_for_each_275 () =
  Variant443_0 ()

main_276 :: () -> Closure444
main_276 () =
  Variant444_0 ()

lam_range_279 :: (Int64, Int64) -> Iter1
lam_range_279 (l0, l1) =
  Iter1_0 (Variant90_0 (l0, l1))

lam_range_280 :: ((Int64, Int64), ()) -> Option2
lam_range_280 ((l0, l1), ()) =
  case uncurry (<) (l0, l1) of True -> Some2_0 (l0, lam_range_279 (uncurry (+) (l0, 1), l1)); False -> None2_1

lam_items_281 :: ((Vector Word8) , Int64) -> Word8
lam_items_281 (l0, l1) =
  intrinsicGet l0 l1

lam_map_282 :: (Iter1, Closure92) -> Iter3
lam_map_282 (l0, l1) =
  Iter3_0 (Variant91_0 (l0, l1))

lam_chars_to_nat_289 :: (Int64, Int64) -> Int64
lam_chars_to_nat_289 (l0, l1) =
  uncurry (+) (uncurry (*) (l0, 10), l1)

lam_digit_to_nat_291 :: Word8 -> Option0
lam_digit_to_nat_291 l0 =
  case l0 of 48 -> Some0_0 0; 49 -> Some0_0 1; 50 -> Some0_0 2; 51 -> Some0_0 3; 52 -> Some0_0 4; 53 -> Some0_0 5; 54 -> Some0_0 6; 55 -> Some0_0 7; 56 -> Some0_0 8; 57 -> Some0_0 9; l_0 -> None0_1

lam_wrapped_map_296 :: (Iter1, Closure92) -> Iter3
lam_wrapped_map_296 l0 =
  lam_map_282 l0

lam_wrapped_range_297 :: (Int64, Int64) -> Iter1
lam_wrapped_range_297 l0 =
  lam_range_279 l0

lam_items_295 :: (Vector Word8)  -> Iter3
lam_items_295 l0 =
  lam_wrapped_map_296 (lam_wrapped_range_297 (0, intrinsicLen l0), Variant92_0 l0)

lam_read_input_rec_300 :: (Vector ((Vector Word8) ))  -> (Vector ((Vector Word8) )) 
lam_read_input_rec_300 l0 =
  let l1 = input () in (case uncurry (==) (intrinsicLen l1, 0) of True -> l0; False -> lam_read_input_rec_300 (intrinsicPush l0 l1))

lam_wrapped_read_input_rec_299 :: (Vector ((Vector Word8) ))  -> (Vector ((Vector Word8) )) 
lam_wrapped_read_input_rec_299 l0 =
  lam_read_input_rec_300 l0

lam_read_input_298 :: () -> (Vector ((Vector Word8) )) 
lam_read_input_298 () =
  lam_wrapped_read_input_rec_299 ((V.fromList []))

lam_range_302 :: (Int64, Int64) -> Iter5
lam_range_302 (l0, l1) =
  Iter5_0 (Variant93_0 (l0, l1))

lam_range_303 :: ((Int64, Int64), ()) -> Option6
lam_range_303 ((l0, l1), ()) =
  case uncurry (<) (l0, l1) of True -> Some6_0 (l0, lam_range_302 (uncurry (+) (l0, 1), l1)); False -> None6_1

lam_items_304 :: ((Vector ((Vector Word8) )) , Int64) -> (Vector Word8) 
lam_items_304 (l0, l1) =
  intrinsicGet l0 l1

lam_map_305 :: (Iter5, Closure95) -> Iter7
lam_map_305 (l0, l1) =
  Iter7_0 (Variant94_0 (l0, l1))

lam_byte_310 :: (Int64, (Vector Word8) ) -> Option11
lam_byte_310 (l0, l1) =
  case uncurry (<) (l0, intrinsicLen l1) of True -> Some11_0 (uncurry (+) (l0, 1), intrinsicGet l1 l0); False -> None11_1

lam_byte_eq_311 :: (Word8, Word8) -> Bool
lam_byte_eq_311 (l0, l1) =
  uncurry (==) (l1, l0)

lam_pure_312 :: (Word8, (Int64, (Vector Word8) )) -> Option11
lam_pure_312 (l0, (l1, l2)) =
  Some11_0 (l1, l0)

lam_fail_313 :: (Int64, (Vector Word8) ) -> Option11
lam_fail_313 (l_0, l_1) =
  None11_1

lam_pure_315 :: Word8 -> Parse12
lam_pure_315 l0 =
  Parse12_0 (Variant96_0 l0)

lam_parse_from_317 :: (Int64, (Vector Word8) , Parse13) -> Option11
lam_parse_from_317 (l0, l1, l2) =
  let Parse13_0 l3 = l2 in lam_byte_310 (l0, l1)

lam_space_329 :: Word8 -> ()
lam_space_329 l_0 =
  ()

lam_map_332 :: (Closure104, (Int64, Word8)) -> (Int64, ())
lam_map_332 (l0, (l1, l2)) =
  (l1, lam_space_329 l2)

lam_skip_many0_334 :: ((), ()) -> ()
lam_skip_many0_334 (l_1, l_2) =
  ()

lam_optional_336 :: () -> Option20
lam_optional_336 l0 =
  Some20_0 l0

lam_map_339 :: (Closure110, (Int64, ())) -> (Int64, Option20)
lam_map_339 (l0, (l1, l2)) =
  (l1, lam_optional_336 l2)

lam_pure_341 :: (Option20, (Int64, (Vector Word8) )) -> Option21
lam_pure_341 (l0, (l1, l2)) =
  Some21_0 (l1, l0)

lam_pure_343 :: Option20 -> Parse22
lam_pure_343 l0 =
  Parse22_0 (Variant108_0 l0)

lam_optional_342 :: () -> Parse22
lam_optional_342 () =
  lam_pure_343 (None20_1)

lam_pure_352 :: () -> Parse19
lam_pure_352 l0 =
  Parse19_0 (Variant105_0 l0)

lam_pure_353 :: ((), (Int64, (Vector Word8) )) -> Option17
lam_pure_353 (l0, (l1, l2)) =
  Some17_0 (l1, l0)

lam_and_then_354 :: (Parse25, Closure106) -> Parse19
lam_and_then_354 (l0, l1) =
  Parse19_0 (Variant105_1 (l0, l1))

lam_or_361 :: (Parse23, Parse24) -> Parse25
lam_or_361 (l0, l1) =
  Parse25_0 (Variant113_0 (l0, l1))

lam_map_362 :: (Parse18, Closure110) -> Parse23
lam_map_362 (l0, l1) =
  Parse23_0 (Variant109_0 (l0, l1))

lam_lazy_363 :: Closure112 -> Parse24
lam_lazy_363 l0 =
  Parse24_0 (Variant111_0 l0)

lam_optional_360 :: Parse18 -> Parse25
lam_optional_360 l0 =
  lam_or_361 (lam_map_362 (l0, Variant110_0 ()), lam_lazy_363 (Variant112_0 ()))

lam_many0_fold_335 :: (Parse18, (), Closure107) -> Parse19
lam_many0_fold_335 (l0, l1, l2) =
  lam_and_then_354 (lam_optional_360 l0, Variant106_0 (l1, l0, l2))

lam_many0_fold_351 :: (((), Parse18, Closure107), Option20) -> Parse19
lam_many0_fold_351 ((l0, l1, l2), l3) =
  case l3 of None20_1 -> lam_pure_352 l0; Some20_0 l4 -> lam_many0_fold_335 (l1, lam_skip_many0_334 (l0, l4), l2)

lam_wrapped_many0_fold_365 :: (Parse18, (), Closure107) -> Parse19
lam_wrapped_many0_fold_365 l0 =
  lam_many0_fold_335 l0

lam_skip_many0_364 :: (Parse18, ()) -> Parse19
lam_skip_many0_364 (l0, ()) =
  lam_wrapped_many0_fold_365 (l0, (), Variant107_0 ())

lam_skip_post_371 :: (Expr9, (Int64, ())) -> (Int64, Expr9)
lam_skip_post_371 (l0, (l1, l_3)) =
  (l1, l0)

lam_skip_post_381 :: (Expr9, (Int64, Word8)) -> (Int64, Expr9)
lam_skip_post_381 (l0, (l1, l_4)) =
  (l1, l0)

lam_mul_level_operator_388 :: (Expr9, Expr9) -> Expr9
lam_mul_level_operator_388 l0 =
  Mul9_3 l0

lam_mul_level_operator_389 :: (Expr9, Expr9) -> Expr9
lam_mul_level_operator_389 l0 =
  Div9_4 l0

lam_mul_level_operator_390 :: Word8 -> Closure120
lam_mul_level_operator_390 l_5 =
  Variant120_0 ()

lam_map_392 :: (Closure122, (Int64, Word8)) -> (Int64, Closure120)
lam_map_392 (l0, (l1, l2)) =
  (l1, lam_mul_level_operator_390 l2)

lam_mul_level_operator_394 :: Word8 -> Closure120
lam_mul_level_operator_394 l_6 =
  Variant120_1 ()

lam_map_396 :: (Closure124, (Int64, Word8)) -> (Int64, Closure120)
lam_map_396 (l0, (l1, l2)) =
  (l1, lam_mul_level_operator_394 l2)

lam_skip_post_406 :: (Closure120, (Int64, ())) -> (Int64, Closure120)
lam_skip_post_406 (l0, (l1, l_7)) =
  (l1, l0)

lam_pure_413 :: ((Closure120, Expr9), (Int64, (Vector Word8) )) -> Option37
lam_pure_413 (l0, (l1, l2)) =
  Some37_0 (l1, l0)

lam_pure_415 :: (Closure120, Expr9) -> Parse38
lam_pure_415 l0 =
  Parse38_0 (Variant127_0 l0)

lam_operator_chain_414 :: (Closure120, Expr9) -> Parse38
lam_operator_chain_414 (l0, l1) =
  lam_pure_415 (l0, l1)

lam_and_then_422 :: (Parse39, Closure130) -> Parse40
lam_and_then_422 (l0, l1) =
  Parse40_0 (Variant129_0 (l0, l1))

lam_operator_chain_421 :: (Parse39, Closure120) -> Parse40
lam_operator_chain_421 (l0, l1) =
  lam_and_then_422 (l0, Variant130_0 l1)

lam_optional_430 :: (Closure120, Expr9) -> Option44
lam_optional_430 l0 =
  Some44_0 l0

lam_map_433 :: (Closure139, (Int64, (Closure120, Expr9))) -> (Int64, Option44)
lam_map_433 (l0, (l1, l2)) =
  (l1, lam_optional_430 l2)

lam_pure_435 :: (Option44, (Int64, (Vector Word8) )) -> Option45
lam_pure_435 (l0, (l1, l2)) =
  Some45_0 (l1, l0)

lam_pure_437 :: Option44 -> Parse46
lam_pure_437 l0 =
  Parse46_0 (Variant137_0 l0)

lam_optional_436 :: () -> Parse46
lam_optional_436 () =
  lam_pure_437 (None44_1)

lam_pure_446 :: Expr9 -> Parse43
lam_pure_446 l0 =
  Parse43_0 (Variant134_0 l0)

lam_pure_447 :: (Expr9, (Int64, (Vector Word8) )) -> Option28
lam_pure_447 (l0, (l1, l2)) =
  Some28_0 (l1, l0)

lam_and_then_448 :: (Parse49, Closure135) -> Parse43
lam_and_then_448 (l0, l1) =
  Parse43_0 (Variant134_1 (l0, l1))

lam_or_455 :: (Parse47, Parse48) -> Parse49
lam_or_455 (l0, l1) =
  Parse49_0 (Variant142_0 (l0, l1))

lam_map_456 :: (Parse42, Closure139) -> Parse47
lam_map_456 (l0, l1) =
  Parse47_0 (Variant138_0 (l0, l1))

lam_lazy_457 :: Closure141 -> Parse48
lam_lazy_457 l0 =
  Parse48_0 (Variant140_0 l0)

lam_optional_454 :: Parse42 -> Parse49
lam_optional_454 l0 =
  lam_or_455 (lam_map_456 (l0, Variant139_0 ()), lam_lazy_457 (Variant141_0 ()))

lam_many0_fold_429 :: (Parse42, Expr9, Closure136) -> Parse43
lam_many0_fold_429 (l0, l1, l2) =
  lam_and_then_448 (lam_optional_454 l0, Variant135_0 (l1, l0, l2))

lam_wrapped_many0_fold_459 :: (Parse42, Expr9, Closure136) -> Parse43
lam_wrapped_many0_fold_459 l0 =
  lam_many0_fold_429 l0

lam_and_then_460 :: (Parse41, Closure133) -> Parse42
lam_and_then_460 (l0, l1) =
  Parse42_0 (Variant132_0 (l0, l1))

lam_skip_pre_462 :: (Parse27, Parse36) -> Parse41
lam_skip_pre_462 (l0, l1) =
  Parse41_0 (Variant131_0 (l0, l1))

lam_lazy_464 :: Closure116 -> Parse27
lam_lazy_464 l0 =
  Parse27_0 (Variant115_0 l0)

lam_skip_many0_463 :: Parse18 -> Parse27
lam_skip_many0_463 l0 =
  lam_lazy_464 (Variant116_0 l0)

lam_or_465 :: (Parse14, Parse15) -> Parse16
lam_or_465 (l0, l1) =
  Parse16_0 (Variant102_0 (l0, l1))

lam_and_then_468 :: (Parse13, Closure99) -> Parse14
lam_and_then_468 (l0, l1) =
  Parse14_0 (Variant98_0 (l0, l1))

lam_guard_467 :: (Parse13, Closure100) -> Parse14
lam_guard_467 (l0, l1) =
  lam_and_then_468 (l0, Variant99_0 l1)

lam_byte_eq_466 :: Word8 -> Parse14
lam_byte_eq_466 l0 =
  lam_guard_467 (byte_111 (), Variant100_0 l0)

lam_or_469 :: (Parse14, Parse14) -> Parse15
lam_or_469 (l0, l1) =
  Parse15_0 (Variant101_0 (l0, l1))

lam_map_470 :: (Parse16, Closure104) -> Parse18
lam_map_470 (l0, l1) =
  Parse18_0 (Variant103_0 (l0, l1))

space_107 :: () -> Parse18
space_107 () =
  let l0 = lam_or_465 (lam_byte_eq_466 (ascii_space_113 ()), lam_or_469 (lam_byte_eq_466 (ascii_newline_115 ()), lam_byte_eq_466 (ascii_tab_116 ()))) in lam_map_470 (l0, Variant104_0 ())

spaces_104 :: () -> Parse27
spaces_104 () =
  lam_skip_many0_463 (space_107 ())

lam_skip_post_471 :: (Parse35, Parse27) -> Parse36
lam_skip_post_471 (l0, l1) =
  Parse36_0 (Variant126_0 (l0, l1))

lam_spaced_461 :: Parse35 -> Parse41
lam_spaced_461 l0 =
  lam_skip_pre_462 (spaces_104 (), lam_skip_post_471 (l0, spaces_104 ()))

lam_operator_chain_458 :: ((Parse35, Parse39), Expr9) -> Parse43
lam_operator_chain_458 ((l0, l1), l2) =
  lam_wrapped_many0_fold_459 (lam_and_then_460 (lam_spaced_461 l0, Variant133_0 l1), l2, Variant136_0 ())

lam_add_level_operator_475 :: (Expr9, Expr9) -> Expr9
lam_add_level_operator_475 l0 =
  Add9_1 l0

lam_add_level_operator_476 :: (Expr9, Expr9) -> Expr9
lam_add_level_operator_476 l0 =
  Sub9_2 l0

lam_add_level_operator_477 :: Word8 -> Closure143
lam_add_level_operator_477 l_0 =
  Variant143_0 ()

lam_map_479 :: (Closure145, (Int64, Word8)) -> (Int64, Closure143)
lam_map_479 (l0, (l1, l2)) =
  (l1, lam_add_level_operator_477 l2)

lam_add_level_operator_481 :: Word8 -> Closure143
lam_add_level_operator_481 l_1 =
  Variant143_1 ()

lam_map_483 :: (Closure147, (Int64, Word8)) -> (Int64, Closure143)
lam_map_483 (l0, (l1, l2)) =
  (l1, lam_add_level_operator_481 l2)

lam_skip_post_493 :: (Closure143, (Int64, ())) -> (Int64, Closure143)
lam_skip_post_493 (l0, (l1, l_2)) =
  (l1, l0)

lam_pure_500 :: ((Closure143, Expr9), (Int64, (Vector Word8) )) -> Option55
lam_pure_500 (l0, (l1, l2)) =
  Some55_0 (l1, l0)

lam_pure_502 :: (Closure143, Expr9) -> Parse56
lam_pure_502 l0 =
  Parse56_0 (Variant150_0 l0)

lam_operator_chain_501 :: (Closure143, Expr9) -> Parse56
lam_operator_chain_501 (l0, l1) =
  lam_pure_502 (l0, l1)

lam_and_then_509 :: (Parse57, Closure154) -> Parse58
lam_and_then_509 (l0, l1) =
  Parse58_0 (Variant153_0 (l0, l1))

lam_operator_chain_508 :: (Parse57, Closure143) -> Parse58
lam_operator_chain_508 (l0, l1) =
  lam_and_then_509 (l0, Variant154_0 l1)

lam_optional_517 :: (Closure143, Expr9) -> Option62
lam_optional_517 l0 =
  Some62_0 l0

lam_map_520 :: (Closure163, (Int64, (Closure143, Expr9))) -> (Int64, Option62)
lam_map_520 (l0, (l1, l2)) =
  (l1, lam_optional_517 l2)

lam_pure_522 :: (Option62, (Int64, (Vector Word8) )) -> Option63
lam_pure_522 (l0, (l1, l2)) =
  Some63_0 (l1, l0)

lam_pure_524 :: Option62 -> Parse64
lam_pure_524 l0 =
  Parse64_0 (Variant161_0 l0)

lam_optional_523 :: () -> Parse64
lam_optional_523 () =
  lam_pure_524 (None62_1)

lam_pure_533 :: Expr9 -> Parse61
lam_pure_533 l0 =
  Parse61_0 (Variant158_0 l0)

lam_and_then_534 :: (Parse67, Closure159) -> Parse61
lam_and_then_534 (l0, l1) =
  Parse61_0 (Variant158_1 (l0, l1))

lam_or_541 :: (Parse65, Parse66) -> Parse67
lam_or_541 (l0, l1) =
  Parse67_0 (Variant166_0 (l0, l1))

lam_map_542 :: (Parse60, Closure163) -> Parse65
lam_map_542 (l0, l1) =
  Parse65_0 (Variant162_0 (l0, l1))

lam_lazy_543 :: Closure165 -> Parse66
lam_lazy_543 l0 =
  Parse66_0 (Variant164_0 l0)

lam_optional_540 :: Parse60 -> Parse67
lam_optional_540 l0 =
  lam_or_541 (lam_map_542 (l0, Variant163_0 ()), lam_lazy_543 (Variant165_0 ()))

lam_many0_fold_516 :: (Parse60, Expr9, Closure160) -> Parse61
lam_many0_fold_516 (l0, l1, l2) =
  lam_and_then_534 (lam_optional_540 l0, Variant159_0 (l1, l0, l2))

lam_wrapped_many0_fold_545 :: (Parse60, Expr9, Closure160) -> Parse61
lam_wrapped_many0_fold_545 l0 =
  lam_many0_fold_516 l0

lam_and_then_546 :: (Parse59, Closure157) -> Parse60
lam_and_then_546 (l0, l1) =
  Parse60_0 (Variant156_0 (l0, l1))

lam_skip_pre_548 :: (Parse27, Parse54) -> Parse59
lam_skip_pre_548 (l0, l1) =
  Parse59_0 (Variant155_0 (l0, l1))

lam_skip_post_549 :: (Parse53, Parse27) -> Parse54
lam_skip_post_549 (l0, l1) =
  Parse54_0 (Variant149_0 (l0, l1))

lam_spaced_547 :: Parse53 -> Parse59
lam_spaced_547 l0 =
  lam_skip_pre_548 (spaces_104 (), lam_skip_post_549 (l0, spaces_104 ()))

lam_operator_chain_544 :: ((Parse53, Parse57), Expr9) -> Parse61
lam_operator_chain_544 ((l0, l1), l2) =
  lam_wrapped_many0_fold_545 (lam_and_then_546 (lam_spaced_547 l0, Variant157_0 l1), l2, Variant160_0 ())

lam_byte_range_553 :: ((Word8, Word8), Word8) -> Bool
lam_byte_range_553 ((l0, l1), l2) =
  case uncurry (<=) (l0, l2) of False -> False; True -> uncurry (<=) (l2, l1)

lam_digit_558 :: Word8 -> Int64
lam_digit_558 l0 =
  fromIntegral (uncurry (-) (l0, ascii_zero_163 ()))

lam_map_561 :: (Closure171, (Int64, Word8)) -> (Int64, Int64)
lam_map_561 (l0, (l1, l2)) =
  (l1, lam_digit_558 l2)

lam_int_563 :: (Int64, Int64) -> Int64
lam_int_563 (l0, l1) =
  uncurry (+) (uncurry (*) (l0, 10), l1)

lam_optional_565 :: Int64 -> Option0
lam_optional_565 l0 =
  Some0_0 l0

lam_map_568 :: (Closure177, (Int64, Int64)) -> (Int64, Option0)
lam_map_568 (l0, (l1, l2)) =
  (l1, lam_optional_565 l2)

lam_pure_570 :: (Option0, (Int64, (Vector Word8) )) -> Option72
lam_pure_570 (l0, (l1, l2)) =
  Some72_0 (l1, l0)

lam_pure_572 :: Option0 -> Parse73
lam_pure_572 l0 =
  Parse73_0 (Variant175_0 l0)

lam_optional_571 :: () -> Parse73
lam_optional_571 () =
  lam_pure_572 (None0_1)

lam_pure_581 :: Int64 -> Parse71
lam_pure_581 l0 =
  Parse71_0 (Variant172_0 l0)

lam_pure_582 :: (Int64, (Int64, (Vector Word8) )) -> Option69
lam_pure_582 (l0, (l1, l2)) =
  Some69_0 (l1, l0)

lam_and_then_583 :: (Parse76, Closure173) -> Parse71
lam_and_then_583 (l0, l1) =
  Parse71_0 (Variant172_1 (l0, l1))

lam_or_590 :: (Parse74, Parse75) -> Parse76
lam_or_590 (l0, l1) =
  Parse76_0 (Variant180_0 (l0, l1))

lam_map_591 :: (Parse70, Closure177) -> Parse74
lam_map_591 (l0, l1) =
  Parse74_0 (Variant176_0 (l0, l1))

lam_lazy_592 :: Closure179 -> Parse75
lam_lazy_592 l0 =
  Parse75_0 (Variant178_0 l0)

lam_optional_589 :: Parse70 -> Parse76
lam_optional_589 l0 =
  lam_or_590 (lam_map_591 (l0, Variant177_0 ()), lam_lazy_592 (Variant179_0 ()))

lam_many0_fold_564 :: (Parse70, Int64, Closure174) -> Parse71
lam_many0_fold_564 (l0, l1, l2) =
  lam_and_then_583 (lam_optional_589 l0, Variant173_0 (l1, l0, l2))

lam_many0_fold_580 :: ((Int64, Parse70, Closure174), Option0) -> Parse71
lam_many0_fold_580 ((l0, l1, l2), l3) =
  case l3 of None0_1 -> lam_pure_581 l0; Some0_0 l4 -> lam_many0_fold_564 (l1, lam_int_563 (l0, l4), l2)

lam_wrapped_many0_fold_594 :: (Parse70, Int64, Closure174) -> Parse71
lam_wrapped_many0_fold_594 l0 =
  lam_many0_fold_564 l0

lam_many1_fold_593 :: ((Parse70, Closure174, Int64), Int64) -> Parse71
lam_many1_fold_593 ((l0, l1, l2), l3) =
  lam_wrapped_many0_fold_594 (l0, lam_int_563 (l2, l3), l1)

lam_atomic_expr_598 :: Int64 -> Expr9
lam_atomic_expr_598 l0 =
  Const9_0 l0

lam_map_601 :: (Closure188, (Int64, Int64)) -> (Int64, Expr9)
lam_map_601 (l0, (l1, l2)) =
  (l1, lam_atomic_expr_598 l2)

lam_skip_pre_604 :: (Parse14, Parse31) -> Parse78
lam_skip_pre_604 (l0, l1) =
  Parse78_0 (Variant183_0 (l0, l1))

lam_skip_post_605 :: (Parse30, Parse14) -> Parse31
lam_skip_post_605 (l0, l1) =
  Parse31_0 (Variant119_0 (l0, l1))

lam_skip_pre_607 :: (Parse27, Parse29) -> Parse30
lam_skip_pre_607 (l0, l1) =
  Parse30_0 (Variant118_0 (l0, l1))

lam_skip_post_608 :: (Parse26, Parse27) -> Parse29
lam_skip_post_608 (l0, l1) =
  Parse29_0 (Variant117_0 (l0, l1))

lam_spaced_606 :: Parse26 -> Parse30
lam_spaced_606 l0 =
  lam_skip_pre_607 (spaces_104 (), lam_skip_post_608 (l0, spaces_104 ()))

lam_skip_pre_612 :: (Parse27, Parse80) -> Parse26
lam_skip_pre_612 (l0, l1) =
  Parse26_0 (Variant114_0 (l0, l1))

lam_skip_post_617 :: (Parse79, Parse27) -> Parse80
lam_skip_post_617 (l0, l1) =
  Parse80_0 (Variant186_0 (l0, l1))

lam_spaced_609 :: Parse79 -> Parse26
lam_spaced_609 l0 =
  lam_skip_pre_612 (spaces_104 (), lam_skip_post_617 (l0, spaces_104 ()))

lam_and_then_619 :: (Parse57, Closure185) -> Parse79
lam_and_then_619 (l0, l1) =
  Parse79_0 (Variant184_0 (l0, l1))

lam_operator_chain_618 :: (Parse57, Parse53) -> Parse79
lam_operator_chain_618 (l0, l1) =
  lam_and_then_619 (l0, Variant185_0 (l1, l0))

lam_and_then_621 :: (Parse39, Closure152) -> Parse57
lam_and_then_621 (l0, l1) =
  Parse57_0 (Variant151_0 (l0, l1))

lam_operator_chain_620 :: (Parse39, Parse35) -> Parse57
lam_operator_chain_620 (l0, l1) =
  lam_and_then_621 (l0, Variant152_0 (l1, l0))

lam_or_622 :: (Parse81, Parse82) -> Parse39
lam_or_622 (l0, l1) =
  Parse39_0 (Variant128_0 (l0, l1))

lam_map_628 :: (Parse77, Closure188) -> Parse81
lam_map_628 (l0, l1) =
  Parse81_0 (Variant187_0 (l0, l1))

lam_and_then_630 :: (Parse70, Closure182) -> Parse77
lam_and_then_630 (l0, l1) =
  Parse77_0 (Variant181_0 (l0, l1))

lam_many1_fold_629 :: (Parse70, Int64, Closure174) -> Parse77
lam_many1_fold_629 (l0, l1, l2) =
  lam_and_then_630 (l0, Variant182_0 (l0, l2, l1))

lam_and_then_633 :: (Parse13, Closure168) -> Parse68
lam_and_then_633 (l0, l1) =
  Parse68_0 (Variant167_0 (l0, l1))

lam_guard_632 :: (Parse13, Closure169) -> Parse68
lam_guard_632 (l0, l1) =
  lam_and_then_633 (l0, Variant168_0 l1)

lam_byte_range_631 :: (Word8, Word8) -> Parse68
lam_byte_range_631 (l0, l1) =
  lam_guard_632 (byte_111 (), Variant169_0 (l0, l1))

lam_map_634 :: (Parse68, Closure171) -> Parse70
lam_map_634 (l0, l1) =
  Parse70_0 (Variant170_0 (l0, l1))

digit_215 :: () -> Parse70
digit_215 () =
  let l0 = lam_byte_range_631 (ascii_zero_163 (), ascii_nine_219 ()) in lam_map_634 (l0, Variant171_0 ())

int_212 :: () -> Parse77
int_212 () =
  lam_many1_fold_629 (digit_215 (), 0, Variant174_0 ())

lam_lazy_635 :: Closure190 -> Parse82
lam_lazy_635 l0 =
  Parse82_0 (Variant189_0 l0)

atomic_expr_206 :: () -> Parse39
atomic_expr_206 () =
  lam_or_622 (lam_map_628 (int_212 (), Variant188_0 ()), lam_lazy_635 (Variant190_0 ()))

lam_or_638 :: (Parse33, Parse34) -> Parse35
lam_or_638 (l0, l1) =
  Parse35_0 (Variant125_0 (l0, l1))

lam_map_639 :: (Parse14, Closure122) -> Parse33
lam_map_639 (l0, l1) =
  Parse33_0 (Variant121_0 (l0, l1))

lam_map_640 :: (Parse14, Closure124) -> Parse34
lam_map_640 (l0, l1) =
  Parse34_0 (Variant123_0 (l0, l1))

mul_level_operator_223 :: () -> Parse35
mul_level_operator_223 () =
  lam_or_638 (let l0 = lam_byte_eq_466 (ascii_asterisk_225 ()) in lam_map_639 (l0, Variant122_0 ()), let l0 = lam_byte_eq_466 (ascii_slash_227 ()) in lam_map_640 (l0, Variant124_0 ()))

mul_level_expr_203 :: () -> Parse57
mul_level_expr_203 () =
  lam_operator_chain_620 (atomic_expr_206 (), mul_level_operator_223 ())

lam_or_641 :: (Parse51, Parse52) -> Parse53
lam_or_641 (l0, l1) =
  Parse53_0 (Variant148_0 (l0, l1))

lam_map_642 :: (Parse14, Closure145) -> Parse51
lam_map_642 (l0, l1) =
  Parse51_0 (Variant144_0 (l0, l1))

lam_map_643 :: (Parse14, Closure147) -> Parse52
lam_map_643 (l0, l1) =
  Parse52_0 (Variant146_0 (l0, l1))

add_level_operator_229 :: () -> Parse53
add_level_operator_229 () =
  lam_or_641 (let l0 = lam_byte_eq_466 (ascii_plus_231 ()) in lam_map_642 (l0, Variant145_0 ()), let l0 = lam_byte_eq_466 (ascii_minus_233 ()) in lam_map_643 (l0, Variant147_0 ()))

add_level_expr_200 :: () -> Parse79
add_level_expr_200 () =
  lam_operator_chain_618 (mul_level_expr_203 (), add_level_operator_229 ())

expr_193 :: () -> Parse26
expr_193 () =
  lam_spaced_609 (add_level_expr_200 ())

lam_atomic_expr_603 :: () -> Parse78
lam_atomic_expr_603 () =
  lam_skip_pre_604 (lam_byte_eq_466 (ascii_open_paren_188 ()), lam_skip_post_605 (lam_spaced_606 (expr_193 ()), lam_byte_eq_466 (ascii_close_paren_235 ())))

lam_parse_all_646 :: ((Vector Word8) , (Int64, Expr9)) -> Option10
lam_parse_all_646 (l0, (l1, l2)) =
  case uncurry (==) (l1, intrinsicLen l0) of True -> Some10_0 l2; False -> None10_1

lam_unwrap_648 :: Option10 -> Expr9
lam_unwrap_648 l0 =
  case l0 of Some10_0 l1 -> l1; None10_1 -> panic ((V.fromList [67, 97, 110, 110, 111, 116, 32, 99, 97, 108, 108, 32, 39, 79, 112, 116, 105, 111, 110, 46, 117, 110, 119, 114, 97, 112, 39, 32, 111, 110, 32, 97, 32, 39, 78, 111, 110, 101, 39, 32, 118, 97, 108, 117, 101, 10]))

lam_eval_650 :: Expr9 -> Int64
lam_eval_650 l0 =
  case l0 of Const9_0 l1 -> l1; Add9_1 (l1, l2) -> uncurry (+) (lam_eval_650 l1, lam_eval_650 l2); Sub9_2 (l1, l2) -> uncurry (-) (lam_eval_650 l1, lam_eval_650 l2); Mul9_3 (l1, l2) -> uncurry (*) (lam_eval_650 l1, lam_eval_650 l2); Div9_4 (l1, l2) -> uncurry divv (lam_eval_650 l1, lam_eval_650 l2)

lam_wrapped_eval_649 :: Expr9 -> Int64
lam_wrapped_eval_649 l0 =
  lam_eval_650 l0

lam_map_651 :: (Iter7, Closure192) -> Iter83
lam_map_651 (l0, l1) =
  Iter83_0 (Variant191_0 (l0, l1))

lam_wrapped_map_654 :: (Iter7, Closure192) -> Iter83
lam_wrapped_map_654 l0 =
  lam_map_651 l0

lam_wrapped_map_661 :: (Iter5, Closure95) -> Iter7
lam_wrapped_map_661 l0 =
  lam_map_305 l0

lam_wrapped_range_662 :: (Int64, Int64) -> Iter5
lam_wrapped_range_662 l0 =
  lam_range_302 l0

lam_items_660 :: (Vector ((Vector Word8) ))  -> Iter7
lam_items_660 l0 =
  lam_wrapped_map_661 (lam_wrapped_range_662 (0, intrinsicLen l0), Variant95_0 l0)

lam_range_665 :: (Int64, Int64) -> Iter86
lam_range_665 (l0, l1) =
  Iter86_0 (Variant193_0 (l0, l1))

lam_range_666 :: ((Int64, Int64), ()) -> Option87
lam_range_666 ((l0, l1), ()) =
  case uncurry (<) (l0, l1) of True -> Some87_0 (l0, lam_range_665 (uncurry (+) (l0, 1), l1)); False -> None87_1

lam_items_667 :: ((Vector Int64) , Int64) -> Int64
lam_items_667 (l0, l1) =
  intrinsicGet l0 l1

lam_map_668 :: (Iter86, Closure195) -> Iter88
lam_map_668 (l0, l1) =
  Iter88_0 (Variant194_0 (l0, l1))

lam_wrapped_map_672 :: (Iter86, Closure195) -> Iter88
lam_wrapped_map_672 l0 =
  lam_map_668 l0

lam_wrapped_range_673 :: (Int64, Int64) -> Iter86
lam_wrapped_range_673 l0 =
  lam_range_665 l0

lam_items_671 :: (Vector Int64)  -> Iter88
lam_items_671 l0 =
  lam_wrapped_map_672 (lam_wrapped_range_673 (0, intrinsicLen l0), Variant195_0 l0)

lam_writeln_675 :: (Vector Word8)  -> ()
lam_writeln_675 l0 =
  let l_0 = output l0 in l_0 `seq` output ((V.fromList [10]))

lam_concat_from_679 :: ((Vector Word8) , (Vector Word8) , Int64) -> (Vector Word8) 
lam_concat_from_679 (l0, l1, l2) =
  case uncurry (==) (l2, intrinsicLen l1) of True -> l0; False -> lam_concat_from_679 (intrinsicPush l0 (intrinsicGet l1 l2), l1, uncurry (+) (l2, 1))

lam_wrapped_concat_from_678 :: ((Vector Word8) , (Vector Word8) , Int64) -> (Vector Word8) 
lam_wrapped_concat_from_678 l0 =
  lam_concat_from_679 l0

lam_concat_677 :: ((Vector Word8) , (Vector Word8) ) -> (Vector Word8) 
lam_concat_677 (l0, l1) =
  lam_wrapped_concat_from_678 (l0, l1, 0)

lam_nat_to_string_682 :: Int64 -> (Vector Word8) 
lam_nat_to_string_682 l0 =
  case l0 of 0 -> (V.fromList [48]); 1 -> (V.fromList [49]); 2 -> (V.fromList [50]); 3 -> (V.fromList [51]); 4 -> (V.fromList [52]); 5 -> (V.fromList [53]); 6 -> (V.fromList [54]); 7 -> (V.fromList [55]); 8 -> (V.fromList [56]); 9 -> (V.fromList [57]); l_1 -> (V.fromList [])

lam_nat_to_string_681 :: Int64 -> (Vector Word8) 
lam_nat_to_string_681 l0 =
  let l1 = Variant497_0 () in (case uncurry (==) (l0, 0) of True -> (V.fromList []); False -> lam_concat_677 (lam_nat_to_string_681 (uncurry divv (l0, 10)), lam_nat_to_string_682 (uncurry (-) (l0, uncurry (*) (uncurry divv (l0, 10), 10)))))

lam_wrapped_nat_to_string_680 :: Int64 -> (Vector Word8) 
lam_wrapped_nat_to_string_680 l0 =
  lam_nat_to_string_681 l0

lam_int_to_string_676 :: Int64 -> (Vector Word8) 
lam_int_to_string_676 l0 =
  case uncurry (==) (l0, 0) of True -> (V.fromList [48]); False -> (case uncurry (<) (l0, 0) of True -> lam_concat_677 ((V.fromList [45]), lam_wrapped_nat_to_string_680 (uncurry (-) (0, l0))); False -> lam_wrapped_nat_to_string_680 l0)

lam_main_674 :: Int64 -> ()
lam_main_674 l0 =
  lam_writeln_675 (lam_int_to_string_676 l0)

dispatch_687 :: (Closure92, Int64) -> Word8
dispatch_687 (l0, l1) =
  case l0 of Variant92_0 l2 -> lam_items_281 (l2, l1)

dispatch_688 :: (Closure90, ()) -> Option2
dispatch_688 (l0, l1) =
  case l0 of Variant90_0 l2 -> lam_range_280 (l2, l1)

lam_next_284 :: Iter1 -> Option2
lam_next_284 l0 =
  let Iter1_0 l1 = l0 in dispatch_688 (l1, ())

lam_map_283 :: ((Iter1, Closure92), ()) -> Option4
lam_map_283 ((l0, l1), ()) =
  case lam_next_284 l0 of Some2_0 (l2, l3) -> Some4_0 (dispatch_687 (l1, l2), lam_map_282 (l3, l1)); None2_1 -> None4_1

dispatch_689 :: (Closure91, ()) -> Option4
dispatch_689 (l0, l1) =
  case l0 of Variant91_0 l2 -> lam_map_283 (l2, l1)

lam_next_286 :: Iter3 -> Option4
lam_next_286 l0 =
  let Iter3_0 l1 = l0 in dispatch_689 (l1, ())

dispatch_690 :: (Closure449, Int64) -> Int64
dispatch_690 (l0, l1) =
  case l0 of Variant449_0 l2 -> lam_chars_to_nat_289 (l2, l1)

lam_map_290 :: (Option0, Closure449) -> Option0
lam_map_290 (l0, l1) =
  case l0 of Some0_0 l2 -> Some0_0 (dispatch_690 (l1, l2)); None0_1 -> None0_1

lam_chars_to_nat_288 :: (Word8, Int64) -> Option0
lam_chars_to_nat_288 (l0, l1) =
  lam_map_290 (lam_digit_to_nat_291 l0, Variant449_0 l1)

dispatch_691 :: (Closure448, Int64) -> Option0
dispatch_691 (l0, l1) =
  case l0 of Variant448_0 l2 -> lam_chars_to_nat_288 (l2, l1)

lam_and_then_292 :: (Option0, Closure448) -> Option0
lam_and_then_292 (l0, l1) =
  case l0 of Some0_0 l2 -> dispatch_691 (l1, l2); None0_1 -> None0_1

lam_chars_to_nat_287 :: (Option0, Word8) -> Option0
lam_chars_to_nat_287 (l0, l1) =
  lam_and_then_292 (l0, Variant448_0 l1)

lam_foldl_294 :: (Iter3, Option0, Closure447) -> Option0
lam_foldl_294 (l0, l1, l2) =
  case lam_next_286 l0 of Some4_0 (l3, l4) -> lam_foldl_294 (l4, lam_chars_to_nat_287 (l1, l3), l2); None4_1 -> l1

lam_wrapped_foldl_293 :: (Iter3, Option0, Closure447) -> Option0
lam_wrapped_foldl_293 l0 =
  lam_foldl_294 l0

lam_chars_to_nat_285 :: Iter3 -> Option0
lam_chars_to_nat_285 l0 =
  case lam_next_286 l0 of None4_1 -> None0_1; Some4_0 l_0 -> lam_wrapped_foldl_293 (l0, Some0_0 0, Variant447_0 ())

lam_string_to_nat_278 :: (Vector Word8)  -> Option0
lam_string_to_nat_278 l0 =
  lam_chars_to_nat_285 (lam_items_295 l0)

dispatch_692 :: (Closure95, Int64) -> (Vector Word8) 
dispatch_692 (l0, l1) =
  case l0 of Variant95_0 l2 -> lam_items_304 (l2, l1)

dispatch_693 :: (Closure93, ()) -> Option6
dispatch_693 (l0, l1) =
  case l0 of Variant93_0 l2 -> lam_range_303 (l2, l1)

lam_next_307 :: Iter5 -> Option6
lam_next_307 l0 =
  let Iter5_0 l1 = l0 in dispatch_693 (l1, ())

lam_map_306 :: ((Iter5, Closure95), ()) -> Option8
lam_map_306 ((l0, l1), ()) =
  case lam_next_307 l0 of Some6_0 (l2, l3) -> Some8_0 (dispatch_692 (l1, l2), lam_map_305 (l3, l1)); None6_1 -> None8_1

dispatch_694 :: (Closure100, Word8) -> Bool
dispatch_694 (l0, l1) =
  case l0 of Variant100_0 l2 -> lam_byte_eq_311 (l2, l1)

lam_guard_314 :: (Closure100, Word8) -> Parse12
lam_guard_314 (l0, l1) =
  case dispatch_694 (l0, l1) of True -> lam_pure_315 l1; False -> fail_27 ()

dispatch_695 :: (Closure99, Word8) -> Parse12
dispatch_695 (l0, l1) =
  case l0 of Variant99_0 l2 -> lam_guard_314 (l2, l1)

dispatch_696 :: (Closure96, (Int64, (Vector Word8) )) -> Option11
dispatch_696 (l0, l1) =
  case l0 of Variant96_0 l2 -> lam_pure_312 (l2, l1); Variant96_1 l2 -> lam_fail_313 l1

lam_parse_from_319 :: (Int64, (Vector Word8) , Parse12) -> Option11
lam_parse_from_319 (l0, l1, l2) =
  let Parse12_0 l3 = l2 in dispatch_696 (l3, (l0, l1))

lam_and_then_318 :: (((Vector Word8) , Closure99), (Int64, Word8)) -> Option11
lam_and_then_318 ((l0, l1), (l2, l3)) =
  lam_parse_from_319 (l2, l0, dispatch_695 (l1, l3))

dispatch_697 :: (Closure450, (Int64, Word8)) -> Option11
dispatch_697 (l0, l1) =
  case l0 of Variant450_0 l2 -> lam_and_then_318 (l2, l1)

lam_and_then_320 :: (Option11, Closure450) -> Option11
lam_and_then_320 (l0, l1) =
  case l0 of Some11_0 l2 -> dispatch_697 (l1, l2); None11_1 -> None11_1

lam_and_then_316 :: ((Parse13, Closure99), (Int64, (Vector Word8) )) -> Option11
lam_and_then_316 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_317 (l2, l3, l0) in lam_and_then_320 (l4, Variant450_0 (l3, l1))

dispatch_698 :: (Closure98, (Int64, (Vector Word8) )) -> Option11
dispatch_698 (l0, l1) =
  case l0 of Variant98_0 l2 -> lam_and_then_316 (l2, l1)

lam_parse_from_322 :: (Int64, (Vector Word8) , Parse14) -> Option11
lam_parse_from_322 (l0, l1, l2) =
  let Parse14_0 l3 = l2 in dispatch_698 (l3, (l0, l1))

lam_or_323 :: ((Int64, (Vector Word8) , Parse14), ()) -> Option11
lam_or_323 ((l0, l1, l2), ()) =
  lam_parse_from_322 (l0, l1, l2)

dispatch_699 :: (Closure451, ()) -> Option11
dispatch_699 (l0, l1) =
  case l0 of Variant451_0 l2 -> lam_or_323 (l2, l1)

lam_or_else_324 :: (Option11, Closure451) -> Option11
lam_or_else_324 (l0, l1) =
  case l0 of Some11_0 l2 -> Some11_0 l2; None11_1 -> dispatch_699 (l1, ())

lam_or_321 :: ((Parse14, Parse14), (Int64, (Vector Word8) )) -> Option11
lam_or_321 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_322 (l2, l3, l0) in lam_or_else_324 (l4, Variant451_0 (l2, l3, l1))

dispatch_700 :: (Closure101, (Int64, (Vector Word8) )) -> Option11
dispatch_700 (l0, l1) =
  case l0 of Variant101_0 l2 -> lam_or_321 (l2, l1)

lam_parse_from_327 :: (Int64, (Vector Word8) , Parse15) -> Option11
lam_parse_from_327 (l0, l1, l2) =
  let Parse15_0 l3 = l2 in dispatch_700 (l3, (l0, l1))

lam_or_326 :: ((Int64, (Vector Word8) , Parse15), ()) -> Option11
lam_or_326 ((l0, l1, l2), ()) =
  lam_parse_from_327 (l0, l1, l2)

dispatch_701 :: (Closure452, ()) -> Option11
dispatch_701 (l0, l1) =
  case l0 of Variant452_0 l2 -> lam_or_326 (l2, l1)

lam_or_else_328 :: (Option11, Closure452) -> Option11
lam_or_else_328 (l0, l1) =
  case l0 of Some11_0 l2 -> Some11_0 l2; None11_1 -> dispatch_701 (l1, ())

lam_or_325 :: ((Parse14, Parse15), (Int64, (Vector Word8) )) -> Option11
lam_or_325 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_322 (l2, l3, l0) in lam_or_else_328 (l4, Variant452_0 (l2, l3, l1))

dispatch_702 :: (Closure102, (Int64, (Vector Word8) )) -> Option11
dispatch_702 (l0, l1) =
  case l0 of Variant102_0 l2 -> lam_or_325 (l2, l1)

lam_parse_from_331 :: (Int64, (Vector Word8) , Parse16) -> Option11
lam_parse_from_331 (l0, l1, l2) =
  let Parse16_0 l3 = l2 in dispatch_702 (l3, (l0, l1))

dispatch_703 :: (Closure453, (Int64, Word8)) -> (Int64, ())
dispatch_703 (l0, l1) =
  case l0 of Variant453_0 l2 -> lam_map_332 (l2, l1)

lam_map_333 :: (Option11, Closure453) -> Option17
lam_map_333 (l0, l1) =
  case l0 of Some11_0 l2 -> Some17_0 (dispatch_703 (l1, l2)); None11_1 -> None17_1

lam_map_330 :: ((Parse16, Closure104), (Int64, (Vector Word8) )) -> Option17
lam_map_330 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_331 (l2, l3, l0) in lam_map_333 (l4, Variant453_0 l1)

dispatch_704 :: (Closure103, (Int64, (Vector Word8) )) -> Option17
dispatch_704 (l0, l1) =
  case l0 of Variant103_0 l2 -> lam_map_330 (l2, l1)

lam_parse_from_338 :: (Int64, (Vector Word8) , Parse18) -> Option17
lam_parse_from_338 (l0, l1, l2) =
  let Parse18_0 l3 = l2 in dispatch_704 (l3, (l0, l1))

dispatch_705 :: (Closure454, (Int64, ())) -> (Int64, Option20)
dispatch_705 (l0, l1) =
  case l0 of Variant454_0 l2 -> lam_map_339 (l2, l1)

lam_map_340 :: (Option17, Closure454) -> Option21
lam_map_340 (l0, l1) =
  case l0 of Some17_0 l2 -> Some21_0 (dispatch_705 (l1, l2)); None17_1 -> None21_1

lam_map_337 :: ((Parse18, Closure110), (Int64, (Vector Word8) )) -> Option21
lam_map_337 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_338 (l2, l3, l0) in lam_map_340 (l4, Variant454_0 l1)

dispatch_706 :: (Closure108, (Int64, (Vector Word8) )) -> Option21
dispatch_706 (l0, l1) =
  case l0 of Variant108_0 l2 -> lam_pure_341 (l2, l1)

lam_parse_from_345 :: (Int64, (Vector Word8) , Parse22) -> Option21
lam_parse_from_345 (l0, l1, l2) =
  let Parse22_0 l3 = l2 in dispatch_706 (l3, (l0, l1))

lam_lazy_344 :: (Closure112, (Int64, (Vector Word8) )) -> Option21
lam_lazy_344 (l0, (l1, l2)) =
  lam_parse_from_345 (l1, l2, lam_optional_342 ())

dispatch_707 :: (Closure109, (Int64, (Vector Word8) )) -> Option21
dispatch_707 (l0, l1) =
  case l0 of Variant109_0 l2 -> lam_map_337 (l2, l1)

lam_parse_from_347 :: (Int64, (Vector Word8) , Parse23) -> Option21
lam_parse_from_347 (l0, l1, l2) =
  let Parse23_0 l3 = l2 in dispatch_707 (l3, (l0, l1))

dispatch_708 :: (Closure111, (Int64, (Vector Word8) )) -> Option21
dispatch_708 (l0, l1) =
  case l0 of Variant111_0 l2 -> lam_lazy_344 (l2, l1)

lam_parse_from_349 :: (Int64, (Vector Word8) , Parse24) -> Option21
lam_parse_from_349 (l0, l1, l2) =
  let Parse24_0 l3 = l2 in dispatch_708 (l3, (l0, l1))

lam_or_348 :: ((Int64, (Vector Word8) , Parse24), ()) -> Option21
lam_or_348 ((l0, l1, l2), ()) =
  lam_parse_from_349 (l0, l1, l2)

dispatch_709 :: (Closure455, ()) -> Option21
dispatch_709 (l0, l1) =
  case l0 of Variant455_0 l2 -> lam_or_348 (l2, l1)

lam_or_else_350 :: (Option21, Closure455) -> Option21
lam_or_else_350 (l0, l1) =
  case l0 of Some21_0 l2 -> Some21_0 l2; None21_1 -> dispatch_709 (l1, ())

lam_or_346 :: ((Parse23, Parse24), (Int64, (Vector Word8) )) -> Option21
lam_or_346 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_347 (l2, l3, l0) in lam_or_else_350 (l4, Variant455_0 (l2, l3, l1))

dispatch_710 :: (Closure113, (Int64, (Vector Word8) )) -> Option21
dispatch_710 (l0, l1) =
  case l0 of Variant113_0 l2 -> lam_or_346 (l2, l1)

lam_parse_from_356 :: (Int64, (Vector Word8) , Parse25) -> Option21
lam_parse_from_356 (l0, l1, l2) =
  let Parse25_0 l3 = l2 in dispatch_710 (l3, (l0, l1))

dispatch_711 :: (Closure106, Option20) -> Parse19
dispatch_711 (l0, l1) =
  case l0 of Variant106_0 l2 -> lam_many0_fold_351 (l2, l1)

dispatch_713 :: (Closure456, (Int64, Option20)) -> Option17
dispatch_713 (l0, l1) =
  case l0 of Variant456_0 l2 -> lam_and_then_357 (l2, l1)

lam_and_then_357 ((l0, l1), (l2, l3)) =
  lam_parse_from_358 (l2, l0, dispatch_711 (l1, l3))

lam_parse_from_358 (l0, l1, l2) =
  let Parse19_0 l3 = l2 in dispatch_712 (l3, (l0, l1))

dispatch_712 (l0, l1) =
  case l0 of Variant105_0 l2 -> lam_pure_353 (l2, l1); Variant105_1 l2 -> lam_and_then_355 (l2, l1)

lam_and_then_355 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_356 (l2, l3, l0) in lam_and_then_359 (l4, Variant456_0 (l3, l1))

lam_and_then_359 (l0, l1) =
  case l0 of Some21_0 l2 -> dispatch_713 (l1, l2); None21_1 -> None17_1

dispatch_714 :: (Closure116, ()) -> Parse19
dispatch_714 (l0, l1) =
  case l0 of Variant116_0 l2 -> lam_skip_many0_364 (l2, l1)

lam_lazy_366 :: (Closure116, (Int64, (Vector Word8) )) -> Option17
lam_lazy_366 (l0, (l1, l2)) =
  lam_parse_from_358 (l1, l2, dispatch_714 (l0, ()))

dispatch_716 :: (Closure115, (Int64, (Vector Word8) )) -> Option17
dispatch_716 (l0, l1) =
  case l0 of Variant115_0 l2 -> lam_lazy_366 (l2, l1)

lam_parse_from_370 :: (Int64, (Vector Word8) , Parse27) -> Option17
lam_parse_from_370 (l0, l1, l2) =
  let Parse27_0 l3 = l2 in dispatch_716 (l3, (l0, l1))

dispatch_717 :: (Closure458, (Int64, ())) -> (Int64, Expr9)
dispatch_717 (l0, l1) =
  case l0 of Variant458_0 l2 -> lam_skip_post_371 (l2, l1)

lam_map_372 :: (Option17, Closure458) -> Option28
lam_map_372 (l0, l1) =
  case l0 of Some17_0 l2 -> Some28_0 (dispatch_717 (l1, l2)); None17_1 -> None28_1

lam_skip_post_369 :: (((Vector Word8) , Parse27), (Int64, Expr9)) -> Option28
lam_skip_post_369 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_370 (l2, l0, l1) in lam_map_372 (l4, Variant458_0 l3)

dispatch_718 :: (Closure457, (Int64, Expr9)) -> Option28
dispatch_718 (l0, l1) =
  case l0 of Variant457_0 l2 -> lam_skip_post_369 (l2, l1)

lam_and_then_373 :: (Option28, Closure457) -> Option28
lam_and_then_373 (l0, l1) =
  case l0 of Some28_0 l2 -> dispatch_718 (l1, l2); None28_1 -> None28_1

dispatch_722 :: (Closure461, (Int64, Word8)) -> (Int64, Expr9)
dispatch_722 (l0, l1) =
  case l0 of Variant461_0 l2 -> lam_skip_post_381 (l2, l1)

lam_map_382 :: (Option11, Closure461) -> Option28
lam_map_382 (l0, l1) =
  case l0 of Some11_0 l2 -> Some28_0 (dispatch_722 (l1, l2)); None11_1 -> None28_1

lam_skip_post_380 :: (((Vector Word8) , Parse14), (Int64, Expr9)) -> Option28
lam_skip_post_380 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_322 (l2, l0, l1) in lam_map_382 (l4, Variant461_0 l3)

dispatch_723 :: (Closure460, (Int64, Expr9)) -> Option28
dispatch_723 (l0, l1) =
  case l0 of Variant460_0 l2 -> lam_skip_post_380 (l2, l1)

lam_and_then_383 :: (Option28, Closure460) -> Option28
lam_and_then_383 (l0, l1) =
  case l0 of Some28_0 l2 -> dispatch_723 (l1, l2); None28_1 -> None28_1

dispatch_726 :: (Closure463, (Int64, Word8)) -> (Int64, Closure120)
dispatch_726 (l0, l1) =
  case l0 of Variant463_0 l2 -> lam_map_392 (l2, l1)

lam_map_393 :: (Option11, Closure463) -> Option32
lam_map_393 (l0, l1) =
  case l0 of Some11_0 l2 -> Some32_0 (dispatch_726 (l1, l2)); None11_1 -> None32_1

lam_map_391 :: ((Parse14, Closure122), (Int64, (Vector Word8) )) -> Option32
lam_map_391 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_322 (l2, l3, l0) in lam_map_393 (l4, Variant463_0 l1)

dispatch_727 :: (Closure464, (Int64, Word8)) -> (Int64, Closure120)
dispatch_727 (l0, l1) =
  case l0 of Variant464_0 l2 -> lam_map_396 (l2, l1)

lam_map_397 :: (Option11, Closure464) -> Option32
lam_map_397 (l0, l1) =
  case l0 of Some11_0 l2 -> Some32_0 (dispatch_727 (l1, l2)); None11_1 -> None32_1

lam_map_395 :: ((Parse14, Closure124), (Int64, (Vector Word8) )) -> Option32
lam_map_395 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_322 (l2, l3, l0) in lam_map_397 (l4, Variant464_0 l1)

dispatch_728 :: (Closure121, (Int64, (Vector Word8) )) -> Option32
dispatch_728 (l0, l1) =
  case l0 of Variant121_0 l2 -> lam_map_391 (l2, l1)

lam_parse_from_399 :: (Int64, (Vector Word8) , Parse33) -> Option32
lam_parse_from_399 (l0, l1, l2) =
  let Parse33_0 l3 = l2 in dispatch_728 (l3, (l0, l1))

dispatch_729 :: (Closure123, (Int64, (Vector Word8) )) -> Option32
dispatch_729 (l0, l1) =
  case l0 of Variant123_0 l2 -> lam_map_395 (l2, l1)

lam_parse_from_401 :: (Int64, (Vector Word8) , Parse34) -> Option32
lam_parse_from_401 (l0, l1, l2) =
  let Parse34_0 l3 = l2 in dispatch_729 (l3, (l0, l1))

lam_or_400 :: ((Int64, (Vector Word8) , Parse34), ()) -> Option32
lam_or_400 ((l0, l1, l2), ()) =
  lam_parse_from_401 (l0, l1, l2)

dispatch_730 :: (Closure465, ()) -> Option32
dispatch_730 (l0, l1) =
  case l0 of Variant465_0 l2 -> lam_or_400 (l2, l1)

lam_or_else_402 :: (Option32, Closure465) -> Option32
lam_or_else_402 (l0, l1) =
  case l0 of Some32_0 l2 -> Some32_0 l2; None32_1 -> dispatch_730 (l1, ())

lam_or_398 :: ((Parse33, Parse34), (Int64, (Vector Word8) )) -> Option32
lam_or_398 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_399 (l2, l3, l0) in lam_or_else_402 (l4, Variant465_0 (l2, l3, l1))

dispatch_731 :: (Closure125, (Int64, (Vector Word8) )) -> Option32
dispatch_731 (l0, l1) =
  case l0 of Variant125_0 l2 -> lam_or_398 (l2, l1)

lam_parse_from_404 :: (Int64, (Vector Word8) , Parse35) -> Option32
lam_parse_from_404 (l0, l1, l2) =
  let Parse35_0 l3 = l2 in dispatch_731 (l3, (l0, l1))

dispatch_732 :: (Closure467, (Int64, ())) -> (Int64, Closure120)
dispatch_732 (l0, l1) =
  case l0 of Variant467_0 l2 -> lam_skip_post_406 (l2, l1)

lam_map_407 :: (Option17, Closure467) -> Option32
lam_map_407 (l0, l1) =
  case l0 of Some17_0 l2 -> Some32_0 (dispatch_732 (l1, l2)); None17_1 -> None32_1

lam_skip_post_405 :: (((Vector Word8) , Parse27), (Int64, Closure120)) -> Option32
lam_skip_post_405 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_370 (l2, l0, l1) in lam_map_407 (l4, Variant467_0 l3)

dispatch_733 :: (Closure466, (Int64, Closure120)) -> Option32
dispatch_733 (l0, l1) =
  case l0 of Variant466_0 l2 -> lam_skip_post_405 (l2, l1)

lam_and_then_408 :: (Option32, Closure466) -> Option32
lam_and_then_408 (l0, l1) =
  case l0 of Some32_0 l2 -> dispatch_733 (l1, l2); None32_1 -> None32_1

lam_skip_post_403 :: ((Parse35, Parse27), (Int64, (Vector Word8) )) -> Option32
lam_skip_post_403 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_404 (l2, l3, l0) in lam_and_then_408 (l4, Variant466_0 (l3, l1))

dispatch_734 :: (Closure126, (Int64, (Vector Word8) )) -> Option32
dispatch_734 (l0, l1) =
  case l0 of Variant126_0 l2 -> lam_skip_post_403 (l2, l1)

lam_parse_from_411 :: (Int64, (Vector Word8) , Parse36) -> Option32
lam_parse_from_411 (l0, l1, l2) =
  let Parse36_0 l3 = l2 in dispatch_734 (l3, (l0, l1))

lam_skip_pre_410 :: (((Vector Word8) , Parse36), (Int64, ())) -> Option32
lam_skip_pre_410 ((l0, l1), (l2, l_0)) =
  lam_parse_from_411 (l2, l0, l1)

dispatch_735 :: (Closure468, (Int64, ())) -> Option32
dispatch_735 (l0, l1) =
  case l0 of Variant468_0 l2 -> lam_skip_pre_410 (l2, l1)

lam_and_then_412 :: (Option17, Closure468) -> Option32
lam_and_then_412 (l0, l1) =
  case l0 of Some17_0 l2 -> dispatch_735 (l1, l2); None17_1 -> None32_1

lam_skip_pre_409 :: ((Parse27, Parse36), (Int64, (Vector Word8) )) -> Option32
lam_skip_pre_409 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_370 (l2, l3, l0) in lam_and_then_412 (l4, Variant468_0 (l3, l1))

dispatch_737 :: (Closure130, Expr9) -> Parse38
dispatch_737 (l0, l1) =
  case l0 of Variant130_0 l2 -> lam_operator_chain_414 (l2, l1)

dispatch_738 :: (Closure127, (Int64, (Vector Word8) )) -> Option37
dispatch_738 (l0, l1) =
  case l0 of Variant127_0 l2 -> lam_pure_413 (l2, l1)

lam_parse_from_419 :: (Int64, (Vector Word8) , Parse38) -> Option37
lam_parse_from_419 (l0, l1, l2) =
  let Parse38_0 l3 = l2 in dispatch_738 (l3, (l0, l1))

lam_and_then_418 :: (((Vector Word8) , Closure130), (Int64, Expr9)) -> Option37
lam_and_then_418 ((l0, l1), (l2, l3)) =
  lam_parse_from_419 (l2, l0, dispatch_737 (l1, l3))

dispatch_739 :: (Closure469, (Int64, Expr9)) -> Option37
dispatch_739 (l0, l1) =
  case l0 of Variant469_0 l2 -> lam_and_then_418 (l2, l1)

lam_and_then_420 :: (Option28, Closure469) -> Option37
lam_and_then_420 (l0, l1) =
  case l0 of Some28_0 l2 -> dispatch_739 (l1, l2); None28_1 -> None37_1

dispatch_740 :: (Closure131, (Int64, (Vector Word8) )) -> Option32
dispatch_740 (l0, l1) =
  case l0 of Variant131_0 l2 -> lam_skip_pre_409 (l2, l1)

lam_parse_from_424 :: (Int64, (Vector Word8) , Parse41) -> Option32
lam_parse_from_424 (l0, l1, l2) =
  let Parse41_0 l3 = l2 in dispatch_740 (l3, (l0, l1))

dispatch_741 :: (Closure133, Closure120) -> Parse40
dispatch_741 (l0, l1) =
  case l0 of Variant133_0 l2 -> lam_operator_chain_421 (l2, l1)

dispatch_744 :: (Closure120, (Expr9, Expr9)) -> Expr9
dispatch_744 (l0, l1) =
  case l0 of Variant120_0 l2 -> lam_mul_level_operator_388 l1; Variant120_1 l2 -> lam_mul_level_operator_389 l1

lam_operator_chain_428 :: (Expr9, (Closure120, Expr9)) -> Expr9
lam_operator_chain_428 (l0, (l1, l2)) =
  dispatch_744 (l1, (l0, l2))

lam_many0_fold_445 :: ((Expr9, Parse42, Closure136), Option44) -> Parse43
lam_many0_fold_445 ((l0, l1, l2), l3) =
  case l3 of None44_1 -> lam_pure_446 l0; Some44_0 l4 -> lam_many0_fold_429 (l1, lam_operator_chain_428 (l0, l4), l2)

dispatch_746 :: (Closure471, (Int64, (Closure120, Expr9))) -> (Int64, Option44)
dispatch_746 (l0, l1) =
  case l0 of Variant471_0 l2 -> lam_map_433 (l2, l1)

lam_map_434 :: (Option37, Closure471) -> Option45
lam_map_434 (l0, l1) =
  case l0 of Some37_0 l2 -> Some45_0 (dispatch_746 (l1, l2)); None37_1 -> None45_1

dispatch_747 :: (Closure137, (Int64, (Vector Word8) )) -> Option45
dispatch_747 (l0, l1) =
  case l0 of Variant137_0 l2 -> lam_pure_435 (l2, l1)

lam_parse_from_439 :: (Int64, (Vector Word8) , Parse46) -> Option45
lam_parse_from_439 (l0, l1, l2) =
  let Parse46_0 l3 = l2 in dispatch_747 (l3, (l0, l1))

lam_lazy_438 :: (Closure141, (Int64, (Vector Word8) )) -> Option45
lam_lazy_438 (l0, (l1, l2)) =
  lam_parse_from_439 (l1, l2, lam_optional_436 ())

dispatch_749 :: (Closure140, (Int64, (Vector Word8) )) -> Option45
dispatch_749 (l0, l1) =
  case l0 of Variant140_0 l2 -> lam_lazy_438 (l2, l1)

lam_parse_from_443 :: (Int64, (Vector Word8) , Parse48) -> Option45
lam_parse_from_443 (l0, l1, l2) =
  let Parse48_0 l3 = l2 in dispatch_749 (l3, (l0, l1))

lam_or_442 :: ((Int64, (Vector Word8) , Parse48), ()) -> Option45
lam_or_442 ((l0, l1, l2), ()) =
  lam_parse_from_443 (l0, l1, l2)

dispatch_750 :: (Closure472, ()) -> Option45
dispatch_750 (l0, l1) =
  case l0 of Variant472_0 l2 -> lam_or_442 (l2, l1)

lam_or_else_444 :: (Option45, Closure472) -> Option45
lam_or_else_444 (l0, l1) =
  case l0 of Some45_0 l2 -> Some45_0 l2; None45_1 -> dispatch_750 (l1, ())

dispatch_752 :: (Closure135, Option44) -> Parse43
dispatch_752 (l0, l1) =
  case l0 of Variant135_0 l2 -> lam_many0_fold_445 (l2, l1)

dispatch_755 :: (Closure152, Expr9) -> Parse43
dispatch_755 (l0, l1) =
  case l0 of Variant152_0 l2 -> lam_operator_chain_458 (l2, l1)

dispatch_757 :: (Closure475, (Int64, Word8)) -> (Int64, Closure143)
dispatch_757 (l0, l1) =
  case l0 of Variant475_0 l2 -> lam_map_479 (l2, l1)

lam_map_480 :: (Option11, Closure475) -> Option50
lam_map_480 (l0, l1) =
  case l0 of Some11_0 l2 -> Some50_0 (dispatch_757 (l1, l2)); None11_1 -> None50_1

lam_map_478 :: ((Parse14, Closure145), (Int64, (Vector Word8) )) -> Option50
lam_map_478 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_322 (l2, l3, l0) in lam_map_480 (l4, Variant475_0 l1)

dispatch_758 :: (Closure476, (Int64, Word8)) -> (Int64, Closure143)
dispatch_758 (l0, l1) =
  case l0 of Variant476_0 l2 -> lam_map_483 (l2, l1)

lam_map_484 :: (Option11, Closure476) -> Option50
lam_map_484 (l0, l1) =
  case l0 of Some11_0 l2 -> Some50_0 (dispatch_758 (l1, l2)); None11_1 -> None50_1

lam_map_482 :: ((Parse14, Closure147), (Int64, (Vector Word8) )) -> Option50
lam_map_482 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_322 (l2, l3, l0) in lam_map_484 (l4, Variant476_0 l1)

dispatch_759 :: (Closure144, (Int64, (Vector Word8) )) -> Option50
dispatch_759 (l0, l1) =
  case l0 of Variant144_0 l2 -> lam_map_478 (l2, l1)

lam_parse_from_486 :: (Int64, (Vector Word8) , Parse51) -> Option50
lam_parse_from_486 (l0, l1, l2) =
  let Parse51_0 l3 = l2 in dispatch_759 (l3, (l0, l1))

dispatch_760 :: (Closure146, (Int64, (Vector Word8) )) -> Option50
dispatch_760 (l0, l1) =
  case l0 of Variant146_0 l2 -> lam_map_482 (l2, l1)

lam_parse_from_488 :: (Int64, (Vector Word8) , Parse52) -> Option50
lam_parse_from_488 (l0, l1, l2) =
  let Parse52_0 l3 = l2 in dispatch_760 (l3, (l0, l1))

lam_or_487 :: ((Int64, (Vector Word8) , Parse52), ()) -> Option50
lam_or_487 ((l0, l1, l2), ()) =
  lam_parse_from_488 (l0, l1, l2)

dispatch_761 :: (Closure477, ()) -> Option50
dispatch_761 (l0, l1) =
  case l0 of Variant477_0 l2 -> lam_or_487 (l2, l1)

lam_or_else_489 :: (Option50, Closure477) -> Option50
lam_or_else_489 (l0, l1) =
  case l0 of Some50_0 l2 -> Some50_0 l2; None50_1 -> dispatch_761 (l1, ())

lam_or_485 :: ((Parse51, Parse52), (Int64, (Vector Word8) )) -> Option50
lam_or_485 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_486 (l2, l3, l0) in lam_or_else_489 (l4, Variant477_0 (l2, l3, l1))

dispatch_762 :: (Closure148, (Int64, (Vector Word8) )) -> Option50
dispatch_762 (l0, l1) =
  case l0 of Variant148_0 l2 -> lam_or_485 (l2, l1)

lam_parse_from_491 :: (Int64, (Vector Word8) , Parse53) -> Option50
lam_parse_from_491 (l0, l1, l2) =
  let Parse53_0 l3 = l2 in dispatch_762 (l3, (l0, l1))

dispatch_763 :: (Closure479, (Int64, ())) -> (Int64, Closure143)
dispatch_763 (l0, l1) =
  case l0 of Variant479_0 l2 -> lam_skip_post_493 (l2, l1)

lam_map_494 :: (Option17, Closure479) -> Option50
lam_map_494 (l0, l1) =
  case l0 of Some17_0 l2 -> Some50_0 (dispatch_763 (l1, l2)); None17_1 -> None50_1

lam_skip_post_492 :: (((Vector Word8) , Parse27), (Int64, Closure143)) -> Option50
lam_skip_post_492 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_370 (l2, l0, l1) in lam_map_494 (l4, Variant479_0 l3)

dispatch_764 :: (Closure478, (Int64, Closure143)) -> Option50
dispatch_764 (l0, l1) =
  case l0 of Variant478_0 l2 -> lam_skip_post_492 (l2, l1)

lam_and_then_495 :: (Option50, Closure478) -> Option50
lam_and_then_495 (l0, l1) =
  case l0 of Some50_0 l2 -> dispatch_764 (l1, l2); None50_1 -> None50_1

lam_skip_post_490 :: ((Parse53, Parse27), (Int64, (Vector Word8) )) -> Option50
lam_skip_post_490 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_491 (l2, l3, l0) in lam_and_then_495 (l4, Variant478_0 (l3, l1))

dispatch_765 :: (Closure149, (Int64, (Vector Word8) )) -> Option50
dispatch_765 (l0, l1) =
  case l0 of Variant149_0 l2 -> lam_skip_post_490 (l2, l1)

lam_parse_from_498 :: (Int64, (Vector Word8) , Parse54) -> Option50
lam_parse_from_498 (l0, l1, l2) =
  let Parse54_0 l3 = l2 in dispatch_765 (l3, (l0, l1))

lam_skip_pre_497 :: (((Vector Word8) , Parse54), (Int64, ())) -> Option50
lam_skip_pre_497 ((l0, l1), (l2, l_0)) =
  lam_parse_from_498 (l2, l0, l1)

dispatch_766 :: (Closure480, (Int64, ())) -> Option50
dispatch_766 (l0, l1) =
  case l0 of Variant480_0 l2 -> lam_skip_pre_497 (l2, l1)

lam_and_then_499 :: (Option17, Closure480) -> Option50
lam_and_then_499 (l0, l1) =
  case l0 of Some17_0 l2 -> dispatch_766 (l1, l2); None17_1 -> None50_1

lam_skip_pre_496 :: ((Parse27, Parse54), (Int64, (Vector Word8) )) -> Option50
lam_skip_pre_496 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_370 (l2, l3, l0) in lam_and_then_499 (l4, Variant480_0 (l3, l1))

dispatch_768 :: (Closure154, Expr9) -> Parse56
dispatch_768 (l0, l1) =
  case l0 of Variant154_0 l2 -> lam_operator_chain_501 (l2, l1)

dispatch_769 :: (Closure150, (Int64, (Vector Word8) )) -> Option55
dispatch_769 (l0, l1) =
  case l0 of Variant150_0 l2 -> lam_pure_500 (l2, l1)

lam_parse_from_506 :: (Int64, (Vector Word8) , Parse56) -> Option55
lam_parse_from_506 (l0, l1, l2) =
  let Parse56_0 l3 = l2 in dispatch_769 (l3, (l0, l1))

lam_and_then_505 :: (((Vector Word8) , Closure154), (Int64, Expr9)) -> Option55
lam_and_then_505 ((l0, l1), (l2, l3)) =
  lam_parse_from_506 (l2, l0, dispatch_768 (l1, l3))

dispatch_770 :: (Closure481, (Int64, Expr9)) -> Option55
dispatch_770 (l0, l1) =
  case l0 of Variant481_0 l2 -> lam_and_then_505 (l2, l1)

lam_and_then_507 :: (Option28, Closure481) -> Option55
lam_and_then_507 (l0, l1) =
  case l0 of Some28_0 l2 -> dispatch_770 (l1, l2); None28_1 -> None55_1

dispatch_771 :: (Closure155, (Int64, (Vector Word8) )) -> Option50
dispatch_771 (l0, l1) =
  case l0 of Variant155_0 l2 -> lam_skip_pre_496 (l2, l1)

lam_parse_from_511 :: (Int64, (Vector Word8) , Parse59) -> Option50
lam_parse_from_511 (l0, l1, l2) =
  let Parse59_0 l3 = l2 in dispatch_771 (l3, (l0, l1))

dispatch_772 :: (Closure157, Closure143) -> Parse58
dispatch_772 (l0, l1) =
  case l0 of Variant157_0 l2 -> lam_operator_chain_508 (l2, l1)

dispatch_775 :: (Closure143, (Expr9, Expr9)) -> Expr9
dispatch_775 (l0, l1) =
  case l0 of Variant143_0 l2 -> lam_add_level_operator_475 l1; Variant143_1 l2 -> lam_add_level_operator_476 l1

lam_operator_chain_515 :: (Expr9, (Closure143, Expr9)) -> Expr9
lam_operator_chain_515 (l0, (l1, l2)) =
  dispatch_775 (l1, (l0, l2))

lam_many0_fold_532 :: ((Expr9, Parse60, Closure160), Option62) -> Parse61
lam_many0_fold_532 ((l0, l1, l2), l3) =
  case l3 of None62_1 -> lam_pure_533 l0; Some62_0 l4 -> lam_many0_fold_516 (l1, lam_operator_chain_515 (l0, l4), l2)

dispatch_777 :: (Closure483, (Int64, (Closure143, Expr9))) -> (Int64, Option62)
dispatch_777 (l0, l1) =
  case l0 of Variant483_0 l2 -> lam_map_520 (l2, l1)

lam_map_521 :: (Option55, Closure483) -> Option63
lam_map_521 (l0, l1) =
  case l0 of Some55_0 l2 -> Some63_0 (dispatch_777 (l1, l2)); None55_1 -> None63_1

dispatch_778 :: (Closure161, (Int64, (Vector Word8) )) -> Option63
dispatch_778 (l0, l1) =
  case l0 of Variant161_0 l2 -> lam_pure_522 (l2, l1)

lam_parse_from_526 :: (Int64, (Vector Word8) , Parse64) -> Option63
lam_parse_from_526 (l0, l1, l2) =
  let Parse64_0 l3 = l2 in dispatch_778 (l3, (l0, l1))

lam_lazy_525 :: (Closure165, (Int64, (Vector Word8) )) -> Option63
lam_lazy_525 (l0, (l1, l2)) =
  lam_parse_from_526 (l1, l2, lam_optional_523 ())

dispatch_780 :: (Closure164, (Int64, (Vector Word8) )) -> Option63
dispatch_780 (l0, l1) =
  case l0 of Variant164_0 l2 -> lam_lazy_525 (l2, l1)

lam_parse_from_530 :: (Int64, (Vector Word8) , Parse66) -> Option63
lam_parse_from_530 (l0, l1, l2) =
  let Parse66_0 l3 = l2 in dispatch_780 (l3, (l0, l1))

lam_or_529 :: ((Int64, (Vector Word8) , Parse66), ()) -> Option63
lam_or_529 ((l0, l1, l2), ()) =
  lam_parse_from_530 (l0, l1, l2)

dispatch_781 :: (Closure484, ()) -> Option63
dispatch_781 (l0, l1) =
  case l0 of Variant484_0 l2 -> lam_or_529 (l2, l1)

lam_or_else_531 :: (Option63, Closure484) -> Option63
lam_or_else_531 (l0, l1) =
  case l0 of Some63_0 l2 -> Some63_0 l2; None63_1 -> dispatch_781 (l1, ())

dispatch_783 :: (Closure159, Option62) -> Parse61
dispatch_783 (l0, l1) =
  case l0 of Variant159_0 l2 -> lam_many0_fold_532 (l2, l1)

dispatch_786 :: (Closure185, Expr9) -> Parse61
dispatch_786 (l0, l1) =
  case l0 of Variant185_0 l2 -> lam_operator_chain_544 (l2, l1)

dispatch_788 :: (Closure169, Word8) -> Bool
dispatch_788 (l0, l1) =
  case l0 of Variant169_0 l2 -> lam_byte_range_553 (l2, l1)

lam_guard_554 :: (Closure169, Word8) -> Parse12
lam_guard_554 (l0, l1) =
  case dispatch_788 (l0, l1) of True -> lam_pure_315 l1; False -> fail_27 ()

dispatch_789 :: (Closure168, Word8) -> Parse12
dispatch_789 (l0, l1) =
  case l0 of Variant168_0 l2 -> lam_guard_554 (l2, l1)

lam_and_then_556 :: (((Vector Word8) , Closure168), (Int64, Word8)) -> Option11
lam_and_then_556 ((l0, l1), (l2, l3)) =
  lam_parse_from_319 (l2, l0, dispatch_789 (l1, l3))

dispatch_790 :: (Closure487, (Int64, Word8)) -> Option11
dispatch_790 (l0, l1) =
  case l0 of Variant487_0 l2 -> lam_and_then_556 (l2, l1)

lam_and_then_557 :: (Option11, Closure487) -> Option11
lam_and_then_557 (l0, l1) =
  case l0 of Some11_0 l2 -> dispatch_790 (l1, l2); None11_1 -> None11_1

lam_and_then_555 :: ((Parse13, Closure168), (Int64, (Vector Word8) )) -> Option11
lam_and_then_555 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_317 (l2, l3, l0) in lam_and_then_557 (l4, Variant487_0 (l3, l1))

dispatch_791 :: (Closure167, (Int64, (Vector Word8) )) -> Option11
dispatch_791 (l0, l1) =
  case l0 of Variant167_0 l2 -> lam_and_then_555 (l2, l1)

lam_parse_from_560 :: (Int64, (Vector Word8) , Parse68) -> Option11
lam_parse_from_560 (l0, l1, l2) =
  let Parse68_0 l3 = l2 in dispatch_791 (l3, (l0, l1))

dispatch_792 :: (Closure488, (Int64, Word8)) -> (Int64, Int64)
dispatch_792 (l0, l1) =
  case l0 of Variant488_0 l2 -> lam_map_561 (l2, l1)

lam_map_562 :: (Option11, Closure488) -> Option69
lam_map_562 (l0, l1) =
  case l0 of Some11_0 l2 -> Some69_0 (dispatch_792 (l1, l2)); None11_1 -> None69_1

lam_map_559 :: ((Parse68, Closure171), (Int64, (Vector Word8) )) -> Option69
lam_map_559 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_560 (l2, l3, l0) in lam_map_562 (l4, Variant488_0 l1)

dispatch_793 :: (Closure170, (Int64, (Vector Word8) )) -> Option69
dispatch_793 (l0, l1) =
  case l0 of Variant170_0 l2 -> lam_map_559 (l2, l1)

lam_parse_from_567 :: (Int64, (Vector Word8) , Parse70) -> Option69
lam_parse_from_567 (l0, l1, l2) =
  let Parse70_0 l3 = l2 in dispatch_793 (l3, (l0, l1))

dispatch_794 :: (Closure489, (Int64, Int64)) -> (Int64, Option0)
dispatch_794 (l0, l1) =
  case l0 of Variant489_0 l2 -> lam_map_568 (l2, l1)

lam_map_569 :: (Option69, Closure489) -> Option72
lam_map_569 (l0, l1) =
  case l0 of Some69_0 l2 -> Some72_0 (dispatch_794 (l1, l2)); None69_1 -> None72_1

lam_map_566 :: ((Parse70, Closure177), (Int64, (Vector Word8) )) -> Option72
lam_map_566 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_567 (l2, l3, l0) in lam_map_569 (l4, Variant489_0 l1)

dispatch_795 :: (Closure175, (Int64, (Vector Word8) )) -> Option72
dispatch_795 (l0, l1) =
  case l0 of Variant175_0 l2 -> lam_pure_570 (l2, l1)

lam_parse_from_574 :: (Int64, (Vector Word8) , Parse73) -> Option72
lam_parse_from_574 (l0, l1, l2) =
  let Parse73_0 l3 = l2 in dispatch_795 (l3, (l0, l1))

lam_lazy_573 :: (Closure179, (Int64, (Vector Word8) )) -> Option72
lam_lazy_573 (l0, (l1, l2)) =
  lam_parse_from_574 (l1, l2, lam_optional_571 ())

dispatch_796 :: (Closure176, (Int64, (Vector Word8) )) -> Option72
dispatch_796 (l0, l1) =
  case l0 of Variant176_0 l2 -> lam_map_566 (l2, l1)

lam_parse_from_576 :: (Int64, (Vector Word8) , Parse74) -> Option72
lam_parse_from_576 (l0, l1, l2) =
  let Parse74_0 l3 = l2 in dispatch_796 (l3, (l0, l1))

dispatch_797 :: (Closure178, (Int64, (Vector Word8) )) -> Option72
dispatch_797 (l0, l1) =
  case l0 of Variant178_0 l2 -> lam_lazy_573 (l2, l1)

lam_parse_from_578 :: (Int64, (Vector Word8) , Parse75) -> Option72
lam_parse_from_578 (l0, l1, l2) =
  let Parse75_0 l3 = l2 in dispatch_797 (l3, (l0, l1))

lam_or_577 :: ((Int64, (Vector Word8) , Parse75), ()) -> Option72
lam_or_577 ((l0, l1, l2), ()) =
  lam_parse_from_578 (l0, l1, l2)

dispatch_798 :: (Closure490, ()) -> Option72
dispatch_798 (l0, l1) =
  case l0 of Variant490_0 l2 -> lam_or_577 (l2, l1)

lam_or_else_579 :: (Option72, Closure490) -> Option72
lam_or_else_579 (l0, l1) =
  case l0 of Some72_0 l2 -> Some72_0 l2; None72_1 -> dispatch_798 (l1, ())

lam_or_575 :: ((Parse74, Parse75), (Int64, (Vector Word8) )) -> Option72
lam_or_575 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_576 (l2, l3, l0) in lam_or_else_579 (l4, Variant490_0 (l2, l3, l1))

dispatch_799 :: (Closure180, (Int64, (Vector Word8) )) -> Option72
dispatch_799 (l0, l1) =
  case l0 of Variant180_0 l2 -> lam_or_575 (l2, l1)

lam_parse_from_585 :: (Int64, (Vector Word8) , Parse76) -> Option72
lam_parse_from_585 (l0, l1, l2) =
  let Parse76_0 l3 = l2 in dispatch_799 (l3, (l0, l1))

dispatch_800 :: (Closure173, Option0) -> Parse71
dispatch_800 (l0, l1) =
  case l0 of Variant173_0 l2 -> lam_many0_fold_580 (l2, l1)

dispatch_802 :: (Closure491, (Int64, Option0)) -> Option69
dispatch_802 (l0, l1) =
  case l0 of Variant491_0 l2 -> lam_and_then_586 (l2, l1)

lam_and_then_586 ((l0, l1), (l2, l3)) =
  lam_parse_from_587 (l2, l0, dispatch_800 (l1, l3))

lam_parse_from_587 (l0, l1, l2) =
  let Parse71_0 l3 = l2 in dispatch_801 (l3, (l0, l1))

dispatch_801 (l0, l1) =
  case l0 of Variant172_0 l2 -> lam_pure_582 (l2, l1); Variant172_1 l2 -> lam_and_then_584 (l2, l1)

lam_and_then_584 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_585 (l2, l3, l0) in lam_and_then_588 (l4, Variant491_0 (l3, l1))

lam_and_then_588 (l0, l1) =
  case l0 of Some72_0 l2 -> dispatch_802 (l1, l2); None72_1 -> None69_1

dispatch_803 :: (Closure182, Int64) -> Parse71
dispatch_803 (l0, l1) =
  case l0 of Variant182_0 l2 -> lam_many1_fold_593 (l2, l1)

lam_and_then_596 :: (((Vector Word8) , Closure182), (Int64, Int64)) -> Option69
lam_and_then_596 ((l0, l1), (l2, l3)) =
  lam_parse_from_587 (l2, l0, dispatch_803 (l1, l3))

dispatch_804 :: (Closure492, (Int64, Int64)) -> Option69
dispatch_804 (l0, l1) =
  case l0 of Variant492_0 l2 -> lam_and_then_596 (l2, l1)

lam_and_then_597 :: (Option69, Closure492) -> Option69
lam_and_then_597 (l0, l1) =
  case l0 of Some69_0 l2 -> dispatch_804 (l1, l2); None69_1 -> None69_1

lam_and_then_595 :: ((Parse70, Closure182), (Int64, (Vector Word8) )) -> Option69
lam_and_then_595 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_567 (l2, l3, l0) in lam_and_then_597 (l4, Variant492_0 (l3, l1))

dispatch_805 :: (Closure181, (Int64, (Vector Word8) )) -> Option69
dispatch_805 (l0, l1) =
  case l0 of Variant181_0 l2 -> lam_and_then_595 (l2, l1)

lam_parse_from_600 :: (Int64, (Vector Word8) , Parse77) -> Option69
lam_parse_from_600 (l0, l1, l2) =
  let Parse77_0 l3 = l2 in dispatch_805 (l3, (l0, l1))

dispatch_806 :: (Closure493, (Int64, Int64)) -> (Int64, Expr9)
dispatch_806 (l0, l1) =
  case l0 of Variant493_0 l2 -> lam_map_601 (l2, l1)

lam_map_602 :: (Option69, Closure493) -> Option28
lam_map_602 (l0, l1) =
  case l0 of Some69_0 l2 -> Some28_0 (dispatch_806 (l1, l2)); None69_1 -> None28_1

lam_map_599 :: ((Parse77, Closure188), (Int64, (Vector Word8) )) -> Option28
lam_map_599 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_600 (l2, l3, l0) in lam_map_602 (l4, Variant493_0 l1)

dispatch_810 :: (Closure187, (Int64, (Vector Word8) )) -> Option28
dispatch_810 (l0, l1) =
  case l0 of Variant187_0 l2 -> lam_map_599 (l2, l1)

lam_parse_from_624 :: (Int64, (Vector Word8) , Parse81) -> Option28
lam_parse_from_624 (l0, l1, l2) =
  let Parse81_0 l3 = l2 in dispatch_810 (l3, (l0, l1))

dispatch_813 :: (Closure183, (Int64, (Vector Word8) )) -> Option28
dispatch_813 (l0, l1) =
  case l0 of Variant183_0 l2 -> lam_skip_pre_384 (l2, l1)

lam_skip_pre_384 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_322 (l2, l3, l0) in lam_and_then_387 (l4, Variant462_0 (l3, l1))

lam_and_then_387 (l0, l1) =
  case l0 of Some11_0 l2 -> dispatch_725 (l1, l2); None11_1 -> None28_1

dispatch_725 (l0, l1) =
  case l0 of Variant462_0 l2 -> lam_skip_pre_385 (l2, l1)

lam_skip_pre_385 ((l0, l1), (l2, l_0)) =
  lam_parse_from_386 (l2, l0, l1)

lam_parse_from_386 (l0, l1, l2) =
  let Parse31_0 l3 = l2 in dispatch_724 (l3, (l0, l1))

dispatch_724 (l0, l1) =
  case l0 of Variant119_0 l2 -> lam_skip_post_378 (l2, l1)

lam_skip_post_378 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_379 (l2, l3, l0) in lam_and_then_383 (l4, Variant460_0 (l3, l1))

lam_parse_from_379 (l0, l1, l2) =
  let Parse30_0 l3 = l2 in dispatch_721 (l3, (l0, l1))

dispatch_721 (l0, l1) =
  case l0 of Variant118_0 l2 -> lam_skip_pre_374 (l2, l1)

lam_skip_pre_374 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_370 (l2, l3, l0) in lam_and_then_377 (l4, Variant459_0 (l3, l1))

lam_and_then_377 (l0, l1) =
  case l0 of Some17_0 l2 -> dispatch_720 (l1, l2); None17_1 -> None28_1

dispatch_720 (l0, l1) =
  case l0 of Variant459_0 l2 -> lam_skip_pre_375 (l2, l1)

lam_skip_pre_375 ((l0, l1), (l2, l_0)) =
  lam_parse_from_376 (l2, l0, l1)

lam_parse_from_376 (l0, l1, l2) =
  let Parse29_0 l3 = l2 in dispatch_719 (l3, (l0, l1))

dispatch_719 (l0, l1) =
  case l0 of Variant117_0 l2 -> lam_skip_post_367 (l2, l1)

lam_skip_post_367 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_368 (l2, l3, l0) in lam_and_then_373 (l4, Variant457_0 (l3, l1))

lam_parse_from_368 (l0, l1, l2) =
  let Parse26_0 l3 = l2 in dispatch_715 (l3, (l0, l1))

dispatch_715 (l0, l1) =
  case l0 of Variant114_0 l2 -> lam_skip_pre_613 (l2, l1)

lam_skip_pre_613 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_370 (l2, l3, l0) in lam_and_then_616 (l4, Variant494_0 (l3, l1))

lam_and_then_616 (l0, l1) =
  case l0 of Some17_0 l2 -> dispatch_809 (l1, l2); None17_1 -> None28_1

dispatch_809 (l0, l1) =
  case l0 of Variant494_0 l2 -> lam_skip_pre_614 (l2, l1)

lam_skip_pre_614 ((l0, l1), (l2, l_0)) =
  lam_parse_from_615 (l2, l0, l1)

lam_parse_from_615 (l0, l1, l2) =
  let Parse80_0 l3 = l2 in dispatch_808 (l3, (l0, l1))

dispatch_808 (l0, l1) =
  case l0 of Variant186_0 l2 -> lam_skip_post_610 (l2, l1)

lam_skip_post_610 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_611 (l2, l3, l0) in lam_and_then_373 (l4, Variant457_0 (l3, l1))

lam_parse_from_611 (l0, l1, l2) =
  let Parse79_0 l3 = l2 in dispatch_807 (l3, (l0, l1))

dispatch_807 (l0, l1) =
  case l0 of Variant184_0 l2 -> lam_and_then_550 (l2, l1)

lam_and_then_550 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_504 (l2, l3, l0) in lam_and_then_552 (l4, Variant486_0 (l3, l1))

lam_and_then_552 (l0, l1) =
  case l0 of Some28_0 l2 -> dispatch_787 (l1, l2); None28_1 -> None28_1

dispatch_787 (l0, l1) =
  case l0 of Variant486_0 l2 -> lam_and_then_551 (l2, l1)

lam_and_then_551 ((l0, l1), (l2, l3)) =
  lam_parse_from_538 (l2, l0, dispatch_786 (l1, l3))

lam_parse_from_538 (l0, l1, l2) =
  let Parse61_0 l3 = l2 in dispatch_784 (l3, (l0, l1))

dispatch_784 (l0, l1) =
  case l0 of Variant158_0 l2 -> lam_pure_447 (l2, l1); Variant158_1 l2 -> lam_and_then_535 (l2, l1)

lam_and_then_535 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_536 (l2, l3, l0) in lam_and_then_539 (l4, Variant485_0 (l3, l1))

lam_and_then_539 (l0, l1) =
  case l0 of Some63_0 l2 -> dispatch_785 (l1, l2); None63_1 -> None28_1

dispatch_785 (l0, l1) =
  case l0 of Variant485_0 l2 -> lam_and_then_537 (l2, l1)

lam_and_then_537 ((l0, l1), (l2, l3)) =
  lam_parse_from_538 (l2, l0, dispatch_783 (l1, l3))

lam_parse_from_536 (l0, l1, l2) =
  let Parse67_0 l3 = l2 in dispatch_782 (l3, (l0, l1))

dispatch_782 (l0, l1) =
  case l0 of Variant166_0 l2 -> lam_or_527 (l2, l1)

lam_or_527 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_528 (l2, l3, l0) in lam_or_else_531 (l4, Variant484_0 (l2, l3, l1))

lam_parse_from_528 (l0, l1, l2) =
  let Parse65_0 l3 = l2 in dispatch_779 (l3, (l0, l1))

dispatch_779 (l0, l1) =
  case l0 of Variant162_0 l2 -> lam_map_518 (l2, l1)

lam_map_518 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_519 (l2, l3, l0) in lam_map_521 (l4, Variant483_0 l1)

lam_parse_from_519 (l0, l1, l2) =
  let Parse60_0 l3 = l2 in dispatch_776 (l3, (l0, l1))

dispatch_776 (l0, l1) =
  case l0 of Variant156_0 l2 -> lam_and_then_510 (l2, l1)

lam_and_then_510 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_511 (l2, l3, l0) in lam_and_then_514 (l4, Variant482_0 (l3, l1))

lam_and_then_514 (l0, l1) =
  case l0 of Some50_0 l2 -> dispatch_774 (l1, l2); None50_1 -> None55_1

dispatch_774 (l0, l1) =
  case l0 of Variant482_0 l2 -> lam_and_then_512 (l2, l1)

lam_and_then_512 ((l0, l1), (l2, l3)) =
  lam_parse_from_513 (l2, l0, dispatch_772 (l1, l3))

lam_parse_from_513 (l0, l1, l2) =
  let Parse58_0 l3 = l2 in dispatch_773 (l3, (l0, l1))

dispatch_773 (l0, l1) =
  case l0 of Variant153_0 l2 -> lam_and_then_503 (l2, l1)

lam_and_then_503 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_504 (l2, l3, l0) in lam_and_then_507 (l4, Variant481_0 (l3, l1))

lam_parse_from_504 (l0, l1, l2) =
  let Parse57_0 l3 = l2 in dispatch_767 (l3, (l0, l1))

dispatch_767 (l0, l1) =
  case l0 of Variant151_0 l2 -> lam_and_then_472 (l2, l1)

lam_and_then_472 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_417 (l2, l3, l0) in lam_and_then_474 (l4, Variant474_0 (l3, l1))

lam_and_then_474 (l0, l1) =
  case l0 of Some28_0 l2 -> dispatch_756 (l1, l2); None28_1 -> None28_1

dispatch_756 (l0, l1) =
  case l0 of Variant474_0 l2 -> lam_and_then_473 (l2, l1)

lam_and_then_473 ((l0, l1), (l2, l3)) =
  lam_parse_from_452 (l2, l0, dispatch_755 (l1, l3))

lam_parse_from_452 (l0, l1, l2) =
  let Parse43_0 l3 = l2 in dispatch_753 (l3, (l0, l1))

dispatch_753 (l0, l1) =
  case l0 of Variant134_0 l2 -> lam_pure_447 (l2, l1); Variant134_1 l2 -> lam_and_then_449 (l2, l1)

lam_and_then_449 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_450 (l2, l3, l0) in lam_and_then_453 (l4, Variant473_0 (l3, l1))

lam_and_then_453 (l0, l1) =
  case l0 of Some45_0 l2 -> dispatch_754 (l1, l2); None45_1 -> None28_1

dispatch_754 (l0, l1) =
  case l0 of Variant473_0 l2 -> lam_and_then_451 (l2, l1)

lam_and_then_451 ((l0, l1), (l2, l3)) =
  lam_parse_from_452 (l2, l0, dispatch_752 (l1, l3))

lam_parse_from_450 (l0, l1, l2) =
  let Parse49_0 l3 = l2 in dispatch_751 (l3, (l0, l1))

dispatch_751 (l0, l1) =
  case l0 of Variant142_0 l2 -> lam_or_440 (l2, l1)

lam_or_440 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_441 (l2, l3, l0) in lam_or_else_444 (l4, Variant472_0 (l2, l3, l1))

lam_parse_from_441 (l0, l1, l2) =
  let Parse47_0 l3 = l2 in dispatch_748 (l3, (l0, l1))

dispatch_748 (l0, l1) =
  case l0 of Variant138_0 l2 -> lam_map_431 (l2, l1)

lam_map_431 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_432 (l2, l3, l0) in lam_map_434 (l4, Variant471_0 l1)

lam_parse_from_432 (l0, l1, l2) =
  let Parse42_0 l3 = l2 in dispatch_745 (l3, (l0, l1))

dispatch_745 (l0, l1) =
  case l0 of Variant132_0 l2 -> lam_and_then_423 (l2, l1)

lam_and_then_423 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_424 (l2, l3, l0) in lam_and_then_427 (l4, Variant470_0 (l3, l1))

lam_and_then_427 (l0, l1) =
  case l0 of Some32_0 l2 -> dispatch_743 (l1, l2); None32_1 -> None37_1

dispatch_743 (l0, l1) =
  case l0 of Variant470_0 l2 -> lam_and_then_425 (l2, l1)

lam_and_then_425 ((l0, l1), (l2, l3)) =
  lam_parse_from_426 (l2, l0, dispatch_741 (l1, l3))

lam_parse_from_426 (l0, l1, l2) =
  let Parse40_0 l3 = l2 in dispatch_742 (l3, (l0, l1))

dispatch_742 (l0, l1) =
  case l0 of Variant129_0 l2 -> lam_and_then_416 (l2, l1)

lam_and_then_416 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_417 (l2, l3, l0) in lam_and_then_420 (l4, Variant469_0 (l3, l1))

lam_parse_from_417 (l0, l1, l2) =
  let Parse39_0 l3 = l2 in dispatch_736 (l3, (l0, l1))

dispatch_736 (l0, l1) =
  case l0 of Variant128_0 l2 -> lam_or_623 (l2, l1)

lam_or_623 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_624 (l2, l3, l0) in lam_or_else_627 (l4, Variant495_0 (l2, l3, l1))

lam_or_else_627 (l0, l1) =
  case l0 of Some28_0 l2 -> Some28_0 l2; None28_1 -> dispatch_812 (l1, ())

dispatch_812 (l0, l1) =
  case l0 of Variant495_0 l2 -> lam_or_625 (l2, l1)

lam_or_625 ((l0, l1, l2), ()) =
  lam_parse_from_626 (l0, l1, l2)

lam_parse_from_626 (l0, l1, l2) =
  let Parse82_0 l3 = l2 in dispatch_811 (l3, (l0, l1))

dispatch_811 (l0, l1) =
  case l0 of Variant189_0 l2 -> lam_lazy_636 (l2, l1)

lam_lazy_636 (l0, (l1, l2)) =
  lam_parse_from_637 (l1, l2, lam_atomic_expr_603 ())

lam_parse_from_637 (l0, l1, l2) =
  let Parse78_0 l3 = l2 in dispatch_813 (l3, (l0, l1))

lam_parse_prefix_645 :: ((Vector Word8) , Parse26) -> Option28
lam_parse_prefix_645 (l0, l1) =
  lam_parse_from_368 (0, l0, l1)

dispatch_814 :: (Closure496, (Int64, Expr9)) -> Option10
dispatch_814 (l0, l1) =
  case l0 of Variant496_0 l2 -> lam_parse_all_646 (l2, l1)

lam_and_then_647 :: (Option28, Closure496) -> Option10
lam_and_then_647 (l0, l1) =
  case l0 of Some28_0 l2 -> dispatch_814 (l1, l2); None28_1 -> None10_1

lam_parse_all_644 :: ((Vector Word8) , Parse26) -> Option10
lam_parse_all_644 (l0, l1) =
  let l2 = (let l2 = l0 in lam_parse_prefix_645 (l2, l1)) in lam_and_then_647 (l2, Variant496_0 l0)

lam_eval_exprs_309 :: (Vector Word8)  -> Int64
lam_eval_exprs_309 l0 =
  let l1 = (let l1 = (let l1 = l0 in lam_parse_all_644 (l1, expr_193 ())) in lam_unwrap_648 l1) in lam_wrapped_eval_649 l1

dispatch_815 :: (Closure94, ()) -> Option8
dispatch_815 (l0, l1) =
  case l0 of Variant94_0 l2 -> lam_map_306 (l2, l1)

lam_next_653 :: Iter7 -> Option8
lam_next_653 l0 =
  let Iter7_0 l1 = l0 in dispatch_815 (l1, ())

lam_map_652 :: ((Iter7, Closure192), ()) -> Option84
lam_map_652 ((l0, l1), ()) =
  case lam_next_653 l0 of Some8_0 (l2, l3) -> Some84_0 (lam_eval_exprs_309 l2, lam_map_651 (l3, l1)); None8_1 -> None84_1

dispatch_816 :: (Closure191, ()) -> Option84
dispatch_816 (l0, l1) =
  case l0 of Variant191_0 l2 -> lam_map_652 (l2, l1)

lam_next_659 :: Iter83 -> Option84
lam_next_659 l0 =
  let Iter83_0 l1 = l0 in dispatch_816 (l1, ())

lam_foldl_658 :: (Iter83, (Vector Int64) , Closure418) -> (Vector Int64) 
lam_foldl_658 (l0, l1, l2) =
  case lam_next_659 l0 of Some84_0 (l3, l4) -> lam_foldl_658 (l4, intrinsicPush l1 l3, l2); None84_1 -> l1

lam_wrapped_foldl_657 :: (Iter83, (Vector Int64) , Closure418) -> (Vector Int64) 
lam_wrapped_foldl_657 l0 =
  lam_foldl_658 l0

lam_from_iter_with_capacity_656 :: (Iter83, Int64) -> (Vector Int64) 
lam_from_iter_with_capacity_656 (l0, l1) =
  lam_wrapped_foldl_657 (l0, intrinsicReserve ((V.fromList [])) l1, push_248 ())

lam_from_iter_655 :: Iter83 -> (Vector Int64) 
lam_from_iter_655 l0 =
  lam_from_iter_with_capacity_656 (l0, 0)

lam_eval_exprs_308 :: Iter7 -> (Vector Int64) 
lam_eval_exprs_308 l0 =
  let l1 = (let l1 = l0 in lam_wrapped_map_654 (l1, Variant192_0 ())) in lam_from_iter_655 l1

lam_main_301 :: ((Vector ((Vector Word8) )) , ()) -> (Vector Int64) 
lam_main_301 (l0, ()) =
  lam_eval_exprs_308 (lam_items_660 l0)

dispatch_817 :: (Closure445, ()) -> (Vector Int64) 
dispatch_817 (l0, l1) =
  case l0 of Variant445_0 l2 -> lam_main_301 (l2, l1)

lam_repeat_664 :: (Int64, Closure445) -> Option85
lam_repeat_664 (l0, l1) =
  case uncurry (<) (l0, 1) of True -> None85_1; False -> (let l2 = dispatch_817 (l1, ()) in (case uncurry (==) (l0, 1) of True -> Some85_0 l2; False -> lam_repeat_664 (uncurry (-) (l0, 1), l1)))

lam_wrapped_repeat_663 :: (Int64, Closure445) -> Option85
lam_wrapped_repeat_663 l0 =
  lam_repeat_664 l0

dispatch_818 :: (Closure195, Int64) -> Int64
dispatch_818 (l0, l1) =
  case l0 of Variant195_0 l2 -> lam_items_667 (l2, l1)

dispatch_819 :: (Closure193, ()) -> Option87
dispatch_819 (l0, l1) =
  case l0 of Variant193_0 l2 -> lam_range_666 (l2, l1)

lam_next_670 :: Iter86 -> Option87
lam_next_670 l0 =
  let Iter86_0 l1 = l0 in dispatch_819 (l1, ())

lam_map_669 :: ((Iter86, Closure195), ()) -> Option89
lam_map_669 ((l0, l1), ()) =
  case lam_next_670 l0 of Some87_0 (l2, l3) -> Some89_0 (dispatch_818 (l1, l2), lam_map_668 (l3, l1)); None87_1 -> None89_1

dispatch_820 :: (Closure194, ()) -> Option89
dispatch_820 (l0, l1) =
  case l0 of Variant194_0 l2 -> lam_map_669 (l2, l1)

lam_next_685 :: Iter88 -> Option89
lam_next_685 l0 =
  let Iter88_0 l1 = l0 in dispatch_820 (l1, ())

lam_for_each_684 :: (Iter88, Closure446) -> ()
lam_for_each_684 (l0, l1) =
  case lam_next_685 l0 of Some89_0 (l2, l3) -> (let l_0 = lam_main_674 l2 in l_0 `seq` lam_for_each_684 (l3, l1)); None89_1 -> ()

lam_wrapped_for_each_683 :: (Iter88, Closure446) -> ()
lam_wrapped_for_each_683 l0 =
  lam_for_each_684 l0

lam_main_277 :: () -> ()
lam_main_277 () =
  case lam_string_to_nat_278 (input ()) of Some0_0 l0 -> (let l1 = lam_read_input_298 () in (case lam_wrapped_repeat_663 (l0, Variant445_0 l1) of Some85_0 l2 -> (let l3 = lam_items_671 l2 in lam_wrapped_for_each_683 (l3, Variant446_0 ())); None85_1 -> ())); None0_1 -> lam_writeln_675 ((V.fromList [80, 108, 101, 97, 115, 101, 32, 101, 110, 116, 101, 114, 32, 97, 110, 32, 105, 116, 101, 114, 97, 116, 105, 111, 110, 32, 99, 111, 117, 110, 116]))

dispatch_821 :: (Closure444, ()) -> ()
dispatch_821 (l0, l1) =
  case l0 of Variant444_0 l2 -> lam_main_277 l1

main_wrapper_686 :: () -> ()
main_wrapper_686 () =
  dispatch_821 (main_276 (), ())


main :: IO ()
main = main_wrapper_686 () `seq` return ()
