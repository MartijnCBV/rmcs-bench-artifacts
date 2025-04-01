
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

data VarId6
  = VarId6_0 Int64

data HeadId8
  = HeadId8_0 Int64

data Expr7
  = EVar7_0 VarId6
  | EHead7_1 (HeadId8, (Vector Expr7) )

data Problem5
  = Problem5_0 (Int64, VarId6, (Vector ((VarId6, Expr7))) )

data Option9
  = Some9_0 (Int64, Word8)
  | None9_1

data Option13
  = Some13_0 (Int64, Int64)
  | None13_1

data Option16
  = Some16_0 (Int64, Option0)
  | None16_1

data Option22
  = Some22_0 (Int64, VarId6)
  | None22_1

data Option26
  = Some26_0 (Int64, Expr7)
  | None26_1

data Option27
  = Some27_0 (Int64, HeadId8)
  | None27_1

data Option32
  = Some32_0 Expr7
  | None32_1

data Option33
  = Some33_0 (Int64, Option32)
  | None33_1

data Option37
  = Some37_0 (Int64, (Vector Expr7) )
  | None37_1

data Option47
  = Some47_0 (Int64, (VarId6, Expr7))
  | None47_1

data Option51
  = Some51_0 (VarId6, Expr7)
  | None51_1

data Option53
  = Some53_0 (Int64, Option51)
  | None53_1

data Option57
  = Some57_0 (Int64, (Vector ((VarId6, Expr7))) )
  | None57_1

data Option64
  = Some64_0 (Int64, Problem5)
  | None64_1

data Option72
  = Some72_0 Problem5
  | None72_1

data Option73
  = Some73_0 (Vector Option32) 
  | None73_1

data Term83
  = Unknown83_0
  | Equal83_1 VarId6
  | Term83_2 (HeadId8, (Vector VarId6) )
  | Recursing83_3

data Terms82
  = Terms82_0 (Vector Term83) 

data Closure146
  = Variant146_0 (Int64, Int64)

data Iter1
  = Iter1_0 Closure146

data Option2
  = Some2_0 (Int64, Iter1)
  | None2_1

data Closure148
  = Variant148_0 (Vector Word8) 

data Closure147
  = Variant147_0 (Iter1, Closure148)

data Iter3
  = Iter3_0 Closure147

data Option4
  = Some4_0 (Word8, Iter3)
  | None4_1

data Closure149
  = Variant149_0 Word8
  | Variant149_1 ()

data Parse10
  = Parse10_0 Closure149

data Closure150
  = Variant150_0 ()

data Parse11
  = Parse11_0 Closure150

data Closure153
  = Variant153_0 (Word8, Word8)

data Closure152
  = Variant152_0 Closure153

data Closure151
  = Variant151_0 (Parse11, Closure152)

data Parse12
  = Parse12_0 Closure151

data Closure155
  = Variant155_0 ()

data Closure154
  = Variant154_0 (Parse12, Closure155)

data Parse14
  = Parse14_0 Closure154

data Closure158
  = Variant158_0 ()

data Closure157
  = Variant157_0 (Int64, Parse14, Closure158)

data Closure159
  = Variant159_0 Option0

data Parse17
  = Parse17_0 Closure159

data Closure161
  = Variant161_0 ()

data Closure160
  = Variant160_0 (Parse14, Closure161)

data Parse18
  = Parse18_0 Closure160

data Closure163
  = Variant163_0 ()

data Closure162
  = Variant162_0 Closure163

data Parse19
  = Parse19_0 Closure162

data Closure164
  = Variant164_0 (Parse18, Parse19)

data Parse20
  = Parse20_0 Closure164

data Closure156
  = Variant156_0 Int64
  | Variant156_1 (Parse20, Closure157)

data Parse15
  = Parse15_0 Closure156

data Closure166
  = Variant166_0 (Parse14, Closure158, Int64)

data Closure165
  = Variant165_0 (Parse14, Closure166)

data Parse21
  = Parse21_0 Closure165

data Closure169
  = Variant169_0 Word8

data Closure168
  = Variant168_0 Closure169

data Closure167
  = Variant167_0 (Parse11, Closure168)

data Parse23
  = Parse23_0 Closure167

data Closure171
  = Variant171_0 ()

data Closure170
  = Variant170_0 (Parse21, Closure171)

data Parse24
  = Parse24_0 Closure170

data Closure172
  = Variant172_0 (Parse23, Parse24)

data Parse25
  = Parse25_0 Closure172

data Closure174
  = Variant174_0 ()

data Closure173
  = Variant173_0 (Parse21, Closure174)

data Parse28
  = Parse28_0 Closure173

data Closure177
  = Variant177_0 ()

data Closure176
  = Variant176_0 Closure177

data Parse30
  = Parse30_0 Closure176

data Closure180
  = Variant180_0

data Closure179
  = Variant179_0 ((Vector Expr7) , Parse30, Closure180)

data Closure181
  = Variant181_0 Option32

data Parse34
  = Parse34_0 Closure181

data Closure183
  = Variant183_0 ()

data Closure182
  = Variant182_0 (Parse30, Closure183)

data Parse35
  = Parse35_0 Closure182

data Closure185
  = Variant185_0 ()

data Closure184
  = Variant184_0 Closure185

data Parse36
  = Parse36_0 Closure184

data Closure186
  = Variant186_0 (Parse35, Parse36)

data Parse38
  = Parse38_0 Closure186

data Closure178
  = Variant178_0 (Vector Expr7) 
  | Variant178_1 (Parse38, Closure179)

data Parse31
  = Parse31_0 Closure178

data Closure187
  = Variant187_0 Expr7

data Parse39
  = Parse39_0 Closure187

data Closure189
  = Variant189_0 Parse30

data Closure188
  = Variant188_0 Closure189

data Parse40
  = Parse40_0 Closure188

data Closure191
  = Variant191_0 HeadId8

data Closure190
  = Variant190_0 (Parse40, Closure191)

data Parse41
  = Parse41_0 Closure190

data Closure192
  = Variant192_0 (Parse41, Parse23)

data Parse42
  = Parse42_0 Closure192

data Closure175
  = Variant175_0 (Parse23, Parse42)

data Parse29
  = Parse29_0 Closure175

data Closure195
  = Variant195_0 ()

data Closure194
  = Variant194_0 (Parse25, Closure195)

data Parse44
  = Parse44_0 Closure194

data Closure197
  = Variant197_0 ()

data Closure198
  = Variant198_0 (Parse23, Parse28)

data Parse46
  = Parse46_0 Closure198

data Closure196
  = Variant196_0 (Parse46, Closure197)

data Parse45
  = Parse45_0 Closure196

data Closure193
  = Variant193_0 (Parse44, Parse45)

data Parse43
  = Parse43_0 Closure193

data Closure199
  = Variant199_0 (VarId6, Expr7)

data Parse48
  = Parse48_0 Closure199

data Closure201
  = Variant201_0 VarId6

data Closure200
  = Variant200_0 (Parse30, Closure201)

data Parse49
  = Parse49_0 Closure200

data Closure202
  = Variant202_0 (Parse23, Parse49)

data Parse50
  = Parse50_0 Closure202

data Closure204
  = Variant204_0 ()

data Closure203
  = Variant203_0 (Parse25, Closure204)

data Parse52
  = Parse52_0 Closure203

data Closure205
  = Variant205_0 Option51

data Parse54
  = Parse54_0 Closure205

data Closure207
  = Variant207_0 ()

data Closure206
  = Variant206_0 (Parse52, Closure207)

data Parse55
  = Parse55_0 Closure206

data Closure209
  = Variant209_0 ()

data Closure208
  = Variant208_0 Closure209

data Parse56
  = Parse56_0 Closure208

data Closure210
  = Variant210_0 (Parse23, Parse52)

data Parse58
  = Parse58_0 Closure210

data Closure213
  = Variant213_0

data Closure212
  = Variant212_0 ((Vector ((VarId6, Expr7))) , Parse58, Closure213)

data Closure214
  = Variant214_0 (Parse58, Closure207)

data Parse60
  = Parse60_0 Closure214

data Closure215
  = Variant215_0 (Parse60, Parse56)

data Parse61
  = Parse61_0 Closure215

data Closure211
  = Variant211_0 (Vector ((VarId6, Expr7))) 
  | Variant211_1 (Parse61, Closure212)

data Parse59
  = Parse59_0 Closure211

data Closure216
  = Variant216_0 (Parse55, Parse56)

data Parse62
  = Parse62_0 Closure216

data Closure218
  = Variant218_0 ((Vector ((VarId6, Expr7))) , Parse23, Parse52, Closure213)

data Closure217
  = Variant217_0 (Parse62, Closure218)

data Parse63
  = Parse63_0 Closure217

data Closure219
  = Variant219_0 Problem5

data Parse65
  = Parse65_0 Closure219

data Closure221
  = Variant221_0 (Parse52, Parse23)

data Closure220
  = Variant220_0 Closure221

data Parse66
  = Parse66_0 Closure220

data Closure223
  = Variant223_0 (Int64, VarId6)

data Closure222
  = Variant222_0 (Parse66, Closure223)

data Parse67
  = Parse67_0 Closure222

data Closure224
  = Variant224_0 (Parse23, Parse67)

data Parse68
  = Parse68_0 Closure224

data Closure226
  = Variant226_0 Int64

data Closure225
  = Variant225_0 (Parse25, Closure226)

data Parse69
  = Parse69_0 Closure225

data Closure227
  = Variant227_0 (Parse23, Parse69)

data Parse70
  = Parse70_0 Closure227

data Closure229
  = Variant229_0 ()

data Closure228
  = Variant228_0 (Parse21, Closure229)

data Parse71
  = Parse71_0 Closure228

data Closure230
  = Variant230_0 (Int64, Int64)

data Iter74
  = Iter74_0 Closure230

data Option75
  = Some75_0 (Int64, Iter74)
  | None75_1

data Closure232
  = Variant232_0 (Vector Problem5) 

data Closure231
  = Variant231_0 (Iter74, Closure232)

data Iter76
  = Iter76_0 Closure231

data Option77
  = Some77_0 (Problem5, Iter76)
  | None77_1

data Closure233
  = Variant233_0 (Int64, Int64)

data Iter78
  = Iter78_0 Closure233

data Option79
  = Some79_0 (Int64, Iter78)
  | None79_1

data Closure235
  = Variant235_0 (Vector ((VarId6, Expr7))) 

data Closure234
  = Variant234_0 (Iter78, Closure235)

data Iter80
  = Iter80_0 Closure234

data Option81
  = Some81_0 ((VarId6, Expr7), Iter80)
  | None81_1

data Closure236
  = Variant236_0 (Vector VarId6) 

data State84
  = State84_0 Closure236

data Closure238
  = Variant238_0 HeadId8

data Closure239
  = Variant239_0 (Int64, Int64)

data Iter86
  = Iter86_0 Closure239

data Option87
  = Some87_0 (Int64, Iter86)
  | None87_1

data Closure241
  = Variant241_0 (Vector Expr7) 

data Closure240
  = Variant240_0 (Iter86, Closure241)

data Iter88
  = Iter88_0 Closure240

data Option89
  = Some89_0 (Expr7, Iter88)
  | None89_1

data Closure243
  = Variant243_0 (Vector VarId6) 

data Closure246
  = Variant246_0 ()

data Closure245
  = Variant245_0 (Iter88, Closure246)

data Closure244
  = Variant244_0 (Vector VarId6) 
  | Variant244_1 (State90, Closure245)

data State90
  = State90_0 Closure242

data Closure242
  = Variant242_0 (State85, Closure243)

data State85
  = State85_0 Closure237

data Closure237
  = Variant237_0 VarId6
  | Variant237_1 (State91, Closure238)

data State91
  = State91_0 Closure244

data Closure247
  = Variant247_0 Term83

data State92
  = State92_0 Closure247

data Closure248
  = Variant248_0 (Int64, Int64)

data Iter93
  = Iter93_0 Closure248

data Option94
  = Some94_0 (Int64, Iter93)
  | None94_1

data Closure250
  = Variant250_0 (Vector VarId6) 

data Closure249
  = Variant249_0 (Iter93, Closure250)

data Iter95
  = Iter95_0 Closure249

data Option96
  = Some96_0 (VarId6, Iter95)
  | None96_1

data Closure251
  = Variant251_0 (Iter93, Closure250)

data Iter97
  = Iter97_0 Closure251

data Option98
  = Some98_0 (VarId6, Iter97)
  | None98_1

data Closure252
  = Variant252_0 (Iter95, Iter97)

data Iter99
  = Iter99_0 Closure252

data Option100
  = Some100_0 ((VarId6, VarId6), Iter99)
  | None100_1

data Closure255
  = Variant255_0 ()

data Closure254
  = Variant254_0 (Iter99, Closure255)

data Closure257
  = Variant257_0 VarId6

data Closure258
  = Variant258_0 Bool

data State103
  = State103_0 Closure258

data Closure260
  = Variant260_0 (VarId6, Term83)

data Closure259
  = Variant259_0 Closure260

data State104
  = State104_0 Closure259

data Closure262
  = Variant262_0 ()

data Closure261
  = Variant261_0 (State104, Closure262)

data State105
  = State105_0 Closure261

data Closure264
  = Variant264_0 (VarId6, HeadId8, (Vector VarId6) )

data Closure263
  = Variant263_0 Bool
  | Variant263_1 (State104, Closure264)

data State106
  = State106_0 Closure263

data Closure266
  = Variant266_0 (VarId6, VarId6, HeadId8, (Vector VarId6) )

data Closure267
  = Variant267_0 VarId6

data State108
  = State108_0 Closure267

data Closure269
  = Variant269_0 VarId6

data Closure268
  = Variant268_0 VarId6
  | Variant268_1 (State104, Closure269)

data State109
  = State109_0 Closure268

data Closure271
  = Variant271_0 VarId6

data Closure272
  = Variant272_0 Term83

data State111
  = State111_0 Closure272

data Closure273
  = Variant273_0 ()

data State112
  = State112_0 Closure273

data Closure275
  = Variant275_0 (VarId6, VarId6)

data Closure277
  = Variant277_0 VarId6

data Closure276
  = Variant276_0 (State112, Closure277)

data State114
  = State114_0 Closure276

data Closure270
  = Variant270_0 (State114, Closure271)

data State110
  = State110_0 Closure270

data Closure256
  = Variant256_0 (State110, Closure257)

data State102
  = State102_0 Closure256

data Closure253
  = Variant253_0 Bool
  | Variant253_1 (State102, Closure254)

data State101
  = State101_0 Closure253

data Closure265
  = Variant265_0 Bool
  | Variant265_1 (State101, Closure266)

data State107
  = State107_0 Closure265

data Closure274
  = Variant274_0 VarId6
  | Variant274_1 (State110, Closure275)

data State113
  = State113_0 Closure274

data Closure279
  = Variant279_0 VarId6

data Closure281
  = Variant281_0 VarId6

data Closure280
  = Variant280_0 (State114, Closure281)

data State116
  = State116_0 Closure280

data Closure278
  = Variant278_0 (State116, Closure279)

data State115
  = State115_0 Closure278

data Closure282
  = Variant282_0 VarId6
  | Variant282_1 (State116, Closure275)

data State117
  = State117_0 Closure282

data Closure284
  = Variant284_0 (VarId6, VarId6)

data Closure283
  = Variant283_0 Bool
  | Variant283_1 (State114, Closure284)

data State118
  = State118_0 Closure283

data Closure286
  = Variant286_0 (Term83, VarId6, VarId6)

data Closure285
  = Variant285_0 (State114, Closure286)

data State119
  = State119_0 Closure285

data Closure288
  = Variant288_0 ()

data Closure289
  = Variant289_0 ()

data Closure290
  = Variant290_0 (VarId6, HeadId8, HeadId8, (Vector VarId6) , (Vector VarId6) , VarId6)

data Closure287
  = Variant287_0 Bool
  | Variant287_1 (State104, Closure288)
  | Variant287_2 (State104, Closure289)
  | Variant287_3 (State104, Closure290)

data State120
  = State120_0 Closure287

data Closure292
  = Variant292_0 (HeadId8, HeadId8, (Vector VarId6) , (Vector VarId6) , VarId6, VarId6)

data Closure291
  = Variant291_0 (State104, Closure292)

data State121
  = State121_0 Closure291

data Closure294
  = Variant294_0 VarId6

data Closure293
  = Variant293_0 (State85, Closure294)

data State122
  = State122_0 Closure293

data Closure297
  = Variant297_0 ()

data Closure296
  = Variant296_0 (Iter80, Closure297)

data Closure295
  = Variant295_0 Bool
  | Variant295_1 (State122, Closure296)

data State123
  = State123_0 Closure295

data Closure298
  = Variant298_0 (Vector Expr7) 

data State124
  = State124_0 Closure298

data Closure300
  = Variant300_0 ()

data Closure301
  = Variant301_0 (Iter93, Closure250)

data Iter126
  = Iter126_0 Closure301

data Option127
  = Some127_0 (VarId6, Iter126)
  | None127_1

data Closure303
  = Variant303_0 (Vector Expr7) 

data Closure305
  = Variant305_0 VarId6

data Closure304
  = Variant304_0 (State114, Closure305)

data State129
  = State129_0 Closure304

data Closure299
  = Variant299_0 (State129, Closure300)

data State125
  = State125_0 Closure299

data Closure302
  = Variant302_0 (State125, Closure303)

data State128
  = State128_0 Closure302

data Closure306
  = Variant306_0 VarId6
  | Variant306_1 (State129, Closure275)

data State130
  = State130_0 Closure306

data Closure308
  = Variant308_0 VarId6

data Closure307
  = Variant307_0 (State114, Closure308)

data State131
  = State131_0 Closure307

data Closure310
  = Variant310_0 HeadId8

data Closure311
  = Variant311_0 Expr7

data State133
  = State133_0 Closure311

data Closure314
  = Variant314_0 ()

data Closure313
  = Variant313_0 (Iter126, Closure314)

data Closure312
  = Variant312_0 (Vector Expr7) 
  | Variant312_1 (State128, Closure313)

data State134
  = State134_0 Closure312

data Closure309
  = Variant309_0 Expr7
  | Variant309_1 (State134, Closure310)

data State132
  = State132_0 Closure309

data Closure315
  = Variant315_0 Option32

data State135
  = State135_0 Closure315

data Closure317
  = Variant317_0 ()

data Closure316
  = Variant316_0 Option32
  | Variant316_1 (State125, Closure317)

data State136
  = State136_0 Closure316

data Closure319
  = Variant319_0 VarId6

data Closure318
  = Variant318_0 (State123, Closure319)

data State137
  = State137_0 Closure318

data Closure321
  = Variant321_0 ()

data Closure320
  = Variant320_0 (Iter76, Closure321)

data Iter138
  = Iter138_0 Closure320

data Option139
  = Some139_0 (Option32, Iter138)
  | None139_1

data Closure322
  = Variant322_0 (Int64, Int64)

data Iter140
  = Iter140_0 Closure322

data Option141
  = Some141_0 (Int64, Iter140)
  | None141_1

data Closure324
  = Variant324_0 (Vector Option32) 

data Closure323
  = Variant323_0 (Iter140, Closure324)

data Iter142
  = Iter142_0 Closure323

data Option143
  = Some143_0 (Option32, Iter142)
  | None143_1

data Closure325
  = Variant325_0 (Iter86, Closure241)

data Iter144
  = Iter144_0 Closure325

data Option145
  = Some145_0 (Expr7, Iter144)
  | None145_1

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
  = Variant340_0

data Closure341
  = Variant341_0

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
  = Variant418_0 ()

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
  = Variant435_0 ()

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
  = Variant445_0 ()

data Closure446
  = Variant446_0 ()

data Closure447
  = Variant447_0 ()

data Closure448
  = Variant448_0 ()

data Closure449
  = Variant449_0 ()

data Closure450
  = Variant450_0 ()

data Closure451
  = Variant451_0 ()

data Closure452
  = Variant452_0 ()

data Closure453
  = Variant453_0 ()

data Closure454
  = Variant454_0 ()

data Closure455
  = Variant455_0 ()

data Closure456
  = Variant456_0 ()

data Closure457
  = Variant457_0 ()

data Closure458
  = Variant458_0 ()

data Closure459
  = Variant459_0 ()

data Closure460
  = Variant460_0 ()

data Closure461
  = Variant461_0 ()

data Closure462
  = Variant462_0 ()

data Closure463
  = Variant463_0 ()

data Closure464
  = Variant464_0 ()

data Closure465
  = Variant465_0 ()

data Closure466
  = Variant466_0 ()

data Closure467
  = Variant467_0 ()

data Closure468
  = Variant468_0 ()

data Closure469
  = Variant469_0 ()

data Closure470
  = Variant470_0 ()

data Closure471
  = Variant471_0 ()

data Closure472
  = Variant472_0 ()

data Closure473
  = Variant473_0 ()

data Closure474
  = Variant474_0 ()

data Closure475
  = Variant475_0 ()

data Closure476
  = Variant476_0 ()

data Closure477
  = Variant477_0 ()

data Closure478
  = Variant478_0 ()

data Closure479
  = Variant479_0 ()

data Closure480
  = Variant480_0 ()

data Closure481
  = Variant481_0 ()

data Closure482
  = Variant482_0 ()

data Closure483
  = Variant483_0 ()

data Closure484
  = Variant484_0 ()

data Closure485
  = Variant485_0 ()

data Closure486
  = Variant486_0 ()

data Closure487
  = Variant487_0 ()

data Closure488
  = Variant488_0 ()

data Closure489
  = Variant489_0 ()

data Closure490
  = Variant490_0 ()

data Closure491
  = Variant491_0 ()

data Closure492
  = Variant492_0 ()

data Closure493
  = Variant493_0 ()

data Closure494
  = Variant494_0 ()

data Closure495
  = Variant495_0 ()

data Closure496
  = Variant496_0 ()

data Closure497
  = Variant497_0 ()

data Closure498
  = Variant498_0

data Closure499
  = Variant499_0 ()

data Closure500
  = Variant500_0 ()

data Closure501
  = Variant501_0 ()

data Closure502
  = Variant502_0 ()

data Closure503
  = Variant503_0 ()

data Closure504
  = Variant504_0 ()

data Closure505
  = Variant505_0 ()

data Closure506
  = Variant506_0 ()

data Closure507
  = Variant507_0 ()

data Closure508
  = Variant508_0 ()

data Closure509
  = Variant509_0

data Closure510
  = Variant510_0 ()

data Closure511
  = Variant511_0 ()

data Closure512
  = Variant512_0 ()

data Closure513
  = Variant513_0 ()

data Closure514
  = Variant514_0 ()

data Closure515
  = Variant515_0 ()

data Closure516
  = Variant516_0 ()

data Closure517
  = Variant517_0 ()

data Closure518
  = Variant518_0 ()

data Closure519
  = Variant519_0 ()

data Closure520
  = Variant520_0 ()

data Closure521
  = Variant521_0 ()

data Closure522
  = Variant522_0 ()

data Closure523
  = Variant523_0 ()

data Closure524
  = Variant524_0

data Closure525
  = Variant525_0

data Closure526
  = Variant526_0 ()

data Closure527
  = Variant527_0 ()

data Closure528
  = Variant528_0 ()

data Closure529
  = Variant529_0 ()

data Closure530
  = Variant530_0 ()

data Closure531
  = Variant531_0 ()

data Closure532
  = Variant532_0 ()

data Closure533
  = Variant533_0 ()

data Closure534
  = Variant534_0 ()

data Closure535
  = Variant535_0 ()

data Closure536
  = Variant536_0 ()

data Closure537
  = Variant537_0 ()

data Closure538
  = Variant538_0 ()

data Closure539
  = Variant539_0 ()

data Closure540
  = Variant540_0 ()

data Closure541
  = Variant541_0 ()

data Closure542
  = Variant542_0 ()

data Closure543
  = Variant543_0 ()

data Closure544
  = Variant544_0 ()

data Closure545
  = Variant545_0 ()

data Closure546
  = Variant546_0 ()

data Closure547
  = Variant547_0 ()

data Closure548
  = Variant548_0 ()

data Closure549
  = Variant549_0 ()

data Closure550
  = Variant550_0 ()

data Closure551
  = Variant551_0 ()

data Closure552
  = Variant552_0 ()

data Closure553
  = Variant553_0 ()

data Closure554
  = Variant554_0 ()

data Closure555
  = Variant555_0 ()

data Closure556
  = Variant556_0 ()

data Closure557
  = Variant557_0 ()

data Closure558
  = Variant558_0 ()

data Closure559
  = Variant559_0

data Closure560
  = Variant560_0 ()

data Closure561
  = Variant561_0 ()

data Closure562
  = Variant562_0 ()

data Closure563
  = Variant563_0 ()

data Closure564
  = Variant564_0 ()

data Closure565
  = Variant565_0 ()

data Closure566
  = Variant566_0 ()

data Closure567
  = Variant567_0 ()

data Closure568
  = Variant568_0 ()

data Closure569
  = Variant569_0 ()

data Closure570
  = Variant570_0 ()

data Closure571
  = Variant571_0 ()

data Closure572
  = Variant572_0 ()

data Closure573
  = Variant573_0 ()

data Closure574
  = Variant574_0 ()

data Closure575
  = Variant575_0 ()

data Closure576
  = Variant576_0 ()

data Closure577
  = Variant577_0

data Closure578
  = Variant578_0 ()

data Closure579
  = Variant579_0 ()

data Closure580
  = Variant580_0 ()

data Closure581
  = Variant581_0 ()

data Closure582
  = Variant582_0 ()

data Closure583
  = Variant583_0 ()

data Closure584
  = Variant584_0 ()

data Closure585
  = Variant585_0 ()

data Closure586
  = Variant586_0 ()

data Closure587
  = Variant587_0 ()

data Closure588
  = Variant588_0 ()

data Closure589
  = Variant589_0 ()

data Closure590
  = Variant590_0 ()

data Closure591
  = Variant591_0 ()

data Closure592
  = Variant592_0 ()

data Closure593
  = Variant593_0 ()

data Closure594
  = Variant594_0 ()

data Closure595
  = Variant595_0 ()

data Closure596
  = Variant596_0 ()

data Closure597
  = Variant597_0 ()

data Closure598
  = Variant598_0 ()

data Closure599
  = Variant599_0 ()

data Closure600
  = Variant600_0 ()

data Closure601
  = Variant601_0 ()

data Closure602
  = Variant602_0 ()

data Closure603
  = Variant603_0 ()

data Closure604
  = Variant604_0 ()

data Closure605
  = Variant605_0 ()

data Closure606
  = Variant606_0 ()

data Closure607
  = Variant607_0 ()

data Closure608
  = Variant608_0 ()

data Closure609
  = Variant609_0 ()

data Closure610
  = Variant610_0 ()

data Closure611
  = Variant611_0 ()

data Closure612
  = Variant612_0 ()

data Closure613
  = Variant613_0 ()

data Closure614
  = Variant614_0 ()

data Closure615
  = Variant615_0 ()

data Closure616
  = Variant616_0 ()

data Closure617
  = Variant617_0 ()

data Closure618
  = Variant618_0 ()

data Closure619
  = Variant619_0 ()

data Closure620
  = Variant620_0 ()

data Closure621
  = Variant621_0 ()

data Closure622
  = Variant622_0 ()

data Closure623
  = Variant623_0 ()

data Closure624
  = Variant624_0 ()

data Closure625
  = Variant625_0 ()

data Closure626
  = Variant626_0 ()

data Closure627
  = Variant627_0 ()

data Closure628
  = Variant628_0 ()

data Closure629
  = Variant629_0 ()

data Closure630
  = Variant630_0 ()

data Closure631
  = Variant631_0 ()

data Closure632
  = Variant632_0 ()

data Closure633
  = Variant633_0 ()

data Closure634
  = Variant634_0 ()

data Closure635
  = Variant635_0 ()

data Closure636
  = Variant636_0 ()

data Closure637
  = Variant637_0 ()

data Closure638
  = Variant638_0 ()

data Closure639
  = Variant639_0 ()

data Closure640
  = Variant640_0 ()

data Closure641
  = Variant641_0 ()

data Closure642
  = Variant642_0 ()

data Closure643
  = Variant643_0 ()

data Closure644
  = Variant644_0 ()

data Closure645
  = Variant645_0 ()

data Closure646
  = Variant646_0 ()

data Closure647
  = Variant647_0 ()

data Closure648
  = Variant648_0 ()

data Closure649
  = Variant649_0 ()

data Closure650
  = Variant650_0 ()

data Closure651
  = Variant651_0 ()

data Closure652
  = Variant652_0 ()

data Closure653
  = Variant653_0 ()

data Closure654
  = Variant654_0 ()

data Closure655
  = Variant655_0 ()

data Closure656
  = Variant656_0 ()

data Closure657
  = Variant657_0 ()

data Closure658
  = Variant658_0 ()

data Closure659
  = Variant659_0 ()

data Closure660
  = Variant660_0 ()

data Closure661
  = Variant661_0 ()

data Closure662
  = Variant662_0 ()

data Closure663
  = Variant663_0 ()

data Closure664
  = Variant664_0 ()

data Closure665
  = Variant665_0 ()

data Closure666
  = Variant666_0 ()

data Closure667
  = Variant667_0 ()

data Closure668
  = Variant668_0 ()

data Closure669
  = Variant669_0 ()

data Closure670
  = Variant670_0 ()

data Closure671
  = Variant671_0 ()

data Closure672
  = Variant672_0

data Closure673
  = Variant673_0 ()

data Closure674
  = Variant674_0 ()

data Closure675
  = Variant675_0 ()

data Closure676
  = Variant676_0 ()

data Closure677
  = Variant677_0 ()

data Closure678
  = Variant678_0 ()

data Closure679
  = Variant679_0 ()

data Closure680
  = Variant680_0 ()

data Closure681
  = Variant681_0 ()

data Closure682
  = Variant682_0 ()

data Closure683
  = Variant683_0 ()

data Closure684
  = Variant684_0 ()

data Closure685
  = Variant685_0

data Closure686
  = Variant686_0 ()

data Closure687
  = Variant687_0 ()

data Closure688
  = Variant688_0 ()

data Closure689
  = Variant689_0 ()

data Closure690
  = Variant690_0 ()

data Closure691
  = Variant691_0 ()

data Closure692
  = Variant692_0 ()

data Closure693
  = Variant693_0 ()

data Closure694
  = Variant694_0 ()

data Closure695
  = Variant695_0 ()

data Closure696
  = Variant696_0 ()

data Closure697
  = Variant697_0 ()

data Closure698
  = Variant698_0 ()

data Closure699
  = Variant699_0 ()

data Closure700
  = Variant700_0 ()

data Closure701
  = Variant701_0 ()

data Closure702
  = Variant702_0 ()

data Closure703
  = Variant703_0 ()

data Closure704
  = Variant704_0 ()

data Closure705
  = Variant705_0 ()

data Closure706
  = Variant706_0 ()

data Closure707
  = Variant707_0 (Vector Problem5) 

data Closure708
  = Variant708_0 ()

data Closure709
  = Variant709_0 ()

data Closure710
  = Variant710_0 Word8

data Closure711
  = Variant711_0 Int64

data Closure712
  = Variant712_0 ((Vector Word8) , Closure152)

data Closure713
  = Variant713_0 Closure155

data Closure714
  = Variant714_0 Closure161

data Closure715
  = Variant715_0 (Int64, (Vector Word8) , Parse19)

data Closure716
  = Variant716_0 ((Vector Word8) , Closure157)

data Closure717
  = Variant717_0 ((Vector Word8) , Closure166)

data Closure718
  = Variant718_0 ((Vector Word8) , Closure168)

data Closure719
  = Variant719_0 Closure171

data Closure720
  = Variant720_0 ((Vector Word8) , Parse24)

data Closure721
  = Variant721_0 Closure195

data Closure722
  = Variant722_0 Closure174

data Closure723
  = Variant723_0 ((Vector Word8) , Parse28)

data Closure724
  = Variant724_0 Closure183

data Closure725
  = Variant725_0 (Int64, (Vector Word8) , Parse36)

data Closure726
  = Variant726_0 ((Vector Word8) , Closure179)

data Closure727
  = Variant727_0 ((Vector Word8) , Closure191)

data Closure728
  = Variant728_0 ((Vector Word8) , Parse23)

data Closure729
  = Variant729_0 Expr7

data Closure730
  = Variant730_0 ((Vector Word8) , Parse42)

data Closure731
  = Variant731_0 (Int64, (Vector Word8) , Parse45)

data Closure732
  = Variant732_0 ((Vector Word8) , Closure197)

data Closure733
  = Variant733_0 ((Vector Word8) , Closure201)

data Closure734
  = Variant734_0 ((Vector Word8) , Parse49)

data Closure735
  = Variant735_0 ((Vector Word8) , Closure204)

data Closure736
  = Variant736_0 Closure207

data Closure737
  = Variant737_0 (Int64, (Vector Word8) , Parse56)

data Closure738
  = Variant738_0 ((Vector Word8) , Parse52)

data Closure739
  = Variant739_0 ((Vector Word8) , Closure212)

data Closure740
  = Variant740_0 ((Vector Word8) , Closure218)

data Closure741
  = Variant741_0 ((Vector Word8) , Closure223)

data Closure742
  = Variant742_0 ((Vector Word8) , Parse67)

data Closure743
  = Variant743_0 ((Vector Word8) , Closure226)

data Closure744
  = Variant744_0 ((Vector Word8) , Parse69)

data Closure745
  = Variant745_0 ((Vector Word8) , Closure229)

data Closure746
  = Variant746_0 (Vector Word8) 

data Closure747
  = Variant747_0 ()

data Closure748
  = Variant748_0 (Term83 -> Vector Term83)

data Closure749
  = Variant749_0

data Closure750
  = Variant750_0 ()

range_0 :: () -> Closure326
range_0 () =
  Variant326_0 ()

next_1 :: () -> Closure327
next_1 () =
  Variant327_0 ()

map_2 :: () -> Closure328
map_2 () =
  Variant328_0 ()

next_3 :: () -> Closure329
next_3 () =
  Variant329_0 ()

map_4 :: () -> Closure330
map_4 () =
  Variant330_0 ()

digit_to_nat_5 :: () -> Closure331
digit_to_nat_5 () =
  Variant331_0 ()

and_then_6 :: () -> Closure332
and_then_6 () =
  Variant332_0 ()

foldl_7 :: () -> Closure333
foldl_7 () =
  Variant333_0 ()

wrapped_foldl_8 :: () -> Closure334
wrapped_foldl_8 () =
  Variant334_0 ()

chars_to_nat_9 :: () -> Closure335
chars_to_nat_9 () =
  Variant335_0 ()

wrapped_map_10 :: () -> Closure336
wrapped_map_10 () =
  Variant336_0 ()

wrapped_range_11 :: () -> Closure337
wrapped_range_11 () =
  Variant337_0 ()

items_12 :: () -> Closure338
items_12 () =
  Variant338_0 ()

string_to_nat_13 :: () -> Closure339
string_to_nat_13 () =
  Variant339_0 ()

len__15 :: () -> Closure340
len__15 () =
  Variant340_0

len_14 :: () -> Closure340
len_14 () =
  len__15 ()

get__17 :: () -> Closure341
get__17 () =
  Variant341_0

get_16 :: () -> Closure341
get_16 () =
  get__17 ()

pure_18 :: () -> Closure342
pure_18 () =
  Variant342_0 ()

fail_19 :: () -> Parse10
fail_19 () =
  Parse10_0 (Variant149_1 ())

parse_from_20 :: () -> Closure343
parse_from_20 () =
  Variant343_0 ()

parse_from_21 :: () -> Closure344
parse_from_21 () =
  Variant344_0 ()

and_then_22 :: () -> Closure345
and_then_22 () =
  Variant345_0 ()

ascii_zero_23 :: () -> Word8
ascii_zero_23 () =
  48

parse_from_24 :: () -> Closure346
parse_from_24 () =
  Variant346_0 ()

map_25 :: () -> Closure347
map_25 () =
  Variant347_0 ()

parse_from_26 :: () -> Closure348
parse_from_26 () =
  Variant348_0 ()

map_27 :: () -> Closure349
map_27 () =
  Variant349_0 ()

pure_28 :: () -> Closure350
pure_28 () =
  Variant350_0 ()

parse_from_29 :: () -> Closure351
parse_from_29 () =
  Variant351_0 ()

parse_from_30 :: () -> Closure352
parse_from_30 () =
  Variant352_0 ()

parse_from_31 :: () -> Closure353
parse_from_31 () =
  Variant353_0 ()

or_else_32 :: () -> Closure354
or_else_32 () =
  Variant354_0 ()

pure_33 :: () -> Closure355
pure_33 () =
  Variant355_0 ()

many0_fold_34 :: () -> Closure356
many0_fold_34 () =
  Variant356_0 ()

parse_from_35 :: () -> Closure357
parse_from_35 () =
  Variant357_0 ()

parse_from_36 :: () -> Closure358
parse_from_36 () =
  Variant358_0 ()

and_then_37 :: () -> Closure359
and_then_37 () =
  Variant359_0 ()

and_then_38 :: () -> Closure360
and_then_38 () =
  Variant360_0 ()

or_39 :: () -> Closure361
or_39 () =
  Variant361_0 ()

map_40 :: () -> Closure362
map_40 () =
  Variant362_0 ()

lazy_41 :: () -> Closure363
lazy_41 () =
  Variant363_0 ()

optional_42 :: () -> Closure364
optional_42 () =
  Variant364_0 ()

wrapped_many0_fold_43 :: () -> Closure365
wrapped_many0_fold_43 () =
  Variant365_0 ()

and_then_44 :: () -> Closure366
and_then_44 () =
  Variant366_0 ()

and_then_45 :: () -> Closure367
and_then_45 () =
  Variant367_0 ()

parse_from_46 :: () -> Closure368
parse_from_46 () =
  Variant368_0 ()

map_47 :: () -> Closure369
map_47 () =
  Variant369_0 ()

parse_from_48 :: () -> Closure370
parse_from_48 () =
  Variant370_0 ()

parse_from_49 :: () -> Closure371
parse_from_49 () =
  Variant371_0 ()

and_then_50 :: () -> Closure372
and_then_50 () =
  Variant372_0 ()

parse_from_51 :: () -> Closure373
parse_from_51 () =
  Variant373_0 ()

map_52 :: () -> Closure374
map_52 () =
  Variant374_0 ()

map_53 :: () -> Closure375
map_53 () =
  Variant375_0 ()

parse_from_54 :: () -> Closure376
parse_from_54 () =
  Variant376_0 ()

and_then_55 :: () -> Closure377
and_then_55 () =
  Variant377_0 ()

parse_from_56 :: () -> Closure378
parse_from_56 () =
  Variant378_0 ()

map_57 :: () -> Closure379
map_57 () =
  Variant379_0 ()

pure_58 :: () -> Closure380
pure_58 () =
  Variant380_0 ()

parse_from_59 :: () -> Closure381
parse_from_59 () =
  Variant381_0 ()

parse_from_60 :: () -> Closure382
parse_from_60 () =
  Variant382_0 ()

parse_from_61 :: () -> Closure383
parse_from_61 () =
  Variant383_0 ()

or_else_62 :: () -> Closure384
or_else_62 () =
  Variant384_0 ()

pure_63 :: () -> Closure385
pure_63 () =
  Variant385_0 ()

many0_fold_64 :: () -> Closure386
many0_fold_64 () =
  Variant386_0 ()

parse_from_65 :: () -> Closure387
parse_from_65 () =
  Variant387_0 ()

parse_from_66 :: () -> Closure388
parse_from_66 () =
  Variant388_0 ()

and_then_67 :: () -> Closure389
and_then_67 () =
  Variant389_0 ()

and_then_68 :: () -> Closure390
and_then_68 () =
  Variant390_0 ()

or_69 :: () -> Closure391
or_69 () =
  Variant391_0 ()

map_70 :: () -> Closure392
map_70 () =
  Variant392_0 ()

lazy_71 :: () -> Closure393
lazy_71 () =
  Variant393_0 ()

optional_72 :: () -> Closure394
optional_72 () =
  Variant394_0 ()

wrapped_many0_fold_73 :: () -> Closure395
wrapped_many0_fold_73 () =
  Variant395_0 ()

push__75 :: () -> Closure180
push__75 () =
  Variant180_0

push_74 :: () -> Closure180
push_74 () =
  push__75 ()

pure_76 :: () -> Closure396
pure_76 () =
  Variant396_0 ()

parse_from_77 :: () -> Closure397
parse_from_77 () =
  Variant397_0 ()

parse_from_78 :: () -> Closure398
parse_from_78 () =
  Variant398_0 ()

and_then_79 :: () -> Closure399
and_then_79 () =
  Variant399_0 ()

parse_from_80 :: () -> Closure400
parse_from_80 () =
  Variant400_0 ()

map_81 :: () -> Closure401
map_81 () =
  Variant401_0 ()

and_then_82 :: () -> Closure402
and_then_82 () =
  Variant402_0 ()

parse_from_83 :: () -> Closure403
parse_from_83 () =
  Variant403_0 ()

and_then_84 :: () -> Closure404
and_then_84 () =
  Variant404_0 ()

skip_pre_85 :: () -> Closure405
skip_pre_85 () =
  Variant405_0 ()

skip_post_86 :: () -> Closure406
skip_post_86 () =
  Variant406_0 ()

between_87 :: () -> Closure407
between_87 () =
  Variant407_0 ()

and_then_88 :: () -> Closure408
and_then_88 () =
  Variant408_0 ()

guard_89 :: () -> Closure409
guard_89 () =
  Variant409_0 ()

byte_90 :: () -> Parse11
byte_90 () =
  Parse11_0 (Variant150_0 ())

byte_eq_91 :: () -> Closure410
byte_eq_91 () =
  Variant410_0 ()

ascii_open_paren_92 :: () -> Word8
ascii_open_paren_92 () =
  40

ascii_close_paren_93 :: () -> Word8
ascii_close_paren_93 () =
  41

and_then_94 :: () -> Closure411
and_then_94 () =
  Variant411_0 ()

lazy_95 :: () -> Closure412
lazy_95 () =
  Variant412_0 ()

many0_96 :: () -> Closure413
many0_96 () =
  Variant413_0 ()

parse_from_98 :: () -> Closure414
parse_from_98 () =
  Variant414_0 ()

parse_from_99 :: () -> Closure415
parse_from_99 () =
  Variant415_0 ()

or_else_100 :: () -> Closure416
or_else_100 () =
  Variant416_0 ()

or_101 :: () -> Closure417
or_101 () =
  Variant417_0 ()

map_102 :: () -> Closure418
map_102 () =
  Variant418_0 ()

skip_pre_104 :: () -> Closure419
skip_pre_104 () =
  Variant419_0 ()

ascii_question_105 :: () -> Word8
ascii_question_105 () =
  63

map_106 :: () -> Closure420
map_106 () =
  Variant420_0 ()

and_then_108 :: () -> Closure421
and_then_108 () =
  Variant421_0 ()

many1_fold_109 :: () -> Closure422
many1_fold_109 () =
  Variant422_0 ()

and_then_111 :: () -> Closure423
and_then_111 () =
  Variant423_0 ()

guard_112 :: () -> Closure424
guard_112 () =
  Variant424_0 ()

byte_range_113 :: () -> Closure425
byte_range_113 () =
  Variant425_0 ()

ascii_nine_114 :: () -> Word8
ascii_nine_114 () =
  57

map_115 :: () -> Closure426
map_115 () =
  Variant426_0 ()

parse_from_116 :: () -> Closure427
parse_from_116 () =
  Variant427_0 ()

parse_from_117 :: () -> Closure428
parse_from_117 () =
  Variant428_0 ()

and_then_118 :: () -> Closure429
and_then_118 () =
  Variant429_0 ()

and_then_119 :: () -> Closure430
and_then_119 () =
  Variant430_0 ()

skip_pre_121 :: () -> Closure431
skip_pre_121 () =
  Variant431_0 ()

ascii_excalam_122 :: () -> Word8
ascii_excalam_122 () =
  33

map_123 :: () -> Closure432
map_123 () =
  Variant432_0 ()

parse_from_124 :: () -> Closure433
parse_from_124 () =
  Variant433_0 ()

lazy_125 :: () -> Closure434
lazy_125 () =
  Variant434_0 ()

pure_126 :: () -> Closure435
pure_126 () =
  Variant435_0 ()

parse_from_127 :: () -> Closure436
parse_from_127 () =
  Variant436_0 ()

and_then_128 :: () -> Closure437
and_then_128 () =
  Variant437_0 ()

parse_from_129 :: () -> Closure438
parse_from_129 () =
  Variant438_0 ()

and_then_130 :: () -> Closure439
and_then_130 () =
  Variant439_0 ()

skip_pre_131 :: () -> Closure440
skip_pre_131 () =
  Variant440_0 ()

ascii_tilde_132 :: () -> Word8
ascii_tilde_132 () =
  126

and_then_133 :: () -> Closure441
and_then_133 () =
  Variant441_0 ()

parse_from_134 :: () -> Closure442
parse_from_134 () =
  Variant442_0 ()

and_then_135 :: () -> Closure443
and_then_135 () =
  Variant443_0 ()

parse_from_136 :: () -> Closure444
parse_from_136 () =
  Variant444_0 ()

map_137 :: () -> Closure445
map_137 () =
  Variant445_0 ()

pure_138 :: () -> Closure446
pure_138 () =
  Variant446_0 ()

parse_from_139 :: () -> Closure447
parse_from_139 () =
  Variant447_0 ()

parse_from_140 :: () -> Closure448
parse_from_140 () =
  Variant448_0 ()

parse_from_141 :: () -> Closure449
parse_from_141 () =
  Variant449_0 ()

or_else_142 :: () -> Closure450
or_else_142 () =
  Variant450_0 ()

and_then_143 :: () -> Closure451
and_then_143 () =
  Variant451_0 ()

parse_from_144 :: () -> Closure452
parse_from_144 () =
  Variant452_0 ()

parse_from_145 :: () -> Closure453
parse_from_145 () =
  Variant453_0 ()

pure_146 :: () -> Closure454
pure_146 () =
  Variant454_0 ()

many0_fold_147 :: () -> Closure455
many0_fold_147 () =
  Variant455_0 ()

parse_from_148 :: () -> Closure456
parse_from_148 () =
  Variant456_0 ()

parse_from_149 :: () -> Closure457
parse_from_149 () =
  Variant457_0 ()

and_then_150 :: () -> Closure458
and_then_150 () =
  Variant458_0 ()

and_then_151 :: () -> Closure459
and_then_151 () =
  Variant459_0 ()

or_152 :: () -> Closure460
or_152 () =
  Variant460_0 ()

map_153 :: () -> Closure461
map_153 () =
  Variant461_0 ()

lazy_154 :: () -> Closure462
lazy_154 () =
  Variant462_0 ()

optional_155 :: () -> Closure463
optional_155 () =
  Variant463_0 ()

wrapped_many0_fold_156 :: () -> Closure464
wrapped_many0_fold_156 () =
  Variant464_0 ()

skip_pre_157 :: () -> Closure465
skip_pre_157 () =
  Variant465_0 ()

parse_from_158 :: () -> Closure466
parse_from_158 () =
  Variant466_0 ()

and_then_159 :: () -> Closure467
and_then_159 () =
  Variant467_0 ()

and_then_160 :: () -> Closure468
and_then_160 () =
  Variant468_0 ()

or_161 :: () -> Closure469
or_161 () =
  Variant469_0 ()

map_162 :: () -> Closure470
map_162 () =
  Variant470_0 ()

optional_163 :: () -> Closure471
optional_163 () =
  Variant471_0 ()

sep0_fold_164 :: () -> Closure472
sep0_fold_164 () =
  Variant472_0 ()

push__166 :: () -> Closure213
push__166 () =
  Variant213_0

push_165 :: () -> Closure213
push_165 () =
  push__166 ()

parse_from_167 :: () -> Closure473
parse_from_167 () =
  Variant473_0 ()

pure_168 :: () -> Closure474
pure_168 () =
  Variant474_0 ()

parse_from_169 :: () -> Closure475
parse_from_169 () =
  Variant475_0 ()

parse_from_170 :: () -> Closure476
parse_from_170 () =
  Variant476_0 ()

and_then_171 :: () -> Closure477
and_then_171 () =
  Variant477_0 ()

parse_from_172 :: () -> Closure478
parse_from_172 () =
  Variant478_0 ()

and_then_173 :: () -> Closure479
and_then_173 () =
  Variant479_0 ()

skip_pre_174 :: () -> Closure480
skip_pre_174 () =
  Variant480_0 ()

ascii_semicolon_175 :: () -> Word8
ascii_semicolon_175 () =
  59

and_then_176 :: () -> Closure481
and_then_176 () =
  Variant481_0 ()

lazy_177 :: () -> Closure482
lazy_177 () =
  Variant482_0 ()

sep0_178 :: () -> Closure483
sep0_178 () =
  Variant483_0 ()

and_then_180 :: () -> Closure484
and_then_180 () =
  Variant484_0 ()

ascii_comma_181 :: () -> Word8
ascii_comma_181 () =
  44

parse_from_182 :: () -> Closure485
parse_from_182 () =
  Variant485_0 ()

and_then_183 :: () -> Closure486
and_then_183 () =
  Variant486_0 ()

parse_from_184 :: () -> Closure487
parse_from_184 () =
  Variant487_0 ()

and_then_185 :: () -> Closure488
and_then_185 () =
  Variant488_0 ()

skip_pre_186 :: () -> Closure489
skip_pre_186 () =
  Variant489_0 ()

and_then_187 :: () -> Closure490
and_then_187 () =
  Variant490_0 ()

parse_from_188 :: () -> Closure491
parse_from_188 () =
  Variant491_0 ()

and_then_189 :: () -> Closure492
and_then_189 () =
  Variant492_0 ()

parse_from_190 :: () -> Closure493
parse_from_190 () =
  Variant493_0 ()

parse_prefix_191 :: () -> Closure494
parse_prefix_191 () =
  Variant494_0 ()

and_then_192 :: () -> Closure495
and_then_192 () =
  Variant495_0 ()

parse_all_193 :: () -> Closure496
parse_all_193 () =
  Variant496_0 ()

and_then_195 :: () -> Closure497
and_then_195 () =
  Variant497_0 ()

push__197 :: () -> Closure498
push__197 () =
  Variant498_0

push_196 :: () -> Closure498
push_196 () =
  push__197 ()

read_problems_rec_198 :: () -> Closure499
read_problems_rec_198 () =
  Variant499_0 ()

wrapped_read_problems_rec_199 :: () -> Closure500
wrapped_read_problems_rec_199 () =
  Variant500_0 ()

read_problems_200 :: () -> Closure501
read_problems_200 () =
  Variant501_0 ()

range_201 :: () -> Closure502
range_201 () =
  Variant502_0 ()

next_202 :: () -> Closure503
next_202 () =
  Variant503_0 ()

map_203 :: () -> Closure504
map_203 () =
  Variant504_0 ()

range_204 :: () -> Closure505
range_204 () =
  Variant505_0 ()

next_205 :: () -> Closure506
next_205 () =
  Variant506_0 ()

map_206 :: () -> Closure507
map_206 () =
  Variant507_0 ()

pure_207 :: () -> Closure508
pure_207 () =
  Variant508_0 ()

push__209 :: () -> Closure509
push__209 () =
  Variant509_0

push_208 :: () -> Closure509
push_208 () =
  push__209 ()

run_210 :: () -> Closure510
run_210 () =
  Variant510_0 ()

run_211 :: () -> Closure511
run_211 () =
  Variant511_0 ()

pure_212 :: () -> Closure512
pure_212 () =
  Variant512_0 ()

range_213 :: () -> Closure513
range_213 () =
  Variant513_0 ()

next_214 :: () -> Closure514
next_214 () =
  Variant514_0 ()

map_215 :: () -> Closure515
map_215 () =
  Variant515_0 ()

bind_216 :: () -> Closure516
bind_216 () =
  Variant516_0 ()

mk_expr_217 :: () -> Closure517
mk_expr_217 () =
  Variant517_0 ()

next_218 :: () -> Closure518
next_218 () =
  Variant518_0 ()

for_each_accum_219 :: () -> Closure519
for_each_accum_219 () =
  Variant519_0 ()

run_220 :: () -> Closure520
run_220 () =
  Variant520_0 ()

run_221 :: () -> Closure521
run_221 () =
  Variant521_0 ()

bind_222 :: () -> Closure522
bind_222 () =
  Variant522_0 ()

pure_223 :: () -> Closure523
pure_223 () =
  Variant523_0 ()

len__225 :: () -> Closure524
len__225 () =
  Variant524_0

len_224 :: () -> Closure524
len_224 () =
  len__225 ()

push__227 :: () -> Closure525
push__227 () =
  Variant525_0

push_226 :: () -> Closure525
push_226 () =
  push__227 ()

modify_ret_228 :: () -> Closure526
modify_ret_228 () =
  Variant526_0 ()

new_term_229 :: () -> Closure527
new_term_229 () =
  Variant527_0 ()

run_230 :: () -> Closure528
run_230 () =
  Variant528_0 ()

bind_231 :: () -> Closure529
bind_231 () =
  Variant529_0 ()

wrapped_for_each_accum_232 :: () -> Closure530
wrapped_for_each_accum_232 () =
  Variant530_0 ()

wrapped_map_233 :: () -> Closure531
wrapped_map_233 () =
  Variant531_0 ()

wrapped_range_234 :: () -> Closure532
wrapped_range_234 () =
  Variant532_0 ()

items_235 :: () -> Closure533
items_235 () =
  Variant533_0 ()

range_236 :: () -> Closure534
range_236 () =
  Variant534_0 ()

next_237 :: () -> Closure535
next_237 () =
  Variant535_0 ()

map_238 :: () -> Closure536
map_238 () =
  Variant536_0 ()

map_239 :: () -> Closure537
map_239 () =
  Variant537_0 ()

next_240 :: () -> Closure538
next_240 () =
  Variant538_0 ()

next_241 :: () -> Closure539
next_241 () =
  Variant539_0 ()

zip_242 :: () -> Closure540
zip_242 () =
  Variant540_0 ()

next_243 :: () -> Closure541
next_243 () =
  Variant541_0 ()

for_each_checked_244 :: () -> Closure542
for_each_checked_244 () =
  Variant542_0 ()

pure_245 :: () -> Closure543
pure_245 () =
  Variant543_0 ()

run_246 :: () -> Closure544
run_246 () =
  Variant544_0 ()

run_247 :: () -> Closure545
run_247 () =
  Variant545_0 ()

bind_248 :: () -> Closure546
bind_248 () =
  Variant546_0 ()

set_249 :: () -> Closure547
set_249 () =
  Variant547_0 ()

pure_250 :: () -> Closure548
pure_250 () =
  Variant548_0 ()

run_251 :: () -> Closure549
run_251 () =
  Variant549_0 ()

run_252 :: () -> Closure550
run_252 () =
  Variant550_0 ()

bind_253 :: () -> Closure551
bind_253 () =
  Variant551_0 ()

modify_254 :: () -> Closure552
modify_254 () =
  Variant552_0 ()

set_term_255 :: () -> Closure553
set_term_255 () =
  Variant553_0 ()

run_256 :: () -> Closure554
run_256 () =
  Variant554_0 ()

bind_257 :: () -> Closure555
bind_257 () =
  Variant555_0 ()

pure_258 :: () -> Closure556
pure_258 () =
  Variant556_0 ()

run_259 :: () -> Closure557
run_259 () =
  Variant557_0 ()

head_id_eq_260 :: () -> Closure558
head_id_eq_260 () =
  Variant558_0 ()

len__262 :: () -> Closure559
len__262 () =
  Variant559_0

len_261 :: () -> Closure559
len_261 () =
  len__262 ()

pure_263 :: () -> Closure560
pure_263 () =
  Variant560_0 ()

wrapped_zip_264 :: () -> Closure561
wrapped_zip_264 () =
  Variant561_0 ()

wrapped_map_265 :: () -> Closure562
wrapped_map_265 () =
  Variant562_0 ()

wrapped_range_266 :: () -> Closure563
wrapped_range_266 () =
  Variant563_0 ()

items_267 :: () -> Closure564
items_267 () =
  Variant564_0 ()

wrapped_map_268 :: () -> Closure565
wrapped_map_268 () =
  Variant565_0 ()

items_269 :: () -> Closure566
items_269 () =
  Variant566_0 ()

bind_270 :: () -> Closure567
bind_270 () =
  Variant567_0 ()

wrapped_for_each_checked_271 :: () -> Closure568
wrapped_for_each_checked_271 () =
  Variant568_0 ()

unify_272 :: () -> Closure255
unify_272 () =
  Variant255_0 ()

pure_273 :: () -> Closure569
pure_273 () =
  Variant569_0 ()

run_274 :: () -> Closure570
run_274 () =
  Variant570_0 ()

var_id_eq_275 :: () -> Closure571
var_id_eq_275 () =
  Variant571_0 ()

pure_276 :: () -> Closure572
pure_276 () =
  Variant572_0 ()

bind_277 :: () -> Closure573
bind_277 () =
  Variant573_0 ()

run_278 :: () -> Closure574
run_278 () =
  Variant574_0 ()

run_279 :: () -> Closure575
run_279 () =
  Variant575_0 ()

pure_280 :: () -> Closure576
pure_280 () =
  Variant576_0 ()

get__282 :: () -> Closure577
get__282 () =
  Variant577_0

get_281 :: () -> Closure577
get_281 () =
  get__282 ()

run_283 :: () -> Closure578
run_283 () =
  Variant578_0 ()

run_284 :: () -> Closure579
run_284 () =
  Variant579_0 ()

bind_285 :: () -> Closure580
bind_285 () =
  Variant580_0 ()

follow_286 :: () -> Closure581
follow_286 () =
  Variant581_0 ()

pure_287 :: () -> Closure582
pure_287 () =
  Variant582_0 ()

run_288 :: () -> Closure583
run_288 () =
  Variant583_0 ()

run_289 :: () -> Closure584
run_289 () =
  Variant584_0 ()

bind_290 :: () -> Closure585
bind_290 () =
  Variant585_0 ()

bind_291 :: () -> Closure586
bind_291 () =
  Variant586_0 ()

get_292 :: () -> State112
get_292 () =
  State112_0 (Variant273_0 ())

get_term_293 :: () -> Closure587
get_term_293 () =
  Variant587_0 ()

run_294 :: () -> Closure588
run_294 () =
  Variant588_0 ()

bind_295 :: () -> Closure589
bind_295 () =
  Variant589_0 ()

follow_296 :: () -> Closure590
follow_296 () =
  Variant590_0 ()

pure_297 :: () -> Closure591
pure_297 () =
  Variant591_0 ()

run_298 :: () -> Closure592
run_298 () =
  Variant592_0 ()

bind_299 :: () -> Closure593
bind_299 () =
  Variant593_0 ()

pure_300 :: () -> Closure594
pure_300 () =
  Variant594_0 ()

bind_301 :: () -> Closure595
bind_301 () =
  Variant595_0 ()

bind_302 :: () -> Closure596
bind_302 () =
  Variant596_0 ()

run_303 :: () -> Closure597
run_303 () =
  Variant597_0 ()

bind_304 :: () -> Closure598
bind_304 () =
  Variant598_0 ()

run_305 :: () -> Closure599
run_305 () =
  Variant599_0 ()

bind_306 :: () -> Closure600
bind_306 () =
  Variant600_0 ()

pure_307 :: () -> Closure601
pure_307 () =
  Variant601_0 ()

run_308 :: () -> Closure602
run_308 () =
  Variant602_0 ()

bind_309 :: () -> Closure603
bind_309 () =
  Variant603_0 ()

run_310 :: () -> Closure604
run_310 () =
  Variant604_0 ()

bind_311 :: () -> Closure605
bind_311 () =
  Variant605_0 ()

run_312 :: () -> Closure606
run_312 () =
  Variant606_0 ()

bind_313 :: () -> Closure607
bind_313 () =
  Variant607_0 ()

wrapped_follow_314 :: () -> Closure608
wrapped_follow_314 () =
  Variant608_0 ()

run_315 :: () -> Closure609
run_315 () =
  Variant609_0 ()

bind_316 :: () -> Closure610
bind_316 () =
  Variant610_0 ()

wrapped_follow_317 :: () -> Closure611
wrapped_follow_317 () =
  Variant611_0 ()

wrapped_unify_318 :: () -> Closure612
wrapped_unify_318 () =
  Variant612_0 ()

bind_319 :: () -> Closure613
bind_319 () =
  Variant613_0 ()

wrapped_mk_expr_320 :: () -> Closure614
wrapped_mk_expr_320 () =
  Variant614_0 ()

next_321 :: () -> Closure615
next_321 () =
  Variant615_0 ()

for_each_checked_322 :: () -> Closure616
for_each_checked_322 () =
  Variant616_0 ()

pure_323 :: () -> Closure617
pure_323 () =
  Variant617_0 ()

run_324 :: () -> Closure618
run_324 () =
  Variant618_0 ()

run_325 :: () -> Closure619
run_325 () =
  Variant619_0 ()

bind_326 :: () -> Closure620
bind_326 () =
  Variant620_0 ()

pure_327 :: () -> Closure621
pure_327 () =
  Variant621_0 ()

run_328 :: () -> Closure622
run_328 () =
  Variant622_0 ()

run_329 :: () -> Closure623
run_329 () =
  Variant623_0 ()

map_330 :: () -> Closure624
map_330 () =
  Variant624_0 ()

bind_331 :: () -> Closure625
bind_331 () =
  Variant625_0 ()

get_expr_332 :: () -> Closure626
get_expr_332 () =
  Variant626_0 ()

run_333 :: () -> Closure627
run_333 () =
  Variant627_0 ()

bind_334 :: () -> Closure628
bind_334 () =
  Variant628_0 ()

follow_335 :: () -> Closure629
follow_335 () =
  Variant629_0 ()

pure_336 :: () -> Closure630
pure_336 () =
  Variant630_0 ()

run_337 :: () -> Closure631
run_337 () =
  Variant631_0 ()

bind_338 :: () -> Closure632
bind_338 () =
  Variant632_0 ()

pure_339 :: () -> Closure633
pure_339 () =
  Variant633_0 ()

pure_340 :: () -> Closure634
pure_340 () =
  Variant634_0 ()

run_341 :: () -> Closure635
run_341 () =
  Variant635_0 ()

run_342 :: () -> Closure636
run_342 () =
  Variant636_0 ()

bind_343 :: () -> Closure637
bind_343 () =
  Variant637_0 ()

next_344 :: () -> Closure638
next_344 () =
  Variant638_0 ()

for_each_accum_345 :: () -> Closure639
for_each_accum_345 () =
  Variant639_0 ()

run_346 :: () -> Closure640
run_346 () =
  Variant640_0 ()

bind_347 :: () -> Closure641
bind_347 () =
  Variant641_0 ()

pure_348 :: () -> Closure642
pure_348 () =
  Variant642_0 ()

wrapped_for_each_accum_349 :: () -> Closure643
wrapped_for_each_accum_349 () =
  Variant643_0 ()

wrapped_map_350 :: () -> Closure644
wrapped_map_350 () =
  Variant644_0 ()

items_351 :: () -> Closure645
items_351 () =
  Variant645_0 ()

run_352 :: () -> Closure646
run_352 () =
  Variant646_0 ()

bind_353 :: () -> Closure647
bind_353 () =
  Variant647_0 ()

run_354 :: () -> Closure648
run_354 () =
  Variant648_0 ()

bind_355 :: () -> Closure649
bind_355 () =
  Variant649_0 ()

wrapped_follow_356 :: () -> Closure650
wrapped_follow_356 () =
  Variant650_0 ()

pure_357 :: () -> Closure651
pure_357 () =
  Variant651_0 ()

run_358 :: () -> Closure652
run_358 () =
  Variant652_0 ()

bind_359 :: () -> Closure653
bind_359 () =
  Variant653_0 ()

wrapped_get_expr_360 :: () -> Closure654
wrapped_get_expr_360 () =
  Variant654_0 ()

pure_361 :: () -> Closure655
pure_361 () =
  Variant655_0 ()

run_362 :: () -> Closure656
run_362 () =
  Variant656_0 ()

bind_363 :: () -> Closure657
bind_363 () =
  Variant657_0 ()

wrapped_for_each_checked_364 :: () -> Closure658
wrapped_for_each_checked_364 () =
  Variant658_0 ()

wrapped_map_365 :: () -> Closure659
wrapped_map_365 () =
  Variant659_0 ()

wrapped_range_366 :: () -> Closure660
wrapped_range_366 () =
  Variant660_0 ()

items_367 :: () -> Closure661
items_367 () =
  Variant661_0 ()

fill_with_rec_368 :: () -> Closure662
fill_with_rec_368 () =
  Variant662_0 ()

wrapped_fill_with_rec_369 :: () -> Closure663
wrapped_fill_with_rec_369 () =
  Variant663_0 ()

fill_with_370 :: () -> Closure664
fill_with_370 () =
  Variant664_0 ()

run_371 :: () -> Closure665
run_371 () =
  Variant665_0 ()

next_372 :: () -> Closure666
next_372 () =
  Variant666_0 ()

map_373 :: () -> Closure667
map_373 () =
  Variant667_0 ()

wrapped_map_374 :: () -> Closure668
wrapped_map_374 () =
  Variant668_0 ()

solve_375 :: () -> Closure321
solve_375 () =
  Variant321_0 ()

next_376 :: () -> Closure669
next_376 () =
  Variant669_0 ()

foldl_377 :: () -> Closure670
foldl_377 () =
  Variant670_0 ()

wrapped_foldl_378 :: () -> Closure671
wrapped_foldl_378 () =
  Variant671_0 ()

push__380 :: () -> Closure672
push__380 () =
  Variant672_0

push_379 :: () -> Closure672
push_379 () =
  push__380 ()

from_iter_with_capacity_381 :: () -> Closure673
from_iter_with_capacity_381 () =
  Variant673_0 ()

from_iter_382 :: () -> Closure674
from_iter_382 () =
  Variant674_0 ()

solve_problems_383 :: () -> Closure675
solve_problems_383 () =
  Variant675_0 ()

wrapped_map_384 :: () -> Closure676
wrapped_map_384 () =
  Variant676_0 ()

wrapped_range_385 :: () -> Closure677
wrapped_range_385 () =
  Variant677_0 ()

items_386 :: () -> Closure678
items_386 () =
  Variant678_0 ()

repeat_387 :: () -> Closure679
repeat_387 () =
  Variant679_0 ()

wrapped_repeat_388 :: () -> Closure680
wrapped_repeat_388 () =
  Variant680_0 ()

range_389 :: () -> Closure681
range_389 () =
  Variant681_0 ()

next_390 :: () -> Closure682
next_390 () =
  Variant682_0 ()

map_391 :: () -> Closure683
map_391 () =
  Variant683_0 ()

concat_from_392 :: () -> Closure684
concat_from_392 () =
  Variant684_0 ()

push__394 :: () -> Closure685
push__394 () =
  Variant685_0

push_393 :: () -> Closure685
push_393 () =
  push__394 ()

wrapped_concat_from_395 :: () -> Closure686
wrapped_concat_from_395 () =
  Variant686_0 ()

concat_396 :: () -> Closure687
concat_396 () =
  Variant687_0 ()

nat_to_string_397 :: () -> Closure688
nat_to_string_397 () =
  Variant688_0 ()

wrapped_nat_to_string_398 :: () -> Closure689
wrapped_nat_to_string_398 () =
  Variant689_0 ()

int_to_string_399 :: () -> Closure690
int_to_string_399 () =
  Variant690_0 ()

map_400 :: () -> Closure691
map_400 () =
  Variant691_0 ()

next_401 :: () -> Closure692
next_401 () =
  Variant692_0 ()

for_each_402 :: () -> Closure693
for_each_402 () =
  Variant693_0 ()

wrapped_for_each_403 :: () -> Closure694
wrapped_for_each_403 () =
  Variant694_0 ()

wrapped_map_404 :: () -> Closure695
wrapped_map_404 () =
  Variant695_0 ()

items_405 :: () -> Closure696
items_405 () =
  Variant696_0 ()

output_expr_406 :: () -> Closure697
output_expr_406 () =
  Variant697_0 ()

wrapped_output_expr_407 :: () -> Closure698
wrapped_output_expr_407 () =
  Variant698_0 ()

next_408 :: () -> Closure699
next_408 () =
  Variant699_0 ()

for_each_409 :: () -> Closure700
for_each_409 () =
  Variant700_0 ()

wrapped_for_each_410 :: () -> Closure701
wrapped_for_each_410 () =
  Variant701_0 ()

wrapped_map_411 :: () -> Closure702
wrapped_map_411 () =
  Variant702_0 ()

wrapped_range_412 :: () -> Closure703
wrapped_range_412 () =
  Variant703_0 ()

items_413 :: () -> Closure704
items_413 () =
  Variant704_0 ()

writeln_414 :: () -> Closure705
writeln_414 () =
  Variant705_0 ()

main_415 :: () -> Closure706
main_415 () =
  Variant706_0 ()

lam_range_418 :: (Int64, Int64) -> Iter1
lam_range_418 (l0, l1) =
  Iter1_0 (Variant146_0 (l0, l1))

lam_range_419 :: ((Int64, Int64), ()) -> Option2
lam_range_419 ((l0, l1), ()) =
  case uncurry (<) (l0, l1) of True -> Some2_0 (l0, lam_range_418 (uncurry (+) (l0, 1), l1)); False -> None2_1

lam_items_420 :: ((Vector Word8) , Int64) -> Word8
lam_items_420 (l0, l1) =
  intrinsicGet l0 l1

lam_map_421 :: (Iter1, Closure148) -> Iter3
lam_map_421 (l0, l1) =
  Iter3_0 (Variant147_0 (l0, l1))

lam_chars_to_nat_428 :: (Int64, Int64) -> Int64
lam_chars_to_nat_428 (l0, l1) =
  uncurry (+) (uncurry (*) (l0, 10), l1)

lam_digit_to_nat_430 :: Word8 -> Option0
lam_digit_to_nat_430 l0 =
  case l0 of 48 -> Some0_0 0; 49 -> Some0_0 1; 50 -> Some0_0 2; 51 -> Some0_0 3; 52 -> Some0_0 4; 53 -> Some0_0 5; 54 -> Some0_0 6; 55 -> Some0_0 7; 56 -> Some0_0 8; 57 -> Some0_0 9; l_0 -> None0_1

lam_wrapped_map_435 :: (Iter1, Closure148) -> Iter3
lam_wrapped_map_435 l0 =
  lam_map_421 l0

lam_wrapped_range_436 :: (Int64, Int64) -> Iter1
lam_wrapped_range_436 l0 =
  lam_range_418 l0

lam_items_434 :: (Vector Word8)  -> Iter3
lam_items_434 l0 =
  lam_wrapped_map_435 (lam_wrapped_range_436 (0, intrinsicLen l0), Variant148_0 l0)

lam_byte_440 :: (Int64, (Vector Word8) ) -> Option9
lam_byte_440 (l0, l1) =
  case uncurry (<) (l0, intrinsicLen l1) of True -> Some9_0 (uncurry (+) (l0, 1), intrinsicGet l1 l0); False -> None9_1

lam_byte_range_441 :: ((Word8, Word8), Word8) -> Bool
lam_byte_range_441 ((l0, l1), l2) =
  case uncurry (<=) (l0, l2) of False -> False; True -> uncurry (<=) (l2, l1)

lam_pure_442 :: (Word8, (Int64, (Vector Word8) )) -> Option9
lam_pure_442 (l0, (l1, l2)) =
  Some9_0 (l1, l0)

lam_fail_443 :: (Int64, (Vector Word8) ) -> Option9
lam_fail_443 (l_1, l_2) =
  None9_1

lam_pure_445 :: Word8 -> Parse10
lam_pure_445 l0 =
  Parse10_0 (Variant149_0 l0)

lam_parse_from_447 :: (Int64, (Vector Word8) , Parse11) -> Option9
lam_parse_from_447 (l0, l1, l2) =
  let (Parse11_0 l3) = l2 in lam_byte_440 (l0, l1)

lam_parse_digit_451 :: Word8 -> Int64
lam_parse_digit_451 l0 =
  fromIntegral (uncurry (-) (l0, ascii_zero_23 ()))

lam_map_454 :: (Closure155, (Int64, Word8)) -> (Int64, Int64)
lam_map_454 (l0, (l1, l2)) =
  (l1, lam_parse_digit_451 l2)

lam_parse_int_456 :: (Int64, Int64) -> Int64
lam_parse_int_456 (l0, l1) =
  uncurry (+) (uncurry (*) (l0, 10), l1)

lam_optional_458 :: Int64 -> Option0
lam_optional_458 l0 =
  Some0_0 l0

lam_map_461 :: (Closure161, (Int64, Int64)) -> (Int64, Option0)
lam_map_461 (l0, (l1, l2)) =
  (l1, lam_optional_458 l2)

lam_pure_463 :: (Option0, (Int64, (Vector Word8) )) -> Option16
lam_pure_463 (l0, (l1, l2)) =
  Some16_0 (l1, l0)

lam_pure_465 :: Option0 -> Parse17
lam_pure_465 l0 =
  Parse17_0 (Variant159_0 l0)

lam_optional_464 :: () -> Parse17
lam_optional_464 () =
  lam_pure_465 (None0_1)

lam_pure_474 :: Int64 -> Parse15
lam_pure_474 l0 =
  Parse15_0 (Variant156_0 l0)

lam_pure_475 :: (Int64, (Int64, (Vector Word8) )) -> Option13
lam_pure_475 (l0, (l1, l2)) =
  Some13_0 (l1, l0)

lam_and_then_476 :: (Parse20, Closure157) -> Parse15
lam_and_then_476 (l0, l1) =
  Parse15_0 (Variant156_1 (l0, l1))

lam_or_483 :: (Parse18, Parse19) -> Parse20
lam_or_483 (l0, l1) =
  Parse20_0 (Variant164_0 (l0, l1))

lam_map_484 :: (Parse14, Closure161) -> Parse18
lam_map_484 (l0, l1) =
  Parse18_0 (Variant160_0 (l0, l1))

lam_lazy_485 :: Closure163 -> Parse19
lam_lazy_485 l0 =
  Parse19_0 (Variant162_0 l0)

lam_optional_482 :: Parse14 -> Parse20
lam_optional_482 l0 =
  lam_or_483 (lam_map_484 (l0, Variant161_0 ()), lam_lazy_485 (Variant163_0 ()))

lam_many0_fold_457 :: (Parse14, Int64, Closure158) -> Parse15
lam_many0_fold_457 (l0, l1, l2) =
  lam_and_then_476 (lam_optional_482 l0, Variant157_0 (l1, l0, l2))

lam_many0_fold_473 :: ((Int64, Parse14, Closure158), Option0) -> Parse15
lam_many0_fold_473 ((l0, l1, l2), l3) =
  case l3 of (None0_1) -> lam_pure_474 l0; (Some0_0 l4) -> lam_many0_fold_457 (l1, lam_parse_int_456 (l0, l4), l2)

lam_wrapped_many0_fold_487 :: (Parse14, Int64, Closure158) -> Parse15
lam_wrapped_many0_fold_487 l0 =
  lam_many0_fold_457 l0

lam_many1_fold_486 :: ((Parse14, Closure158, Int64), Int64) -> Parse15
lam_many1_fold_486 ((l0, l1, l2), l3) =
  lam_wrapped_many0_fold_487 (l0, lam_parse_int_456 (l2, l3), l1)

lam_byte_eq_491 :: (Word8, Word8) -> Bool
lam_byte_eq_491 (l0, l1) =
  uncurry (==) (l1, l0)

lam_parse_var_496 :: Int64 -> VarId6
lam_parse_var_496 l0 =
  VarId6_0 l0

lam_map_499 :: (Closure171, (Int64, Int64)) -> (Int64, VarId6)
lam_map_499 (l0, (l1, l2)) =
  (l1, lam_parse_var_496 l2)

lam_parse_expr_506 :: VarId6 -> Expr7
lam_parse_expr_506 l0 =
  EVar7_0 l0

lam_map_509 :: (Closure195, (Int64, VarId6)) -> (Int64, Expr7)
lam_map_509 (l0, (l1, l2)) =
  (l1, lam_parse_expr_506 l2)

lam_parse_head_511 :: Int64 -> HeadId8
lam_parse_head_511 l0 =
  HeadId8_0 l0

lam_map_513 :: (Closure174, (Int64, Int64)) -> (Int64, HeadId8)
lam_map_513 (l0, (l1, l2)) =
  (l1, lam_parse_head_511 l2)

lam_optional_521 :: Expr7 -> Option32
lam_optional_521 l0 =
  Some32_0 l0

lam_map_524 :: (Closure183, (Int64, Expr7)) -> (Int64, Option32)
lam_map_524 (l0, (l1, l2)) =
  (l1, lam_optional_521 l2)

lam_pure_526 :: (Option32, (Int64, (Vector Word8) )) -> Option33
lam_pure_526 (l0, (l1, l2)) =
  Some33_0 (l1, l0)

lam_pure_528 :: Option32 -> Parse34
lam_pure_528 l0 =
  Parse34_0 (Variant181_0 l0)

lam_optional_527 :: () -> Parse34
lam_optional_527 () =
  lam_pure_528 (None32_1)

lam_pure_537 :: (Vector Expr7)  -> Parse31
lam_pure_537 l0 =
  Parse31_0 (Variant178_0 l0)

lam_pure_538 :: ((Vector Expr7) , (Int64, (Vector Word8) )) -> Option37
lam_pure_538 (l0, (l1, l2)) =
  Some37_0 (l1, l0)

lam_and_then_539 :: (Parse38, Closure179) -> Parse31
lam_and_then_539 (l0, l1) =
  Parse31_0 (Variant178_1 (l0, l1))

lam_or_546 :: (Parse35, Parse36) -> Parse38
lam_or_546 (l0, l1) =
  Parse38_0 (Variant186_0 (l0, l1))

lam_map_547 :: (Parse30, Closure183) -> Parse35
lam_map_547 (l0, l1) =
  Parse35_0 (Variant182_0 (l0, l1))

lam_lazy_548 :: Closure185 -> Parse36
lam_lazy_548 l0 =
  Parse36_0 (Variant184_0 l0)

lam_optional_545 :: Parse30 -> Parse38
lam_optional_545 l0 =
  lam_or_546 (lam_map_547 (l0, Variant183_0 ()), lam_lazy_548 (Variant185_0 ()))

lam_many0_fold_520 :: (Parse30, (Vector Expr7) , Closure180) -> Parse31
lam_many0_fold_520 (l0, l1, l2) =
  lam_and_then_539 (lam_optional_545 l0, Variant179_0 (l1, l0, l2))

lam_many0_fold_536 :: (((Vector Expr7) , Parse30, Closure180), Option32) -> Parse31
lam_many0_fold_536 ((l0, l1, l2), l3) =
  case l3 of (None32_1) -> lam_pure_537 l0; (Some32_0 l4) -> lam_many0_fold_520 (l1, intrinsicPush l0 l4, l2)

lam_wrapped_many0_fold_550 :: (Parse30, (Vector Expr7) , Closure180) -> Parse31
lam_wrapped_many0_fold_550 l0 =
  lam_many0_fold_520 l0

lam_many0_549 :: (Parse30, ()) -> Parse31
lam_many0_549 (l0, ()) =
  lam_wrapped_many0_fold_550 (l0, (V.fromList []), push_74 ())

lam_pure_552 :: (Expr7, (Int64, (Vector Word8) )) -> Option26
lam_pure_552 (l0, (l1, l2)) =
  Some26_0 (l1, l0)

lam_pure_554 :: Expr7 -> Parse39
lam_pure_554 l0 =
  Parse39_0 (Variant187_0 l0)

lam_parse_expr_553 :: (HeadId8, (Vector Expr7) ) -> Parse39
lam_parse_expr_553 (l0, l1) =
  lam_pure_554 (EHead7_1 (l0, l1))

lam_skip_post_564 :: (Expr7, (Int64, Word8)) -> (Int64, Expr7)
lam_skip_post_564 (l0, (l1, l_0)) =
  (l1, l0)

lam_skip_pre_567 :: (Parse23, Parse42) -> Parse29
lam_skip_pre_567 (l0, l1) =
  Parse29_0 (Variant175_0 (l0, l1))

lam_skip_post_572 :: (Parse41, Parse23) -> Parse42
lam_skip_post_572 (l0, l1) =
  Parse42_0 (Variant192_0 (l0, l1))

lam_between_560 :: (Parse23, Parse23, Parse41) -> Parse29
lam_between_560 (l0, l1, l2) =
  lam_skip_pre_567 (l0, lam_skip_post_572 (l2, l1))

lam_and_then_575 :: (Parse11, Closure168) -> Parse23
lam_and_then_575 (l0, l1) =
  Parse23_0 (Variant167_0 (l0, l1))

lam_guard_574 :: (Parse11, Closure169) -> Parse23
lam_guard_574 (l0, l1) =
  lam_and_then_575 (l0, Variant168_0 l1)

lam_byte_eq_573 :: Word8 -> Parse23
lam_byte_eq_573 l0 =
  lam_guard_574 (byte_90 (), Variant169_0 l0)

lam_and_then_576 :: (Parse40, Closure191) -> Parse41
lam_and_then_576 (l0, l1) =
  Parse41_0 (Variant190_0 (l0, l1))

lam_lazy_578 :: Closure189 -> Parse40
lam_lazy_578 l0 =
  Parse40_0 (Variant188_0 l0)

lam_many0_577 :: Parse30 -> Parse40
lam_many0_577 l0 =
  lam_lazy_578 (Variant189_0 l0)

lam_or_580 :: (Parse44, Parse45) -> Parse43
lam_or_580 (l0, l1) =
  Parse43_0 (Variant193_0 (l0, l1))

lam_map_586 :: (Parse25, Closure195) -> Parse44
lam_map_586 (l0, l1) =
  Parse44_0 (Variant194_0 (l0, l1))

lam_skip_pre_587 :: (Parse23, Parse24) -> Parse25
lam_skip_pre_587 (l0, l1) =
  Parse25_0 (Variant172_0 (l0, l1))

lam_map_588 :: (Parse21, Closure171) -> Parse24
lam_map_588 (l0, l1) =
  Parse24_0 (Variant170_0 (l0, l1))

lam_and_then_590 :: (Parse14, Closure166) -> Parse21
lam_and_then_590 (l0, l1) =
  Parse21_0 (Variant165_0 (l0, l1))

lam_many1_fold_589 :: (Parse14, Int64, Closure158) -> Parse21
lam_many1_fold_589 (l0, l1, l2) =
  lam_and_then_590 (l0, Variant166_0 (l0, l2, l1))

lam_and_then_593 :: (Parse11, Closure152) -> Parse12
lam_and_then_593 (l0, l1) =
  Parse12_0 (Variant151_0 (l0, l1))

lam_guard_592 :: (Parse11, Closure153) -> Parse12
lam_guard_592 (l0, l1) =
  lam_and_then_593 (l0, Variant152_0 l1)

lam_byte_range_591 :: (Word8, Word8) -> Parse12
lam_byte_range_591 (l0, l1) =
  lam_guard_592 (byte_90 (), Variant153_0 (l0, l1))

lam_map_594 :: (Parse12, Closure155) -> Parse14
lam_map_594 (l0, l1) =
  Parse14_0 (Variant154_0 (l0, l1))

parse_digit_110 :: () -> Parse14
parse_digit_110 () =
  let l0 = lam_byte_range_591 (ascii_zero_23 (), ascii_nine_114 ()) in lam_map_594 (l0, Variant155_0 ())

parse_int_107 :: () -> Parse21
parse_int_107 () =
  lam_many1_fold_589 (parse_digit_110 (), 0, Variant158_0 ())

parse_var_103 :: () -> Parse25
parse_var_103 () =
  lam_skip_pre_587 (lam_byte_eq_573 (ascii_question_105 ()), lam_map_588 (parse_int_107 (), Variant171_0 ()))

lam_and_then_595 :: (Parse46, Closure197) -> Parse45
lam_and_then_595 (l0, l1) =
  Parse45_0 (Variant196_0 (l0, l1))

lam_skip_pre_601 :: (Parse23, Parse28) -> Parse46
lam_skip_pre_601 (l0, l1) =
  Parse46_0 (Variant198_0 (l0, l1))

lam_map_602 :: (Parse21, Closure174) -> Parse28
lam_map_602 (l0, l1) =
  Parse28_0 (Variant173_0 (l0, l1))

parse_head_120 :: () -> Parse46
parse_head_120 () =
  lam_skip_pre_601 (lam_byte_eq_573 (ascii_excalam_122 ()), lam_map_602 (parse_int_107 (), Variant174_0 ()))

lam_parse_expr_579 :: () -> Parse43
lam_parse_expr_579 () =
  lam_or_580 (lam_map_586 (parse_var_103 (), Variant195_0 ()), lam_and_then_595 (parse_head_120 (), Variant197_0 ()))

lam_lazy_603 :: Closure177 -> Parse30
lam_lazy_603 l0 =
  Parse30_0 (Variant176_0 l0)

parse_expr_97 :: () -> Parse30
parse_expr_97 () =
  lam_lazy_603 (Variant177_0 ())

lam_parse_expr_519 :: HeadId8 -> Parse29
lam_parse_expr_519 l0 =
  lam_between_560 (lam_byte_eq_573 (ascii_open_paren_92 ()), lam_byte_eq_573 (ascii_close_paren_93 ()), lam_and_then_576 (lam_many0_577 (parse_expr_97 ()), Variant191_0 l0))

lam_pure_606 :: ((VarId6, Expr7), (Int64, (Vector Word8) )) -> Option47
lam_pure_606 (l0, (l1, l2)) =
  Some47_0 (l1, l0)

lam_pure_608 :: (VarId6, Expr7) -> Parse48
lam_pure_608 l0 =
  Parse48_0 (Variant199_0 l0)

lam_parse_command_607 :: (VarId6, Expr7) -> Parse48
lam_parse_command_607 (l0, l1) =
  lam_pure_608 (l0, l1)

lam_skip_pre_618 :: (Parse23, Parse49) -> Parse50
lam_skip_pre_618 (l0, l1) =
  Parse50_0 (Variant202_0 (l0, l1))

lam_and_then_619 :: (Parse30, Closure201) -> Parse49
lam_and_then_619 (l0, l1) =
  Parse49_0 (Variant200_0 (l0, l1))

lam_parse_command_617 :: VarId6 -> Parse50
lam_parse_command_617 l0 =
  lam_skip_pre_618 (lam_byte_eq_573 (ascii_tilde_132 ()), lam_and_then_619 (parse_expr_97 (), Variant201_0 l0))

lam_optional_624 :: (VarId6, Expr7) -> Option51
lam_optional_624 l0 =
  Some51_0 l0

lam_map_627 :: (Closure207, (Int64, (VarId6, Expr7))) -> (Int64, Option51)
lam_map_627 (l0, (l1, l2)) =
  (l1, lam_optional_624 l2)

lam_pure_629 :: (Option51, (Int64, (Vector Word8) )) -> Option53
lam_pure_629 (l0, (l1, l2)) =
  Some53_0 (l1, l0)

lam_pure_631 :: Option51 -> Parse54
lam_pure_631 l0 =
  Parse54_0 (Variant205_0 l0)

lam_optional_630 :: () -> Parse54
lam_optional_630 () =
  lam_pure_631 (None51_1)

lam_pure_639 :: ((Vector ((VarId6, Expr7))) , (Int64, (Vector Word8) )) -> Option57
lam_pure_639 (l0, (l1, l2)) =
  Some57_0 (l1, l0)

lam_pure_649 :: (Vector ((VarId6, Expr7)))  -> Parse59
lam_pure_649 l0 =
  Parse59_0 (Variant211_0 l0)

lam_and_then_650 :: (Parse61, Closure212) -> Parse59
lam_and_then_650 (l0, l1) =
  Parse59_0 (Variant211_1 (l0, l1))

lam_or_657 :: (Parse60, Parse56) -> Parse61
lam_or_657 (l0, l1) =
  Parse61_0 (Variant215_0 (l0, l1))

lam_map_658 :: (Parse58, Closure207) -> Parse60
lam_map_658 (l0, l1) =
  Parse60_0 (Variant214_0 (l0, l1))

lam_lazy_659 :: Closure209 -> Parse56
lam_lazy_659 l0 =
  Parse56_0 (Variant208_0 l0)

lam_optional_656 :: Parse58 -> Parse61
lam_optional_656 l0 =
  lam_or_657 (lam_map_658 (l0, Variant207_0 ()), lam_lazy_659 (Variant209_0 ()))

lam_many0_fold_643 :: (Parse58, (Vector ((VarId6, Expr7))) , Closure213) -> Parse59
lam_many0_fold_643 (l0, l1, l2) =
  lam_and_then_650 (lam_optional_656 l0, Variant212_0 (l1, l0, l2))

lam_many0_fold_648 :: (((Vector ((VarId6, Expr7))) , Parse58, Closure213), Option51) -> Parse59
lam_many0_fold_648 ((l0, l1, l2), l3) =
  case l3 of (None51_1) -> lam_pure_649 l0; (Some51_0 l4) -> lam_many0_fold_643 (l1, intrinsicPush l0 l4, l2)

lam_wrapped_many0_fold_661 :: (Parse58, (Vector ((VarId6, Expr7))) , Closure213) -> Parse59
lam_wrapped_many0_fold_661 l0 =
  lam_many0_fold_643 l0

lam_skip_pre_662 :: (Parse23, Parse52) -> Parse58
lam_skip_pre_662 (l0, l1) =
  Parse58_0 (Variant210_0 (l0, l1))

lam_sep0_fold_660 :: (((Vector ((VarId6, Expr7))) , Parse23, Parse52, Closure213), Option51) -> Parse59
lam_sep0_fold_660 ((l0, l1, l2, l3), l4) =
  case l4 of (None51_1) -> lam_pure_649 l0; (Some51_0 l5) -> lam_wrapped_many0_fold_661 (lam_skip_pre_662 (l1, l2), intrinsicPush l0 l5, l3)

lam_and_then_669 :: (Parse62, Closure218) -> Parse63
lam_and_then_669 (l0, l1) =
  Parse63_0 (Variant217_0 (l0, l1))

lam_or_671 :: (Parse55, Parse56) -> Parse62
lam_or_671 (l0, l1) =
  Parse62_0 (Variant216_0 (l0, l1))

lam_map_672 :: (Parse52, Closure207) -> Parse55
lam_map_672 (l0, l1) =
  Parse55_0 (Variant206_0 (l0, l1))

lam_optional_670 :: Parse52 -> Parse62
lam_optional_670 l0 =
  lam_or_671 (lam_map_672 (l0, Variant207_0 ()), lam_lazy_659 (Variant209_0 ()))

lam_sep0_fold_668 :: (Parse52, Parse23, (Vector ((VarId6, Expr7))) , Closure213) -> Parse63
lam_sep0_fold_668 (l0, l1, l2, l3) =
  lam_and_then_669 (lam_optional_670 l0, Variant218_0 (l2, l1, l0, l3))

lam_sep0_667 :: ((Parse52, Parse23), ()) -> Parse63
lam_sep0_667 ((l0, l1), ()) =
  lam_sep0_fold_668 (l0, l1, (V.fromList []), push_165 ())

lam_pure_675 :: (Problem5, (Int64, (Vector Word8) )) -> Option64
lam_pure_675 (l0, (l1, l2)) =
  Some64_0 (l1, l0)

lam_pure_677 :: Problem5 -> Parse65
lam_pure_677 l0 =
  Parse65_0 (Variant219_0 l0)

lam_parse_problem_676 :: ((Int64, VarId6), (Vector ((VarId6, Expr7))) ) -> Parse65
lam_parse_problem_676 ((l0, l1), l2) =
  lam_pure_677 (Problem5_0 (l0, l1, l2))

lam_skip_pre_688 :: (Parse23, Parse67) -> Parse68
lam_skip_pre_688 (l0, l1) =
  Parse68_0 (Variant224_0 (l0, l1))

lam_and_then_689 :: (Parse66, Closure223) -> Parse67
lam_and_then_689 (l0, l1) =
  Parse67_0 (Variant222_0 (l0, l1))

lam_lazy_691 :: Closure221 -> Parse66
lam_lazy_691 l0 =
  Parse66_0 (Variant220_0 l0)

lam_sep0_690 :: (Parse52, Parse23) -> Parse66
lam_sep0_690 (l0, l1) =
  lam_lazy_691 (Variant221_0 (l0, l1))

lam_and_then_692 :: (Parse25, Closure204) -> Parse52
lam_and_then_692 (l0, l1) =
  Parse52_0 (Variant203_0 (l0, l1))

parse_command_179 :: () -> Parse52
parse_command_179 () =
  lam_and_then_692 (parse_var_103 (), Variant204_0 ())

lam_parse_problem_687 :: (Int64, VarId6) -> Parse68
lam_parse_problem_687 (l0, l1) =
  lam_skip_pre_688 (lam_byte_eq_573 (ascii_semicolon_175 ()), lam_and_then_689 (lam_sep0_690 (parse_command_179 (), lam_byte_eq_573 (ascii_comma_181 ())), Variant223_0 (l0, l1)))

lam_skip_pre_702 :: (Parse23, Parse69) -> Parse70
lam_skip_pre_702 (l0, l1) =
  Parse70_0 (Variant227_0 (l0, l1))

lam_and_then_703 :: (Parse25, Closure226) -> Parse69
lam_and_then_703 (l0, l1) =
  Parse69_0 (Variant225_0 (l0, l1))

lam_parse_problem_701 :: Int64 -> Parse70
lam_parse_problem_701 l0 =
  lam_skip_pre_702 (lam_byte_eq_573 (ascii_semicolon_175 ()), lam_and_then_703 (parse_var_103 (), Variant226_0 l0))

lam_parse_all_711 :: ((Vector Word8) , (Int64, Problem5)) -> Option72
lam_parse_all_711 (l0, (l1, l2)) =
  case uncurry (==) (l1, intrinsicLen l0) of True -> Some72_0 l2; False -> None72_1

lam_and_then_713 :: (Parse21, Closure229) -> Parse71
lam_and_then_713 (l0, l1) =
  Parse71_0 (Variant228_0 (l0, l1))

parse_problem_194 :: () -> Parse71
parse_problem_194 () =
  lam_and_then_713 (parse_int_107 (), Variant229_0 ())

lam_range_715 :: (Int64, Int64) -> Iter74
lam_range_715 (l0, l1) =
  Iter74_0 (Variant230_0 (l0, l1))

lam_range_716 :: ((Int64, Int64), ()) -> Option75
lam_range_716 ((l0, l1), ()) =
  case uncurry (<) (l0, l1) of True -> Some75_0 (l0, lam_range_715 (uncurry (+) (l0, 1), l1)); False -> None75_1

lam_items_717 :: ((Vector Problem5) , Int64) -> Problem5
lam_items_717 (l0, l1) =
  intrinsicGet l0 l1

lam_map_718 :: (Iter74, Closure232) -> Iter76
lam_map_718 (l0, l1) =
  Iter76_0 (Variant231_0 (l0, l1))

lam_range_723 :: (Int64, Int64) -> Iter78
lam_range_723 (l0, l1) =
  Iter78_0 (Variant233_0 (l0, l1))

lam_range_724 :: ((Int64, Int64), ()) -> Option79
lam_range_724 ((l0, l1), ()) =
  case uncurry (<) (l0, l1) of True -> Some79_0 (l0, lam_range_723 (uncurry (+) (l0, 1), l1)); False -> None79_1

lam_items_725 :: ((Vector ((VarId6, Expr7))) , Int64) -> (VarId6, Expr7)
lam_items_725 (l0, l1) =
  intrinsicGet l0 l1

lam_map_726 :: (Iter78, Closure235) -> Iter80
lam_map_726 (l0, l1) =
  Iter80_0 (Variant234_0 (l0, l1))

lam_pure_729 :: ((Vector VarId6) , Terms82) -> (Terms82, (Vector VarId6) )
lam_pure_729 (l0, l1) =
  (l1, l0)

lam_pure_731 :: (Vector VarId6)  -> State84
lam_pure_731 l0 =
  State84_0 (Variant236_0 l0)

lam_mk_expr_730 :: ((Vector VarId6) , VarId6) -> State84
lam_mk_expr_730 (l0, l1) =
  lam_pure_731 (intrinsicPush l0 l1)

lam_pure_736 :: VarId6 -> State85
lam_pure_736 l0 =
  State85_0 (Variant237_0 l0)

lam_pure_737 :: (VarId6, Terms82) -> (Terms82, VarId6)
lam_pure_737 (l0, l1) =
  (l1, l0)

lam_range_738 :: (Int64, Int64) -> Iter86
lam_range_738 (l0, l1) =
  Iter86_0 (Variant239_0 (l0, l1))

lam_range_739 :: ((Int64, Int64), ()) -> Option87
lam_range_739 ((l0, l1), ()) =
  case uncurry (<) (l0, l1) of True -> Some87_0 (l0, lam_range_738 (uncurry (+) (l0, 1), l1)); False -> None87_1

lam_items_740 :: ((Vector Expr7) , Int64) -> Expr7
lam_items_740 (l0, l1) =
  intrinsicGet l0 l1

lam_map_741 :: (Iter86, Closure241) -> Iter88
lam_map_741 (l0, l1) =
  Iter88_0 (Variant240_0 (l0, l1))

lam_bind_745 :: (State85, Closure243) -> State90
lam_bind_745 (l0, l1) =
  State90_0 (Variant242_0 (l0, l1))

lam_bind_749 :: (State90, Closure245) -> State91
lam_bind_749 (l0, l1) =
  State91_0 (Variant244_1 (l0, l1))

lam_pure_753 :: (Vector VarId6)  -> State91
lam_pure_753 l0 =
  State91_0 (Variant244_0 l0)

lam_new_term_754 :: (Term83, Terms82) -> (Terms82, VarId6)
lam_new_term_754 (l0, l1) =
  let (Terms82_0 l2) = l1; l3 = VarId6_0 (intrinsicLen l2); l4 = intrinsicPush l2 l0 in (Terms82_0 l4, l3)

lam_modify_ret_757 :: Closure247 -> State92
lam_modify_ret_757 l0 =
  State92_0 l0

lam_new_term_756 :: Term83 -> State92
lam_new_term_756 l0 =
  lam_modify_ret_757 (Variant247_0 l0)

lam_mk_expr_755 :: (HeadId8, (Vector VarId6) ) -> State92
lam_mk_expr_755 (l0, l1) =
  lam_new_term_756 (Term83_2 (l0, l1))

lam_bind_758 :: (State91, Closure238) -> State85
lam_bind_758 (l0, l1) =
  State85_0 (Variant237_1 (l0, l1))

lam_wrapped_map_763 :: (Iter86, Closure241) -> Iter88
lam_wrapped_map_763 l0 =
  lam_map_741 l0

lam_wrapped_range_764 :: (Int64, Int64) -> Iter86
lam_wrapped_range_764 l0 =
  lam_range_738 l0

lam_items_762 :: (Vector Expr7)  -> Iter88
lam_items_762 l0 =
  lam_wrapped_map_763 (lam_wrapped_range_764 (0, intrinsicLen l0), Variant241_0 l0)

lam_pure_765 :: (Bool, Terms82) -> (Terms82, Bool)
lam_pure_765 (l0, l1) =
  (l1, l0)

lam_range_766 :: (Int64, Int64) -> Iter93
lam_range_766 (l0, l1) =
  Iter93_0 (Variant248_0 (l0, l1))

lam_range_767 :: ((Int64, Int64), ()) -> Option94
lam_range_767 ((l0, l1), ()) =
  case uncurry (<) (l0, l1) of True -> Some94_0 (l0, lam_range_766 (uncurry (+) (l0, 1), l1)); False -> None94_1

lam_items_768 :: ((Vector VarId6) , Int64) -> VarId6
lam_items_768 (l0, l1) =
  intrinsicGet l0 l1

lam_map_769 :: (Iter93, Closure250) -> Iter95
lam_map_769 (l0, l1) =
  Iter95_0 (Variant249_0 (l0, l1))

lam_map_772 :: (Iter93, Closure250) -> Iter97
lam_map_772 (l0, l1) =
  Iter97_0 (Variant251_0 (l0, l1))

lam_zip_774 :: (Iter95, Iter97) -> Iter99
lam_zip_774 (l0, l1) =
  Iter99_0 (Variant252_0 (l0, l1))

lam_pure_781 :: Bool -> State101
lam_pure_781 l0 =
  State101_0 (Variant253_0 l0)

lam_bind_782 :: (State102, Closure254) -> State101
lam_bind_782 (l0, l1) =
  State101_0 (Variant253_1 (l0, l1))

lam_pure_790 :: Bool -> State103
lam_pure_790 l0 =
  State103_0 (Variant258_0 l0)

lam_unify_789 :: () -> State103
lam_unify_789 () =
  lam_pure_790 True

lam_bind_795 :: (State104, Closure262) -> State105
lam_bind_795 (l0, l1) =
  State105_0 (Variant261_0 (l0, l1))

lam_modify_797 :: Closure260 -> State104
lam_modify_797 l0 =
  State104_0 (Variant259_0 l0)

lam_set_term_796 :: (VarId6, Term83) -> State104
lam_set_term_796 (l0, l1) =
  lam_modify_797 (Variant260_0 (l0, l1))

lam_unify_794 :: ((VarId6, HeadId8, (Vector VarId6) ), ()) -> State105
lam_unify_794 ((l0, l1, l2), ()) =
  lam_bind_795 (lam_set_term_796 (l0, Term83_2 (l1, l2)), Variant262_0 ())

lam_bind_801 :: (State104, Closure264) -> State106
lam_bind_801 (l0, l1) =
  State106_0 (Variant263_1 (l0, l1))

lam_pure_802 :: Bool -> State106
lam_pure_802 l0 =
  State106_0 (Variant263_0 l0)

lam_unify_800 :: ((VarId6, VarId6, HeadId8, (Vector VarId6) ), Bool) -> State106
lam_unify_800 ((l0, l1, l2, l3), l4) =
  case l4 of True -> lam_bind_801 (lam_set_term_796 (l0, Equal83_1 l1), Variant264_0 (l1, l2, l3)); False -> lam_pure_802 False

lam_head_id_eq_806 :: (HeadId8, HeadId8) -> Bool
lam_head_id_eq_806 (l0, l1) =
  let (HeadId8_0 l2) = l0; (HeadId8_0 l3) = l1 in uncurry (==) (l2, l3)

lam_pure_807 :: Bool -> State107
lam_pure_807 l0 =
  State107_0 (Variant265_0 l0)

lam_wrapped_zip_808 :: (Iter95, Iter97) -> Iter99
lam_wrapped_zip_808 l0 =
  lam_zip_774 l0

lam_wrapped_map_810 :: (Iter93, Closure250) -> Iter95
lam_wrapped_map_810 l0 =
  lam_map_769 l0

lam_wrapped_range_811 :: (Int64, Int64) -> Iter93
lam_wrapped_range_811 l0 =
  lam_range_766 l0

lam_items_809 :: (Vector VarId6)  -> Iter95
lam_items_809 l0 =
  lam_wrapped_map_810 (lam_wrapped_range_811 (0, intrinsicLen l0), Variant250_0 l0)

lam_wrapped_map_813 :: (Iter93, Closure250) -> Iter97
lam_wrapped_map_813 l0 =
  lam_map_772 l0

lam_items_812 :: (Vector VarId6)  -> Iter97
lam_items_812 l0 =
  lam_wrapped_map_813 (lam_wrapped_range_811 (0, intrinsicLen l0), Variant250_0 l0)

lam_bind_814 :: (State101, Closure266) -> State107
lam_bind_814 (l0, l1) =
  State107_0 (Variant265_1 (l0, l1))

lam_pure_818 :: VarId6 -> State108
lam_pure_818 l0 =
  State108_0 (Variant267_0 l0)

lam_follow_817 :: (VarId6, ()) -> State108
lam_follow_817 (l0, ()) =
  lam_pure_818 l0

lam_var_id_eq_822 :: (VarId6, VarId6) -> Bool
lam_var_id_eq_822 (l0, l1) =
  let (VarId6_0 l2) = l0; (VarId6_0 l3) = l1 in uncurry (==) (l2, l3)

lam_pure_823 :: VarId6 -> State109
lam_pure_823 l0 =
  State109_0 (Variant268_0 l0)

lam_bind_824 :: (State104, Closure269) -> State109
lam_bind_824 (l0, l1) =
  State109_0 (Variant268_1 (l0, l1))

lam_follow_821 :: ((VarId6, VarId6), VarId6) -> State109
lam_follow_821 ((l0, l1), l2) =
  case lam_var_id_eq_822 (l0, l2) of True -> lam_pure_823 l2; False -> lam_bind_824 (lam_set_term_796 (l1, Equal83_1 l2), Variant269_0 l2)

lam_get_829 :: Terms82 -> (Terms82, Terms82)
lam_get_829 l0 =
  (l0, l0)

lam_pure_830 :: (Term83, Terms82) -> (Terms82, Term83)
lam_pure_830 (l0, l1) =
  (l1, l0)

lam_pure_832 :: Term83 -> State111
lam_pure_832 l0 =
  State111_0 (Variant272_0 l0)

lam_get_term_831 :: (VarId6, Terms82) -> State111
lam_get_term_831 (l0, l1) =
  let (VarId6_0 l2) = l0; (Terms82_0 l3) = l1 in lam_pure_832 (intrinsicGet l3 l2)

lam_run_834 :: (State112, Terms82) -> (Terms82, Terms82)
lam_run_834 (l0, l1) =
  let (State112_0 l2) = l0 in lam_get_829 l1

lam_bind_837 :: (State110, Closure275) -> State113
lam_bind_837 (l0, l1) =
  State113_0 (Variant274_1 (l0, l1))

lam_pure_838 :: VarId6 -> State113
lam_pure_838 l0 =
  State113_0 (Variant274_0 l0)

lam_bind_839 :: (State114, Closure271) -> State110
lam_bind_839 (l0, l1) =
  State110_0 (Variant270_0 (l0, l1))

lam_bind_844 :: (State112, Closure277) -> State114
lam_bind_844 (l0, l1) =
  State114_0 (Variant276_0 (l0, l1))

lam_get_term_843 :: VarId6 -> State114
lam_get_term_843 l0 =
  lam_bind_844 (get_292 (), Variant277_0 l0)

lam_follow_828 :: VarId6 -> State110
lam_follow_828 l0 =
  lam_bind_839 (lam_get_term_843 l0, Variant271_0 l0)

lam_follow_836 :: (VarId6, Term83) -> State113
lam_follow_836 (l0, l1) =
  case l1 of (Equal83_1 l2) -> lam_bind_837 (lam_follow_828 l2, Variant275_0 (l2, l0)); l_0 -> lam_pure_838 l0

lam_bind_850 :: (State116, Closure275) -> State117
lam_bind_850 (l0, l1) =
  State117_0 (Variant282_1 (l0, l1))

lam_pure_851 :: VarId6 -> State117
lam_pure_851 l0 =
  State117_0 (Variant282_0 l0)

lam_bind_852 :: (State114, Closure281) -> State116
lam_bind_852 (l0, l1) =
  State116_0 (Variant280_0 (l0, l1))

lam_follow_848 :: VarId6 -> State116
lam_follow_848 l0 =
  lam_bind_852 (lam_get_term_843 l0, Variant281_0 l0)

lam_follow_849 :: (VarId6, Term83) -> State117
lam_follow_849 (l0, l1) =
  case l1 of (Equal83_1 l2) -> lam_bind_850 (lam_follow_848 l2, Variant275_0 (l2, l0)); l_1 -> lam_pure_851 l0

lam_pure_856 :: Bool -> State118
lam_pure_856 l0 =
  State118_0 (Variant283_0 l0)

lam_unify_859 :: () -> State103
lam_unify_859 () =
  lam_pure_790 True

lam_bind_860 :: (State104, Closure288) -> State120
lam_bind_860 (l0, l1) =
  State120_0 (Variant287_1 (l0, l1))

lam_unify_862 :: () -> State103
lam_unify_862 () =
  lam_pure_790 True

lam_bind_863 :: (State104, Closure289) -> State120
lam_bind_863 (l0, l1) =
  State120_0 (Variant287_2 (l0, l1))

lam_bind_866 :: (State104, Closure292) -> State121
lam_bind_866 (l0, l1) =
  State121_0 (Variant291_0 (l0, l1))

lam_unify_865 :: ((VarId6, HeadId8, HeadId8, (Vector VarId6) , (Vector VarId6) , VarId6), ()) -> State121
lam_unify_865 ((l0, l1, l2, l3, l4, l5), ()) =
  lam_bind_866 (lam_set_term_796 (l0, Recursing83_3), Variant292_0 (l1, l2, l3, l4, l5, l0))

lam_bind_869 :: (State104, Closure290) -> State120
lam_bind_869 (l0, l1) =
  State120_0 (Variant287_3 (l0, l1))

lam_pure_872 :: Bool -> State120
lam_pure_872 l0 =
  State120_0 (Variant287_0 l0)

lam_unify_858 :: ((Term83, VarId6, VarId6), Term83) -> State120
lam_unify_858 ((l0, l1, l2), l3) =
  case (l0, l3) of ((Equal83_1 l_2), l_3) -> panic ((V.fromList [102, 111, 108, 108, 111, 119, 32, 115, 104, 111, 117, 108, 100, 32, 110, 101, 118, 101, 114, 32, 114, 101, 116, 117, 114, 110, 32, 97, 32, 112, 111, 105, 110, 116, 101, 114, 32, 116, 111, 32, 97, 110, 32, 69, 113, 117, 97, 108])); (l_4, (Equal83_1 l_5)) -> panic ((V.fromList [102, 111, 108, 108, 111, 119, 32, 115, 104, 111, 117, 108, 100, 32, 110, 101, 118, 101, 114, 32, 114, 101, 116, 117, 114, 110, 32, 97, 32, 112, 111, 105, 110, 116, 101, 114, 32, 116, 111, 32, 97, 110, 32, 69, 113, 117, 97, 108])); ((Unknown83_0), l_6) -> lam_bind_860 (lam_set_term_796 (l1, Equal83_1 l2), Variant288_0 ()); (l_7, (Unknown83_0)) -> lam_bind_863 (lam_set_term_796 (l2, Equal83_1 l1), Variant289_0 ()); ((Term83_2 (l4, l5)), (Term83_2 (l6, l7))) -> lam_bind_869 (lam_set_term_796 (l1, Recursing83_3), Variant290_0 (l2, l4, l6, l5, l7, l1)); ((Recursing83_3), l_8) -> lam_pure_872 False; (l_9, (Recursing83_3)) -> lam_pure_872 False

lam_bind_873 :: (State114, Closure286) -> State119
lam_bind_873 (l0, l1) =
  State119_0 (Variant285_0 (l0, l1))

lam_unify_857 :: ((VarId6, VarId6), Term83) -> State119
lam_unify_857 ((l0, l1), l2) =
  lam_bind_873 (lam_get_term_843 l0, Variant286_0 (l2, l1, l0))

lam_bind_876 :: (State114, Closure284) -> State118
lam_bind_876 (l0, l1) =
  State118_0 (Variant283_1 (l0, l1))

lam_unify_855 :: (VarId6, VarId6) -> State118
lam_unify_855 (l0, l1) =
  case lam_var_id_eq_822 (l0, l1) of True -> lam_pure_856 True; False -> lam_bind_876 (lam_get_term_843 l0, Variant284_0 (l1, l0))

lam_bind_879 :: (State116, Closure279) -> State115
lam_bind_879 (l0, l1) =
  State115_0 (Variant278_0 (l0, l1))

lam_wrapped_follow_882 :: VarId6 -> State116
lam_wrapped_follow_882 l0 =
  lam_follow_848 l0

lam_unify_845 :: (VarId6, VarId6) -> State115
lam_unify_845 (l0, l1) =
  lam_bind_879 (lam_wrapped_follow_882 l0, Variant279_0 l1)

lam_bind_883 :: (State110, Closure257) -> State102
lam_bind_883 (l0, l1) =
  State102_0 (Variant256_0 (l0, l1))

lam_wrapped_follow_886 :: VarId6 -> State110
lam_wrapped_follow_886 l0 =
  lam_follow_828 l0

lam_unify_816 :: (VarId6, VarId6) -> State102
lam_unify_816 (l0, l1) =
  lam_bind_883 (lam_wrapped_follow_886 l0, Variant257_0 l1)

lam_wrapped_unify_888 :: (VarId6, VarId6) -> State102
lam_wrapped_unify_888 l0 =
  lam_unify_816 l0

lam_solve_887 :: (VarId6, VarId6) -> State102
lam_solve_887 (l0, l1) =
  lam_wrapped_unify_888 (l0, l1)

lam_bind_891 :: (State85, Closure294) -> State122
lam_bind_891 (l0, l1) =
  State122_0 (Variant293_0 (l0, l1))

lam_pure_896 :: Bool -> State123
lam_pure_896 l0 =
  State123_0 (Variant295_0 l0)

lam_bind_897 :: (State122, Closure296) -> State123
lam_bind_897 (l0, l1) =
  State123_0 (Variant295_1 (l0, l1))

lam_pure_901 :: ((Vector Expr7) , Terms82) -> (Terms82, (Vector Expr7) )
lam_pure_901 (l0, l1) =
  (l1, l0)

lam_pure_903 :: (Vector Expr7)  -> State124
lam_pure_903 l0 =
  State124_0 (Variant298_0 l0)

lam_get_expr_902 :: ((Vector Expr7) , Expr7) -> State124
lam_get_expr_902 (l0, l1) =
  lam_pure_903 (intrinsicPush l0 l1)

lam_pure_907 :: (Expr7, Terms82) -> (Terms82, Expr7)
lam_pure_907 (l0, l1) =
  (l1, l0)

lam_map_908 :: (Iter93, Closure250) -> Iter126
lam_map_908 (l0, l1) =
  Iter126_0 (Variant301_0 (l0, l1))

lam_bind_911 :: (State125, Closure303) -> State128
lam_bind_911 (l0, l1) =
  State128_0 (Variant302_0 (l0, l1))

lam_bind_917 :: (State129, Closure275) -> State130
lam_bind_917 (l0, l1) =
  State130_0 (Variant306_1 (l0, l1))

lam_pure_918 :: VarId6 -> State130
lam_pure_918 l0 =
  State130_0 (Variant306_0 l0)

lam_bind_919 :: (State114, Closure305) -> State129
lam_bind_919 (l0, l1) =
  State129_0 (Variant304_0 (l0, l1))

lam_follow_915 :: VarId6 -> State129
lam_follow_915 l0 =
  lam_bind_919 (lam_get_term_843 l0, Variant305_0 l0)

lam_follow_916 :: (VarId6, Term83) -> State130
lam_follow_916 (l0, l1) =
  case l1 of (Equal83_1 l2) -> lam_bind_917 (lam_follow_915 l2, Variant275_0 (l2, l0)); l_10 -> lam_pure_918 l0

lam_pure_924 :: Expr7 -> State132
lam_pure_924 l0 =
  State132_0 (Variant309_0 l0)

lam_pure_926 :: Expr7 -> State133
lam_pure_926 l0 =
  State133_0 (Variant311_0 l0)

lam_get_expr_925 :: (HeadId8, (Vector Expr7) ) -> State133
lam_get_expr_925 (l0, l1) =
  lam_pure_926 (EHead7_1 (l0, l1))

lam_bind_927 :: (State134, Closure310) -> State132
lam_bind_927 (l0, l1) =
  State132_0 (Variant309_1 (l0, l1))

lam_bind_935 :: (State128, Closure313) -> State134
lam_bind_935 (l0, l1) =
  State134_0 (Variant312_1 (l0, l1))

lam_pure_938 :: (Vector Expr7)  -> State134
lam_pure_938 l0 =
  State134_0 (Variant312_0 l0)

lam_wrapped_map_940 :: (Iter93, Closure250) -> Iter126
lam_wrapped_map_940 l0 =
  lam_map_908 l0

lam_items_939 :: (Vector VarId6)  -> Iter126
lam_items_939 l0 =
  lam_wrapped_map_940 (lam_wrapped_range_811 (0, intrinsicLen l0), Variant250_0 l0)

lam_bind_941 :: (State114, Closure308) -> State131
lam_bind_941 (l0, l1) =
  State131_0 (Variant307_0 (l0, l1))

lam_get_expr_922 :: VarId6 -> State131
lam_get_expr_922 l0 =
  lam_bind_941 (lam_get_term_843 l0, Variant308_0 l0)

lam_bind_944 :: (State129, Closure300) -> State125
lam_bind_944 (l0, l1) =
  State125_0 (Variant299_0 (l0, l1))

lam_wrapped_follow_947 :: VarId6 -> State129
lam_wrapped_follow_947 l0 =
  lam_follow_915 l0

lam_get_expr_912 :: VarId6 -> State125
lam_get_expr_912 l0 =
  lam_bind_944 (lam_wrapped_follow_947 l0, Variant300_0 ())

lam_get_expr_910 :: ((Vector Expr7) , VarId6) -> State128
lam_get_expr_910 (l0, l1) =
  lam_bind_911 (lam_get_expr_912 l1, Variant303_0 l0)

lam_pure_948 :: (Option32, Terms82) -> (Terms82, Option32)
lam_pure_948 (l0, l1) =
  (l1, l0)

lam_pure_950 :: Option32 -> State135
lam_pure_950 l0 =
  State135_0 (Variant315_0 l0)

lam_solve_949 :: Expr7 -> State135
lam_solve_949 l0 =
  lam_pure_950 (Some32_0 l0)

lam_bind_954 :: (State125, Closure317) -> State136
lam_bind_954 (l0, l1) =
  State136_0 (Variant316_1 (l0, l1))

lam_wrapped_get_expr_955 :: VarId6 -> State125
lam_wrapped_get_expr_955 l0 =
  lam_get_expr_912 l0

lam_pure_956 :: Option32 -> State136
lam_pure_956 l0 =
  State136_0 (Variant316_0 l0)

lam_solve_953 :: (VarId6, Bool) -> State136
lam_solve_953 (l0, l1) =
  case l1 of True -> lam_bind_954 (lam_wrapped_get_expr_955 l0, Variant317_0 ()); False -> lam_pure_956 (None32_1)

lam_bind_959 :: (State123, Closure319) -> State137
lam_bind_959 (l0, l1) =
  State137_0 (Variant318_0 (l0, l1))

lam_wrapped_map_962 :: (Iter78, Closure235) -> Iter80
lam_wrapped_map_962 l0 =
  lam_map_726 l0

lam_wrapped_range_963 :: (Int64, Int64) -> Iter78
lam_wrapped_range_963 l0 =
  lam_range_723 l0

lam_items_961 :: (Vector ((VarId6, Expr7)))  -> Iter80
lam_items_961 l0 =
  lam_wrapped_map_962 (lam_wrapped_range_963 (0, intrinsicLen l0), Variant235_0 l0)

lam_solve_964 :: () -> Term83
lam_solve_964 l_11 =
  Unknown83_0

lam_fill_with_rec_967 :: ((Vector Term83) , Int64, Closure747) -> (Vector Term83) 
lam_fill_with_rec_967 (l0, l1, l2) =
  case uncurry (>) (l1, 0) of True -> lam_fill_with_rec_967 (intrinsicPush l0 (lam_solve_964 ()), uncurry (-) (l1, 1), l2); False -> l0

lam_wrapped_fill_with_rec_966 :: ((Vector Term83) , Int64, Closure747) -> (Vector Term83) 
lam_wrapped_fill_with_rec_966 l0 =
  lam_fill_with_rec_967 l0

lam_fill_with_965 :: (Int64, Closure747) -> (Vector Term83) 
lam_fill_with_965 (l0, l1) =
  lam_wrapped_fill_with_rec_966 (intrinsicReserve ((V.fromList [])) l0, l0, l1)

lam_map_969 :: (Iter76, Closure321) -> Iter138
lam_map_969 (l0, l1) =
  Iter138_0 (Variant320_0 (l0, l1))

lam_wrapped_map_972 :: (Iter76, Closure321) -> Iter138
lam_wrapped_map_972 l0 =
  lam_map_969 l0

lam_wrapped_map_979 :: (Iter74, Closure232) -> Iter76
lam_wrapped_map_979 l0 =
  lam_map_718 l0

lam_wrapped_range_980 :: (Int64, Int64) -> Iter74
lam_wrapped_range_980 l0 =
  lam_range_715 l0

lam_items_978 :: (Vector Problem5)  -> Iter76
lam_items_978 l0 =
  lam_wrapped_map_979 (lam_wrapped_range_980 (0, intrinsicLen l0), Variant232_0 l0)

lam_range_983 :: (Int64, Int64) -> Iter140
lam_range_983 (l0, l1) =
  Iter140_0 (Variant322_0 (l0, l1))

lam_range_984 :: ((Int64, Int64), ()) -> Option141
lam_range_984 ((l0, l1), ()) =
  case uncurry (<) (l0, l1) of True -> Some141_0 (l0, lam_range_983 (uncurry (+) (l0, 1), l1)); False -> None141_1

lam_items_985 :: ((Vector Option32) , Int64) -> Option32
lam_items_985 (l0, l1) =
  intrinsicGet l0 l1

lam_map_986 :: (Iter140, Closure324) -> Iter142
lam_map_986 (l0, l1) =
  Iter142_0 (Variant323_0 (l0, l1))

lam_concat_from_995 :: ((Vector Word8) , (Vector Word8) , Int64) -> (Vector Word8) 
lam_concat_from_995 (l0, l1, l2) =
  case uncurry (==) (l2, intrinsicLen l1) of True -> l0; False -> lam_concat_from_995 (intrinsicPush l0 (intrinsicGet l1 l2), l1, uncurry (+) (l2, 1))

lam_wrapped_concat_from_994 :: ((Vector Word8) , (Vector Word8) , Int64) -> (Vector Word8) 
lam_wrapped_concat_from_994 l0 =
  lam_concat_from_995 l0

lam_concat_993 :: ((Vector Word8) , (Vector Word8) ) -> (Vector Word8) 
lam_concat_993 (l0, l1) =
  lam_wrapped_concat_from_994 (l0, l1, 0)

lam_nat_to_string_998 :: Int64 -> (Vector Word8) 
lam_nat_to_string_998 l0 =
  case l0 of 0 -> (V.fromList [48]); 1 -> (V.fromList [49]); 2 -> (V.fromList [50]); 3 -> (V.fromList [51]); 4 -> (V.fromList [52]); 5 -> (V.fromList [53]); 6 -> (V.fromList [54]); 7 -> (V.fromList [55]); 8 -> (V.fromList [56]); 9 -> (V.fromList [57]); l_12 -> (V.fromList [])

lam_nat_to_string_997 :: Int64 -> (Vector Word8) 
lam_nat_to_string_997 l0 =
  let l1 = Variant750_0 () in (case uncurry (==) (l0, 0) of True -> (V.fromList []); False -> lam_concat_993 (lam_nat_to_string_997 (uncurry divv (l0, 10)), lam_nat_to_string_998 (uncurry (-) (l0, uncurry (*) (uncurry divv (l0, 10), 10)))))

lam_wrapped_nat_to_string_996 :: Int64 -> (Vector Word8) 
lam_wrapped_nat_to_string_996 l0 =
  lam_nat_to_string_997 l0

lam_int_to_string_992 :: Int64 -> (Vector Word8) 
lam_int_to_string_992 l0 =
  case uncurry (==) (l0, 0) of True -> (V.fromList [48]); False -> (case uncurry (<) (l0, 0) of True -> lam_concat_993 ((V.fromList [45]), lam_wrapped_nat_to_string_996 (uncurry (-) (0, l0))); False -> lam_wrapped_nat_to_string_996 l0)

lam_map_999 :: (Iter86, Closure241) -> Iter144
lam_map_999 (l0, l1) =
  Iter144_0 (Variant325_0 (l0, l1))

lam_wrapped_map_1005 :: (Iter86, Closure241) -> Iter144
lam_wrapped_map_1005 l0 =
  lam_map_999 l0

lam_items_1004 :: (Vector Expr7)  -> Iter144
lam_items_1004 l0 =
  lam_wrapped_map_1005 (lam_wrapped_range_764 (0, intrinsicLen l0), Variant241_0 l0)

lam_wrapped_map_1010 :: (Iter140, Closure324) -> Iter142
lam_wrapped_map_1010 l0 =
  lam_map_986 l0

lam_wrapped_range_1011 :: (Int64, Int64) -> Iter140
lam_wrapped_range_1011 l0 =
  lam_range_983 l0

lam_items_1009 :: (Vector Option32)  -> Iter142
lam_items_1009 l0 =
  lam_wrapped_map_1010 (lam_wrapped_range_1011 (0, intrinsicLen l0), Variant324_0 l0)

lam_writeln_1012 :: (Vector Word8)  -> ()
lam_writeln_1012 l0 =
  let l_0 = output l0 in l_0 `seq` output ((V.fromList [10]))

dispatch_1014 :: (Closure148, Int64) -> Word8
dispatch_1014 (l0, l1) =
  case l0 of (Variant148_0 l2) -> lam_items_420 (l2, l1)

dispatch_1015 :: (Closure146, ()) -> Option2
dispatch_1015 (l0, l1) =
  case l0 of (Variant146_0 l2) -> lam_range_419 (l2, l1)

lam_next_423 :: Iter1 -> Option2
lam_next_423 l0 =
  let (Iter1_0 l1) = l0 in dispatch_1015 (l1, ())

lam_map_422 :: ((Iter1, Closure148), ()) -> Option4
lam_map_422 ((l0, l1), ()) =
  case lam_next_423 l0 of (Some2_0 (l2, l3)) -> Some4_0 (dispatch_1014 (l1, l2), lam_map_421 (l3, l1)); (None2_1) -> None4_1

dispatch_1016 :: (Closure147, ()) -> Option4
dispatch_1016 (l0, l1) =
  case l0 of (Variant147_0 l2) -> lam_map_422 (l2, l1)

lam_next_425 :: Iter3 -> Option4
lam_next_425 l0 =
  let (Iter3_0 l1) = l0 in dispatch_1016 (l1, ())

dispatch_1017 :: (Closure711, Int64) -> Int64
dispatch_1017 (l0, l1) =
  case l0 of (Variant711_0 l2) -> lam_chars_to_nat_428 (l2, l1)

lam_map_429 :: (Option0, Closure711) -> Option0
lam_map_429 (l0, l1) =
  case l0 of (Some0_0 l2) -> Some0_0 (dispatch_1017 (l1, l2)); (None0_1) -> None0_1

lam_chars_to_nat_427 :: (Word8, Int64) -> Option0
lam_chars_to_nat_427 (l0, l1) =
  lam_map_429 (lam_digit_to_nat_430 l0, Variant711_0 l1)

dispatch_1018 :: (Closure710, Int64) -> Option0
dispatch_1018 (l0, l1) =
  case l0 of (Variant710_0 l2) -> lam_chars_to_nat_427 (l2, l1)

lam_and_then_431 :: (Option0, Closure710) -> Option0
lam_and_then_431 (l0, l1) =
  case l0 of (Some0_0 l2) -> dispatch_1018 (l1, l2); (None0_1) -> None0_1

lam_chars_to_nat_426 :: (Option0, Word8) -> Option0
lam_chars_to_nat_426 (l0, l1) =
  lam_and_then_431 (l0, Variant710_0 l1)

lam_foldl_433 :: (Iter3, Option0, Closure709) -> Option0
lam_foldl_433 (l0, l1, l2) =
  case lam_next_425 l0 of (Some4_0 (l3, l4)) -> lam_foldl_433 (l4, lam_chars_to_nat_426 (l1, l3), l2); (None4_1) -> l1

lam_wrapped_foldl_432 :: (Iter3, Option0, Closure709) -> Option0
lam_wrapped_foldl_432 l0 =
  lam_foldl_433 l0

lam_chars_to_nat_424 :: Iter3 -> Option0
lam_chars_to_nat_424 l0 =
  case lam_next_425 l0 of (None4_1) -> None0_1; (Some4_0 l_0) -> lam_wrapped_foldl_432 (l0, Some0_0 0, Variant709_0 ())

lam_string_to_nat_417 :: (Vector Word8)  -> Option0
lam_string_to_nat_417 l0 =
  lam_chars_to_nat_424 (lam_items_434 l0)

dispatch_1019 :: (Closure153, Word8) -> Bool
dispatch_1019 (l0, l1) =
  case l0 of (Variant153_0 l2) -> lam_byte_range_441 (l2, l1)

lam_guard_444 :: (Closure153, Word8) -> Parse10
lam_guard_444 (l0, l1) =
  case dispatch_1019 (l0, l1) of True -> lam_pure_445 l1; False -> fail_19 ()

dispatch_1020 :: (Closure152, Word8) -> Parse10
dispatch_1020 (l0, l1) =
  case l0 of (Variant152_0 l2) -> lam_guard_444 (l2, l1)

dispatch_1021 :: (Closure149, (Int64, (Vector Word8) )) -> Option9
dispatch_1021 (l0, l1) =
  case l0 of (Variant149_0 l2) -> lam_pure_442 (l2, l1); (Variant149_1 l2) -> lam_fail_443 l1

lam_parse_from_449 :: (Int64, (Vector Word8) , Parse10) -> Option9
lam_parse_from_449 (l0, l1, l2) =
  let (Parse10_0 l3) = l2 in dispatch_1021 (l3, (l0, l1))

lam_and_then_448 :: (((Vector Word8) , Closure152), (Int64, Word8)) -> Option9
lam_and_then_448 ((l0, l1), (l2, l3)) =
  lam_parse_from_449 (l2, l0, dispatch_1020 (l1, l3))

dispatch_1022 :: (Closure712, (Int64, Word8)) -> Option9
dispatch_1022 (l0, l1) =
  case l0 of (Variant712_0 l2) -> lam_and_then_448 (l2, l1)

lam_and_then_450 :: (Option9, Closure712) -> Option9
lam_and_then_450 (l0, l1) =
  case l0 of (Some9_0 l2) -> dispatch_1022 (l1, l2); (None9_1) -> None9_1

lam_and_then_446 :: ((Parse11, Closure152), (Int64, (Vector Word8) )) -> Option9
lam_and_then_446 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_447 (l2, l3, l0) in lam_and_then_450 (l4, Variant712_0 (l3, l1))

dispatch_1023 :: (Closure151, (Int64, (Vector Word8) )) -> Option9
dispatch_1023 (l0, l1) =
  case l0 of (Variant151_0 l2) -> lam_and_then_446 (l2, l1)

lam_parse_from_453 :: (Int64, (Vector Word8) , Parse12) -> Option9
lam_parse_from_453 (l0, l1, l2) =
  let (Parse12_0 l3) = l2 in dispatch_1023 (l3, (l0, l1))

dispatch_1024 :: (Closure713, (Int64, Word8)) -> (Int64, Int64)
dispatch_1024 (l0, l1) =
  case l0 of (Variant713_0 l2) -> lam_map_454 (l2, l1)

lam_map_455 :: (Option9, Closure713) -> Option13
lam_map_455 (l0, l1) =
  case l0 of (Some9_0 l2) -> Some13_0 (dispatch_1024 (l1, l2)); (None9_1) -> None13_1

lam_map_452 :: ((Parse12, Closure155), (Int64, (Vector Word8) )) -> Option13
lam_map_452 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_453 (l2, l3, l0) in lam_map_455 (l4, Variant713_0 l1)

dispatch_1025 :: (Closure154, (Int64, (Vector Word8) )) -> Option13
dispatch_1025 (l0, l1) =
  case l0 of (Variant154_0 l2) -> lam_map_452 (l2, l1)

lam_parse_from_460 :: (Int64, (Vector Word8) , Parse14) -> Option13
lam_parse_from_460 (l0, l1, l2) =
  let (Parse14_0 l3) = l2 in dispatch_1025 (l3, (l0, l1))

dispatch_1026 :: (Closure714, (Int64, Int64)) -> (Int64, Option0)
dispatch_1026 (l0, l1) =
  case l0 of (Variant714_0 l2) -> lam_map_461 (l2, l1)

lam_map_462 :: (Option13, Closure714) -> Option16
lam_map_462 (l0, l1) =
  case l0 of (Some13_0 l2) -> Some16_0 (dispatch_1026 (l1, l2)); (None13_1) -> None16_1

lam_map_459 :: ((Parse14, Closure161), (Int64, (Vector Word8) )) -> Option16
lam_map_459 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_460 (l2, l3, l0) in lam_map_462 (l4, Variant714_0 l1)

dispatch_1027 :: (Closure159, (Int64, (Vector Word8) )) -> Option16
dispatch_1027 (l0, l1) =
  case l0 of (Variant159_0 l2) -> lam_pure_463 (l2, l1)

lam_parse_from_467 :: (Int64, (Vector Word8) , Parse17) -> Option16
lam_parse_from_467 (l0, l1, l2) =
  let (Parse17_0 l3) = l2 in dispatch_1027 (l3, (l0, l1))

lam_lazy_466 :: (Closure163, (Int64, (Vector Word8) )) -> Option16
lam_lazy_466 (l0, (l1, l2)) =
  lam_parse_from_467 (l1, l2, lam_optional_464 ())

dispatch_1028 :: (Closure160, (Int64, (Vector Word8) )) -> Option16
dispatch_1028 (l0, l1) =
  case l0 of (Variant160_0 l2) -> lam_map_459 (l2, l1)

lam_parse_from_469 :: (Int64, (Vector Word8) , Parse18) -> Option16
lam_parse_from_469 (l0, l1, l2) =
  let (Parse18_0 l3) = l2 in dispatch_1028 (l3, (l0, l1))

dispatch_1029 :: (Closure162, (Int64, (Vector Word8) )) -> Option16
dispatch_1029 (l0, l1) =
  case l0 of (Variant162_0 l2) -> lam_lazy_466 (l2, l1)

lam_parse_from_471 :: (Int64, (Vector Word8) , Parse19) -> Option16
lam_parse_from_471 (l0, l1, l2) =
  let (Parse19_0 l3) = l2 in dispatch_1029 (l3, (l0, l1))

lam_or_470 :: ((Int64, (Vector Word8) , Parse19), ()) -> Option16
lam_or_470 ((l0, l1, l2), ()) =
  lam_parse_from_471 (l0, l1, l2)

dispatch_1030 :: (Closure715, ()) -> Option16
dispatch_1030 (l0, l1) =
  case l0 of (Variant715_0 l2) -> lam_or_470 (l2, l1)

lam_or_else_472 :: (Option16, Closure715) -> Option16
lam_or_else_472 (l0, l1) =
  case l0 of (Some16_0 l2) -> Some16_0 l2; (None16_1) -> dispatch_1030 (l1, ())

lam_or_468 :: ((Parse18, Parse19), (Int64, (Vector Word8) )) -> Option16
lam_or_468 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_469 (l2, l3, l0) in lam_or_else_472 (l4, Variant715_0 (l2, l3, l1))

dispatch_1031 :: (Closure164, (Int64, (Vector Word8) )) -> Option16
dispatch_1031 (l0, l1) =
  case l0 of (Variant164_0 l2) -> lam_or_468 (l2, l1)

lam_parse_from_478 :: (Int64, (Vector Word8) , Parse20) -> Option16
lam_parse_from_478 (l0, l1, l2) =
  let (Parse20_0 l3) = l2 in dispatch_1031 (l3, (l0, l1))

dispatch_1032 :: (Closure157, Option0) -> Parse15
dispatch_1032 (l0, l1) =
  case l0 of (Variant157_0 l2) -> lam_many0_fold_473 (l2, l1)

dispatch_1034 :: (Closure716, (Int64, Option0)) -> Option13
dispatch_1034 (l0, l1) =
  case l0 of (Variant716_0 l2) -> lam_and_then_479 (l2, l1)

lam_and_then_479 ((l0, l1), (l2, l3)) =
  lam_parse_from_480 (l2, l0, dispatch_1032 (l1, l3))

lam_parse_from_480 (l0, l1, l2) =
  let (Parse15_0 l3) = l2 in dispatch_1033 (l3, (l0, l1))

dispatch_1033 (l0, l1) =
  case l0 of (Variant156_0 l2) -> lam_pure_475 (l2, l1); (Variant156_1 l2) -> lam_and_then_477 (l2, l1)

lam_and_then_477 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_478 (l2, l3, l0) in lam_and_then_481 (l4, Variant716_0 (l3, l1))

lam_and_then_481 (l0, l1) =
  case l0 of (Some16_0 l2) -> dispatch_1034 (l1, l2); (None16_1) -> None13_1

dispatch_1035 :: (Closure166, Int64) -> Parse15
dispatch_1035 (l0, l1) =
  case l0 of (Variant166_0 l2) -> lam_many1_fold_486 (l2, l1)

lam_and_then_489 :: (((Vector Word8) , Closure166), (Int64, Int64)) -> Option13
lam_and_then_489 ((l0, l1), (l2, l3)) =
  lam_parse_from_480 (l2, l0, dispatch_1035 (l1, l3))

dispatch_1036 :: (Closure717, (Int64, Int64)) -> Option13
dispatch_1036 (l0, l1) =
  case l0 of (Variant717_0 l2) -> lam_and_then_489 (l2, l1)

lam_and_then_490 :: (Option13, Closure717) -> Option13
lam_and_then_490 (l0, l1) =
  case l0 of (Some13_0 l2) -> dispatch_1036 (l1, l2); (None13_1) -> None13_1

lam_and_then_488 :: ((Parse14, Closure166), (Int64, (Vector Word8) )) -> Option13
lam_and_then_488 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_460 (l2, l3, l0) in lam_and_then_490 (l4, Variant717_0 (l3, l1))

dispatch_1037 :: (Closure169, Word8) -> Bool
dispatch_1037 (l0, l1) =
  case l0 of (Variant169_0 l2) -> lam_byte_eq_491 (l2, l1)

lam_guard_492 :: (Closure169, Word8) -> Parse10
lam_guard_492 (l0, l1) =
  case dispatch_1037 (l0, l1) of True -> lam_pure_445 l1; False -> fail_19 ()

dispatch_1038 :: (Closure168, Word8) -> Parse10
dispatch_1038 (l0, l1) =
  case l0 of (Variant168_0 l2) -> lam_guard_492 (l2, l1)

lam_and_then_494 :: (((Vector Word8) , Closure168), (Int64, Word8)) -> Option9
lam_and_then_494 ((l0, l1), (l2, l3)) =
  lam_parse_from_449 (l2, l0, dispatch_1038 (l1, l3))

dispatch_1039 :: (Closure718, (Int64, Word8)) -> Option9
dispatch_1039 (l0, l1) =
  case l0 of (Variant718_0 l2) -> lam_and_then_494 (l2, l1)

lam_and_then_495 :: (Option9, Closure718) -> Option9
lam_and_then_495 (l0, l1) =
  case l0 of (Some9_0 l2) -> dispatch_1039 (l1, l2); (None9_1) -> None9_1

lam_and_then_493 :: ((Parse11, Closure168), (Int64, (Vector Word8) )) -> Option9
lam_and_then_493 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_447 (l2, l3, l0) in lam_and_then_495 (l4, Variant718_0 (l3, l1))

dispatch_1040 :: (Closure165, (Int64, (Vector Word8) )) -> Option13
dispatch_1040 (l0, l1) =
  case l0 of (Variant165_0 l2) -> lam_and_then_488 (l2, l1)

lam_parse_from_498 :: (Int64, (Vector Word8) , Parse21) -> Option13
lam_parse_from_498 (l0, l1, l2) =
  let (Parse21_0 l3) = l2 in dispatch_1040 (l3, (l0, l1))

dispatch_1041 :: (Closure719, (Int64, Int64)) -> (Int64, VarId6)
dispatch_1041 (l0, l1) =
  case l0 of (Variant719_0 l2) -> lam_map_499 (l2, l1)

lam_map_500 :: (Option13, Closure719) -> Option22
lam_map_500 (l0, l1) =
  case l0 of (Some13_0 l2) -> Some22_0 (dispatch_1041 (l1, l2)); (None13_1) -> None22_1

lam_map_497 :: ((Parse21, Closure171), (Int64, (Vector Word8) )) -> Option22
lam_map_497 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_498 (l2, l3, l0) in lam_map_500 (l4, Variant719_0 l1)

dispatch_1042 :: (Closure167, (Int64, (Vector Word8) )) -> Option9
dispatch_1042 (l0, l1) =
  case l0 of (Variant167_0 l2) -> lam_and_then_493 (l2, l1)

lam_parse_from_502 :: (Int64, (Vector Word8) , Parse23) -> Option9
lam_parse_from_502 (l0, l1, l2) =
  let (Parse23_0 l3) = l2 in dispatch_1042 (l3, (l0, l1))

dispatch_1043 :: (Closure170, (Int64, (Vector Word8) )) -> Option22
dispatch_1043 (l0, l1) =
  case l0 of (Variant170_0 l2) -> lam_map_497 (l2, l1)

lam_parse_from_504 :: (Int64, (Vector Word8) , Parse24) -> Option22
lam_parse_from_504 (l0, l1, l2) =
  let (Parse24_0 l3) = l2 in dispatch_1043 (l3, (l0, l1))

lam_skip_pre_503 :: (((Vector Word8) , Parse24), (Int64, Word8)) -> Option22
lam_skip_pre_503 ((l0, l1), (l2, l_0)) =
  lam_parse_from_504 (l2, l0, l1)

dispatch_1044 :: (Closure720, (Int64, Word8)) -> Option22
dispatch_1044 (l0, l1) =
  case l0 of (Variant720_0 l2) -> lam_skip_pre_503 (l2, l1)

lam_and_then_505 :: (Option9, Closure720) -> Option22
lam_and_then_505 (l0, l1) =
  case l0 of (Some9_0 l2) -> dispatch_1044 (l1, l2); (None9_1) -> None22_1

lam_skip_pre_501 :: ((Parse23, Parse24), (Int64, (Vector Word8) )) -> Option22
lam_skip_pre_501 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_502 (l2, l3, l0) in lam_and_then_505 (l4, Variant720_0 (l3, l1))

dispatch_1045 :: (Closure172, (Int64, (Vector Word8) )) -> Option22
dispatch_1045 (l0, l1) =
  case l0 of (Variant172_0 l2) -> lam_skip_pre_501 (l2, l1)

lam_parse_from_508 :: (Int64, (Vector Word8) , Parse25) -> Option22
lam_parse_from_508 (l0, l1, l2) =
  let (Parse25_0 l3) = l2 in dispatch_1045 (l3, (l0, l1))

dispatch_1046 :: (Closure721, (Int64, VarId6)) -> (Int64, Expr7)
dispatch_1046 (l0, l1) =
  case l0 of (Variant721_0 l2) -> lam_map_509 (l2, l1)

lam_map_510 :: (Option22, Closure721) -> Option26
lam_map_510 (l0, l1) =
  case l0 of (Some22_0 l2) -> Some26_0 (dispatch_1046 (l1, l2)); (None22_1) -> None26_1

lam_map_507 :: ((Parse25, Closure195), (Int64, (Vector Word8) )) -> Option26
lam_map_507 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_508 (l2, l3, l0) in lam_map_510 (l4, Variant721_0 l1)

dispatch_1047 :: (Closure722, (Int64, Int64)) -> (Int64, HeadId8)
dispatch_1047 (l0, l1) =
  case l0 of (Variant722_0 l2) -> lam_map_513 (l2, l1)

lam_map_514 :: (Option13, Closure722) -> Option27
lam_map_514 (l0, l1) =
  case l0 of (Some13_0 l2) -> Some27_0 (dispatch_1047 (l1, l2)); (None13_1) -> None27_1

lam_map_512 :: ((Parse21, Closure174), (Int64, (Vector Word8) )) -> Option27
lam_map_512 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_498 (l2, l3, l0) in lam_map_514 (l4, Variant722_0 l1)

dispatch_1048 :: (Closure173, (Int64, (Vector Word8) )) -> Option27
dispatch_1048 (l0, l1) =
  case l0 of (Variant173_0 l2) -> lam_map_512 (l2, l1)

lam_parse_from_517 :: (Int64, (Vector Word8) , Parse28) -> Option27
lam_parse_from_517 (l0, l1, l2) =
  let (Parse28_0 l3) = l2 in dispatch_1048 (l3, (l0, l1))

lam_skip_pre_516 :: (((Vector Word8) , Parse28), (Int64, Word8)) -> Option27
lam_skip_pre_516 ((l0, l1), (l2, l_0)) =
  lam_parse_from_517 (l2, l0, l1)

dispatch_1049 :: (Closure723, (Int64, Word8)) -> Option27
dispatch_1049 (l0, l1) =
  case l0 of (Variant723_0 l2) -> lam_skip_pre_516 (l2, l1)

lam_and_then_518 :: (Option9, Closure723) -> Option27
lam_and_then_518 (l0, l1) =
  case l0 of (Some9_0 l2) -> dispatch_1049 (l1, l2); (None9_1) -> None27_1

lam_skip_pre_515 :: ((Parse23, Parse28), (Int64, (Vector Word8) )) -> Option27
lam_skip_pre_515 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_502 (l2, l3, l0) in lam_and_then_518 (l4, Variant723_0 (l3, l1))

dispatch_1051 :: (Closure724, (Int64, Expr7)) -> (Int64, Option32)
dispatch_1051 (l0, l1) =
  case l0 of (Variant724_0 l2) -> lam_map_524 (l2, l1)

lam_map_525 :: (Option26, Closure724) -> Option33
lam_map_525 (l0, l1) =
  case l0 of (Some26_0 l2) -> Some33_0 (dispatch_1051 (l1, l2)); (None26_1) -> None33_1

dispatch_1052 :: (Closure181, (Int64, (Vector Word8) )) -> Option33
dispatch_1052 (l0, l1) =
  case l0 of (Variant181_0 l2) -> lam_pure_526 (l2, l1)

lam_parse_from_530 :: (Int64, (Vector Word8) , Parse34) -> Option33
lam_parse_from_530 (l0, l1, l2) =
  let (Parse34_0 l3) = l2 in dispatch_1052 (l3, (l0, l1))

lam_lazy_529 :: (Closure185, (Int64, (Vector Word8) )) -> Option33
lam_lazy_529 (l0, (l1, l2)) =
  lam_parse_from_530 (l1, l2, lam_optional_527 ())

dispatch_1054 :: (Closure184, (Int64, (Vector Word8) )) -> Option33
dispatch_1054 (l0, l1) =
  case l0 of (Variant184_0 l2) -> lam_lazy_529 (l2, l1)

lam_parse_from_534 :: (Int64, (Vector Word8) , Parse36) -> Option33
lam_parse_from_534 (l0, l1, l2) =
  let (Parse36_0 l3) = l2 in dispatch_1054 (l3, (l0, l1))

lam_or_533 :: ((Int64, (Vector Word8) , Parse36), ()) -> Option33
lam_or_533 ((l0, l1, l2), ()) =
  lam_parse_from_534 (l0, l1, l2)

dispatch_1055 :: (Closure725, ()) -> Option33
dispatch_1055 (l0, l1) =
  case l0 of (Variant725_0 l2) -> lam_or_533 (l2, l1)

lam_or_else_535 :: (Option33, Closure725) -> Option33
lam_or_else_535 (l0, l1) =
  case l0 of (Some33_0 l2) -> Some33_0 l2; (None33_1) -> dispatch_1055 (l1, ())

dispatch_1057 :: (Closure179, Option32) -> Parse31
dispatch_1057 (l0, l1) =
  case l0 of (Variant179_0 l2) -> lam_many0_fold_536 (l2, l1)

dispatch_1060 :: (Closure189, ()) -> Parse31
dispatch_1060 (l0, l1) =
  case l0 of (Variant189_0 l2) -> lam_many0_549 (l2, l1)

dispatch_1062 :: (Closure191, (Vector Expr7) ) -> Parse39
dispatch_1062 (l0, l1) =
  case l0 of (Variant191_0 l2) -> lam_parse_expr_553 (l2, l1)

dispatch_1063 :: (Closure187, (Int64, (Vector Word8) )) -> Option26
dispatch_1063 (l0, l1) =
  case l0 of (Variant187_0 l2) -> lam_pure_552 (l2, l1)

lam_parse_from_558 :: (Int64, (Vector Word8) , Parse39) -> Option26
lam_parse_from_558 (l0, l1, l2) =
  let (Parse39_0 l3) = l2 in dispatch_1063 (l3, (l0, l1))

lam_and_then_557 :: (((Vector Word8) , Closure191), (Int64, (Vector Expr7) )) -> Option26
lam_and_then_557 ((l0, l1), (l2, l3)) =
  lam_parse_from_558 (l2, l0, dispatch_1062 (l1, l3))

dispatch_1064 :: (Closure727, (Int64, (Vector Expr7) )) -> Option26
dispatch_1064 (l0, l1) =
  case l0 of (Variant727_0 l2) -> lam_and_then_557 (l2, l1)

lam_and_then_559 :: (Option37, Closure727) -> Option26
lam_and_then_559 (l0, l1) =
  case l0 of (Some37_0 l2) -> dispatch_1064 (l1, l2); (None37_1) -> None26_1

dispatch_1066 :: (Closure729, (Int64, Word8)) -> (Int64, Expr7)
dispatch_1066 (l0, l1) =
  case l0 of (Variant729_0 l2) -> lam_skip_post_564 (l2, l1)

lam_map_565 :: (Option9, Closure729) -> Option26
lam_map_565 (l0, l1) =
  case l0 of (Some9_0 l2) -> Some26_0 (dispatch_1066 (l1, l2)); (None9_1) -> None26_1

lam_skip_post_563 :: (((Vector Word8) , Parse23), (Int64, Expr7)) -> Option26
lam_skip_post_563 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_502 (l2, l0, l1) in lam_map_565 (l4, Variant729_0 l3)

dispatch_1067 :: (Closure728, (Int64, Expr7)) -> Option26
dispatch_1067 (l0, l1) =
  case l0 of (Variant728_0 l2) -> lam_skip_post_563 (l2, l1)

lam_and_then_566 :: (Option26, Closure728) -> Option26
lam_and_then_566 (l0, l1) =
  case l0 of (Some26_0 l2) -> dispatch_1067 (l1, l2); (None26_1) -> None26_1

dispatch_1070 :: (Closure194, (Int64, (Vector Word8) )) -> Option26
dispatch_1070 (l0, l1) =
  case l0 of (Variant194_0 l2) -> lam_map_507 (l2, l1)

lam_parse_from_582 :: (Int64, (Vector Word8) , Parse44) -> Option26
lam_parse_from_582 (l0, l1, l2) =
  let (Parse44_0 l3) = l2 in dispatch_1070 (l3, (l0, l1))

dispatch_1073 :: (Closure198, (Int64, (Vector Word8) )) -> Option27
dispatch_1073 (l0, l1) =
  case l0 of (Variant198_0 l2) -> lam_skip_pre_515 (l2, l1)

lam_parse_from_597 :: (Int64, (Vector Word8) , Parse46) -> Option27
lam_parse_from_597 (l0, l1, l2) =
  let (Parse46_0 l3) = l2 in dispatch_1073 (l3, (l0, l1))

dispatch_1076 :: (Closure193, (Int64, (Vector Word8) )) -> Option26
dispatch_1076 (l0, l1) =
  case l0 of (Variant193_0 l2) -> lam_or_581 (l2, l1)

lam_or_581 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_582 (l2, l3, l0) in lam_or_else_585 (l4, Variant731_0 (l2, l3, l1))

lam_or_else_585 (l0, l1) =
  case l0 of (Some26_0 l2) -> Some26_0 l2; (None26_1) -> dispatch_1072 (l1, ())

dispatch_1072 (l0, l1) =
  case l0 of (Variant731_0 l2) -> lam_or_583 (l2, l1)

lam_or_583 ((l0, l1, l2), ()) =
  lam_parse_from_584 (l0, l1, l2)

lam_parse_from_584 (l0, l1, l2) =
  let (Parse45_0 l3) = l2 in dispatch_1071 (l3, (l0, l1))

dispatch_1071 (l0, l1) =
  case l0 of (Variant196_0 l2) -> lam_and_then_596 (l2, l1)

lam_and_then_596 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_597 (l2, l3, l0) in lam_and_then_600 (l4, Variant732_0 (l3, l1))

lam_and_then_600 (l0, l1) =
  case l0 of (Some27_0 l2) -> dispatch_1075 (l1, l2); (None27_1) -> None26_1

dispatch_1075 (l0, l1) =
  case l0 of (Variant732_0 l2) -> lam_and_then_598 (l2, l1)

lam_and_then_598 ((l0, l1), (l2, l3)) =
  lam_parse_from_599 (l2, l0, lam_parse_expr_519 l3)

lam_parse_from_599 (l0, l1, l2) =
  let (Parse29_0 l3) = l2 in dispatch_1074 (l3, (l0, l1))

dispatch_1074 (l0, l1) =
  case l0 of (Variant175_0 l2) -> lam_skip_pre_568 (l2, l1)

lam_skip_pre_568 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_502 (l2, l3, l0) in lam_and_then_571 (l4, Variant730_0 (l3, l1))

lam_and_then_571 (l0, l1) =
  case l0 of (Some9_0 l2) -> dispatch_1069 (l1, l2); (None9_1) -> None26_1

dispatch_1069 (l0, l1) =
  case l0 of (Variant730_0 l2) -> lam_skip_pre_569 (l2, l1)

lam_skip_pre_569 ((l0, l1), (l2, l_0)) =
  lam_parse_from_570 (l2, l0, l1)

lam_parse_from_570 (l0, l1, l2) =
  let (Parse42_0 l3) = l2 in dispatch_1068 (l3, (l0, l1))

dispatch_1068 (l0, l1) =
  case l0 of (Variant192_0 l2) -> lam_skip_post_561 (l2, l1)

lam_skip_post_561 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_562 (l2, l3, l0) in lam_and_then_566 (l4, Variant728_0 (l3, l1))

lam_parse_from_562 (l0, l1, l2) =
  let (Parse41_0 l3) = l2 in dispatch_1065 (l3, (l0, l1))

dispatch_1065 (l0, l1) =
  case l0 of (Variant190_0 l2) -> lam_and_then_555 (l2, l1)

lam_and_then_555 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_556 (l2, l3, l0) in lam_and_then_559 (l4, Variant727_0 (l3, l1))

lam_parse_from_556 (l0, l1, l2) =
  let (Parse40_0 l3) = l2 in dispatch_1061 (l3, (l0, l1))

dispatch_1061 (l0, l1) =
  case l0 of (Variant188_0 l2) -> lam_lazy_551 (l2, l1)

lam_lazy_551 (l0, (l1, l2)) =
  lam_parse_from_543 (l1, l2, dispatch_1060 (l0, ()))

lam_parse_from_543 (l0, l1, l2) =
  let (Parse31_0 l3) = l2 in dispatch_1058 (l3, (l0, l1))

dispatch_1058 (l0, l1) =
  case l0 of (Variant178_0 l2) -> lam_pure_538 (l2, l1); (Variant178_1 l2) -> lam_and_then_540 (l2, l1)

lam_and_then_540 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_541 (l2, l3, l0) in lam_and_then_544 (l4, Variant726_0 (l3, l1))

lam_and_then_544 (l0, l1) =
  case l0 of (Some33_0 l2) -> dispatch_1059 (l1, l2); (None33_1) -> None37_1

dispatch_1059 (l0, l1) =
  case l0 of (Variant726_0 l2) -> lam_and_then_542 (l2, l1)

lam_and_then_542 ((l0, l1), (l2, l3)) =
  lam_parse_from_543 (l2, l0, dispatch_1057 (l1, l3))

lam_parse_from_541 (l0, l1, l2) =
  let (Parse38_0 l3) = l2 in dispatch_1056 (l3, (l0, l1))

dispatch_1056 (l0, l1) =
  case l0 of (Variant186_0 l2) -> lam_or_531 (l2, l1)

lam_or_531 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_532 (l2, l3, l0) in lam_or_else_535 (l4, Variant725_0 (l2, l3, l1))

lam_parse_from_532 (l0, l1, l2) =
  let (Parse35_0 l3) = l2 in dispatch_1053 (l3, (l0, l1))

dispatch_1053 (l0, l1) =
  case l0 of (Variant182_0 l2) -> lam_map_522 (l2, l1)

lam_map_522 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_523 (l2, l3, l0) in lam_map_525 (l4, Variant724_0 l1)

lam_parse_from_523 (l0, l1, l2) =
  let (Parse30_0 l3) = l2 in dispatch_1050 (l3, (l0, l1))

dispatch_1050 (l0, l1) =
  case l0 of (Variant176_0 l2) -> lam_lazy_604 (l2, l1)

lam_lazy_604 (l0, (l1, l2)) =
  lam_parse_from_605 (l1, l2, lam_parse_expr_579 ())

lam_parse_from_605 (l0, l1, l2) =
  let (Parse43_0 l3) = l2 in dispatch_1076 (l3, (l0, l1))

dispatch_1077 :: (Closure201, Expr7) -> Parse48
dispatch_1077 (l0, l1) =
  case l0 of (Variant201_0 l2) -> lam_parse_command_607 (l2, l1)

dispatch_1078 :: (Closure199, (Int64, (Vector Word8) )) -> Option47
dispatch_1078 (l0, l1) =
  case l0 of (Variant199_0 l2) -> lam_pure_606 (l2, l1)

lam_parse_from_611 :: (Int64, (Vector Word8) , Parse48) -> Option47
lam_parse_from_611 (l0, l1, l2) =
  let (Parse48_0 l3) = l2 in dispatch_1078 (l3, (l0, l1))

lam_and_then_610 :: (((Vector Word8) , Closure201), (Int64, Expr7)) -> Option47
lam_and_then_610 ((l0, l1), (l2, l3)) =
  lam_parse_from_611 (l2, l0, dispatch_1077 (l1, l3))

dispatch_1079 :: (Closure733, (Int64, Expr7)) -> Option47
dispatch_1079 (l0, l1) =
  case l0 of (Variant733_0 l2) -> lam_and_then_610 (l2, l1)

lam_and_then_612 :: (Option26, Closure733) -> Option47
lam_and_then_612 (l0, l1) =
  case l0 of (Some26_0 l2) -> dispatch_1079 (l1, l2); (None26_1) -> None47_1

lam_and_then_609 :: ((Parse30, Closure201), (Int64, (Vector Word8) )) -> Option47
lam_and_then_609 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_523 (l2, l3, l0) in lam_and_then_612 (l4, Variant733_0 (l3, l1))

dispatch_1080 :: (Closure200, (Int64, (Vector Word8) )) -> Option47
dispatch_1080 (l0, l1) =
  case l0 of (Variant200_0 l2) -> lam_and_then_609 (l2, l1)

lam_parse_from_615 :: (Int64, (Vector Word8) , Parse49) -> Option47
lam_parse_from_615 (l0, l1, l2) =
  let (Parse49_0 l3) = l2 in dispatch_1080 (l3, (l0, l1))

lam_skip_pre_614 :: (((Vector Word8) , Parse49), (Int64, Word8)) -> Option47
lam_skip_pre_614 ((l0, l1), (l2, l_0)) =
  lam_parse_from_615 (l2, l0, l1)

dispatch_1081 :: (Closure734, (Int64, Word8)) -> Option47
dispatch_1081 (l0, l1) =
  case l0 of (Variant734_0 l2) -> lam_skip_pre_614 (l2, l1)

lam_and_then_616 :: (Option9, Closure734) -> Option47
lam_and_then_616 (l0, l1) =
  case l0 of (Some9_0 l2) -> dispatch_1081 (l1, l2); (None9_1) -> None47_1

lam_skip_pre_613 :: ((Parse23, Parse49), (Int64, (Vector Word8) )) -> Option47
lam_skip_pre_613 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_502 (l2, l3, l0) in lam_and_then_616 (l4, Variant734_0 (l3, l1))

dispatch_1082 :: (Closure202, (Int64, (Vector Word8) )) -> Option47
dispatch_1082 (l0, l1) =
  case l0 of (Variant202_0 l2) -> lam_skip_pre_613 (l2, l1)

lam_parse_from_622 :: (Int64, (Vector Word8) , Parse50) -> Option47
lam_parse_from_622 (l0, l1, l2) =
  let (Parse50_0 l3) = l2 in dispatch_1082 (l3, (l0, l1))

lam_and_then_621 :: (((Vector Word8) , Closure204), (Int64, VarId6)) -> Option47
lam_and_then_621 ((l0, l1), (l2, l3)) =
  lam_parse_from_622 (l2, l0, lam_parse_command_617 l3)

dispatch_1083 :: (Closure735, (Int64, VarId6)) -> Option47
dispatch_1083 (l0, l1) =
  case l0 of (Variant735_0 l2) -> lam_and_then_621 (l2, l1)

lam_and_then_623 :: (Option22, Closure735) -> Option47
lam_and_then_623 (l0, l1) =
  case l0 of (Some22_0 l2) -> dispatch_1083 (l1, l2); (None22_1) -> None47_1

lam_and_then_620 :: ((Parse25, Closure204), (Int64, (Vector Word8) )) -> Option47
lam_and_then_620 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_508 (l2, l3, l0) in lam_and_then_623 (l4, Variant735_0 (l3, l1))

dispatch_1084 :: (Closure203, (Int64, (Vector Word8) )) -> Option47
dispatch_1084 (l0, l1) =
  case l0 of (Variant203_0 l2) -> lam_and_then_620 (l2, l1)

lam_parse_from_626 :: (Int64, (Vector Word8) , Parse52) -> Option47
lam_parse_from_626 (l0, l1, l2) =
  let (Parse52_0 l3) = l2 in dispatch_1084 (l3, (l0, l1))

lam_skip_pre_641 :: (((Vector Word8) , Parse52), (Int64, Word8)) -> Option47
lam_skip_pre_641 ((l0, l1), (l2, l_0)) =
  lam_parse_from_626 (l2, l0, l1)

dispatch_1085 :: (Closure736, (Int64, (VarId6, Expr7))) -> (Int64, Option51)
dispatch_1085 (l0, l1) =
  case l0 of (Variant736_0 l2) -> lam_map_627 (l2, l1)

lam_map_628 :: (Option47, Closure736) -> Option53
lam_map_628 (l0, l1) =
  case l0 of (Some47_0 l2) -> Some53_0 (dispatch_1085 (l1, l2)); (None47_1) -> None53_1

lam_map_625 :: ((Parse52, Closure207), (Int64, (Vector Word8) )) -> Option53
lam_map_625 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_626 (l2, l3, l0) in lam_map_628 (l4, Variant736_0 l1)

dispatch_1086 :: (Closure205, (Int64, (Vector Word8) )) -> Option53
dispatch_1086 (l0, l1) =
  case l0 of (Variant205_0 l2) -> lam_pure_629 (l2, l1)

lam_parse_from_633 :: (Int64, (Vector Word8) , Parse54) -> Option53
lam_parse_from_633 (l0, l1, l2) =
  let (Parse54_0 l3) = l2 in dispatch_1086 (l3, (l0, l1))

lam_lazy_632 :: (Closure209, (Int64, (Vector Word8) )) -> Option53
lam_lazy_632 (l0, (l1, l2)) =
  lam_parse_from_633 (l1, l2, lam_optional_630 ())

dispatch_1087 :: (Closure206, (Int64, (Vector Word8) )) -> Option53
dispatch_1087 (l0, l1) =
  case l0 of (Variant206_0 l2) -> lam_map_625 (l2, l1)

lam_parse_from_635 :: (Int64, (Vector Word8) , Parse55) -> Option53
lam_parse_from_635 (l0, l1, l2) =
  let (Parse55_0 l3) = l2 in dispatch_1087 (l3, (l0, l1))

dispatch_1088 :: (Closure208, (Int64, (Vector Word8) )) -> Option53
dispatch_1088 (l0, l1) =
  case l0 of (Variant208_0 l2) -> lam_lazy_632 (l2, l1)

lam_parse_from_637 :: (Int64, (Vector Word8) , Parse56) -> Option53
lam_parse_from_637 (l0, l1, l2) =
  let (Parse56_0 l3) = l2 in dispatch_1088 (l3, (l0, l1))

lam_or_636 :: ((Int64, (Vector Word8) , Parse56), ()) -> Option53
lam_or_636 ((l0, l1, l2), ()) =
  lam_parse_from_637 (l0, l1, l2)

dispatch_1089 :: (Closure737, ()) -> Option53
dispatch_1089 (l0, l1) =
  case l0 of (Variant737_0 l2) -> lam_or_636 (l2, l1)

lam_or_else_638 :: (Option53, Closure737) -> Option53
lam_or_else_638 (l0, l1) =
  case l0 of (Some53_0 l2) -> Some53_0 l2; (None53_1) -> dispatch_1089 (l1, ())

lam_or_634 :: ((Parse55, Parse56), (Int64, (Vector Word8) )) -> Option53
lam_or_634 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_635 (l2, l3, l0) in lam_or_else_638 (l4, Variant737_0 (l2, l3, l1))

dispatch_1090 :: (Closure738, (Int64, Word8)) -> Option47
dispatch_1090 (l0, l1) =
  case l0 of (Variant738_0 l2) -> lam_skip_pre_641 (l2, l1)

lam_and_then_642 :: (Option9, Closure738) -> Option47
lam_and_then_642 (l0, l1) =
  case l0 of (Some9_0 l2) -> dispatch_1090 (l1, l2); (None9_1) -> None47_1

lam_skip_pre_640 :: ((Parse23, Parse52), (Int64, (Vector Word8) )) -> Option47
lam_skip_pre_640 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_502 (l2, l3, l0) in lam_and_then_642 (l4, Variant738_0 (l3, l1))

dispatch_1091 :: (Closure210, (Int64, (Vector Word8) )) -> Option47
dispatch_1091 (l0, l1) =
  case l0 of (Variant210_0 l2) -> lam_skip_pre_640 (l2, l1)

lam_parse_from_645 :: (Int64, (Vector Word8) , Parse58) -> Option47
lam_parse_from_645 (l0, l1, l2) =
  let (Parse58_0 l3) = l2 in dispatch_1091 (l3, (l0, l1))

lam_map_644 :: ((Parse58, Closure207), (Int64, (Vector Word8) )) -> Option53
lam_map_644 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_645 (l2, l3, l0) in lam_map_628 (l4, Variant736_0 l1)

dispatch_1092 :: (Closure214, (Int64, (Vector Word8) )) -> Option53
dispatch_1092 (l0, l1) =
  case l0 of (Variant214_0 l2) -> lam_map_644 (l2, l1)

lam_parse_from_647 :: (Int64, (Vector Word8) , Parse60) -> Option53
lam_parse_from_647 (l0, l1, l2) =
  let (Parse60_0 l3) = l2 in dispatch_1092 (l3, (l0, l1))

lam_or_646 :: ((Parse60, Parse56), (Int64, (Vector Word8) )) -> Option53
lam_or_646 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_647 (l2, l3, l0) in lam_or_else_638 (l4, Variant737_0 (l2, l3, l1))

dispatch_1093 :: (Closure215, (Int64, (Vector Word8) )) -> Option53
dispatch_1093 (l0, l1) =
  case l0 of (Variant215_0 l2) -> lam_or_646 (l2, l1)

lam_parse_from_652 :: (Int64, (Vector Word8) , Parse61) -> Option53
lam_parse_from_652 (l0, l1, l2) =
  let (Parse61_0 l3) = l2 in dispatch_1093 (l3, (l0, l1))

dispatch_1094 :: (Closure212, Option51) -> Parse59
dispatch_1094 (l0, l1) =
  case l0 of (Variant212_0 l2) -> lam_many0_fold_648 (l2, l1)

dispatch_1096 :: (Closure739, (Int64, Option51)) -> Option57
dispatch_1096 (l0, l1) =
  case l0 of (Variant739_0 l2) -> lam_and_then_653 (l2, l1)

lam_and_then_653 ((l0, l1), (l2, l3)) =
  lam_parse_from_654 (l2, l0, dispatch_1094 (l1, l3))

lam_parse_from_654 (l0, l1, l2) =
  let (Parse59_0 l3) = l2 in dispatch_1095 (l3, (l0, l1))

dispatch_1095 (l0, l1) =
  case l0 of (Variant211_0 l2) -> lam_pure_639 (l2, l1); (Variant211_1 l2) -> lam_and_then_651 (l2, l1)

lam_and_then_651 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_652 (l2, l3, l0) in lam_and_then_655 (l4, Variant739_0 (l3, l1))

lam_and_then_655 (l0, l1) =
  case l0 of (Some53_0 l2) -> dispatch_1096 (l1, l2); (None53_1) -> None57_1

dispatch_1097 :: (Closure216, (Int64, (Vector Word8) )) -> Option53
dispatch_1097 (l0, l1) =
  case l0 of (Variant216_0 l2) -> lam_or_634 (l2, l1)

lam_parse_from_664 :: (Int64, (Vector Word8) , Parse62) -> Option53
lam_parse_from_664 (l0, l1, l2) =
  let (Parse62_0 l3) = l2 in dispatch_1097 (l3, (l0, l1))

dispatch_1098 :: (Closure218, Option51) -> Parse59
dispatch_1098 (l0, l1) =
  case l0 of (Variant218_0 l2) -> lam_sep0_fold_660 (l2, l1)

lam_and_then_665 :: (((Vector Word8) , Closure218), (Int64, Option51)) -> Option57
lam_and_then_665 ((l0, l1), (l2, l3)) =
  lam_parse_from_654 (l2, l0, dispatch_1098 (l1, l3))

dispatch_1099 :: (Closure740, (Int64, Option51)) -> Option57
dispatch_1099 (l0, l1) =
  case l0 of (Variant740_0 l2) -> lam_and_then_665 (l2, l1)

lam_and_then_666 :: (Option53, Closure740) -> Option57
lam_and_then_666 (l0, l1) =
  case l0 of (Some53_0 l2) -> dispatch_1099 (l1, l2); (None53_1) -> None57_1

lam_and_then_663 :: ((Parse62, Closure218), (Int64, (Vector Word8) )) -> Option57
lam_and_then_663 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_664 (l2, l3, l0) in lam_and_then_666 (l4, Variant740_0 (l3, l1))

dispatch_1100 :: (Closure221, ()) -> Parse63
dispatch_1100 (l0, l1) =
  case l0 of (Variant221_0 l2) -> lam_sep0_667 (l2, l1)

dispatch_1101 :: (Closure217, (Int64, (Vector Word8) )) -> Option57
dispatch_1101 (l0, l1) =
  case l0 of (Variant217_0 l2) -> lam_and_then_663 (l2, l1)

lam_parse_from_674 :: (Int64, (Vector Word8) , Parse63) -> Option57
lam_parse_from_674 (l0, l1, l2) =
  let (Parse63_0 l3) = l2 in dispatch_1101 (l3, (l0, l1))

lam_lazy_673 :: (Closure221, (Int64, (Vector Word8) )) -> Option57
lam_lazy_673 (l0, (l1, l2)) =
  lam_parse_from_674 (l1, l2, dispatch_1100 (l0, ()))

dispatch_1102 :: (Closure220, (Int64, (Vector Word8) )) -> Option57
dispatch_1102 (l0, l1) =
  case l0 of (Variant220_0 l2) -> lam_lazy_673 (l2, l1)

lam_parse_from_679 :: (Int64, (Vector Word8) , Parse66) -> Option57
lam_parse_from_679 (l0, l1, l2) =
  let (Parse66_0 l3) = l2 in dispatch_1102 (l3, (l0, l1))

dispatch_1103 :: (Closure223, (Vector ((VarId6, Expr7))) ) -> Parse65
dispatch_1103 (l0, l1) =
  case l0 of (Variant223_0 l2) -> lam_parse_problem_676 (l2, l1)

dispatch_1104 :: (Closure219, (Int64, (Vector Word8) )) -> Option64
dispatch_1104 (l0, l1) =
  case l0 of (Variant219_0 l2) -> lam_pure_675 (l2, l1)

lam_parse_from_681 :: (Int64, (Vector Word8) , Parse65) -> Option64
lam_parse_from_681 (l0, l1, l2) =
  let (Parse65_0 l3) = l2 in dispatch_1104 (l3, (l0, l1))

lam_and_then_680 :: (((Vector Word8) , Closure223), (Int64, (Vector ((VarId6, Expr7))) )) -> Option64
lam_and_then_680 ((l0, l1), (l2, l3)) =
  lam_parse_from_681 (l2, l0, dispatch_1103 (l1, l3))

dispatch_1105 :: (Closure741, (Int64, (Vector ((VarId6, Expr7))) )) -> Option64
dispatch_1105 (l0, l1) =
  case l0 of (Variant741_0 l2) -> lam_and_then_680 (l2, l1)

lam_and_then_682 :: (Option57, Closure741) -> Option64
lam_and_then_682 (l0, l1) =
  case l0 of (Some57_0 l2) -> dispatch_1105 (l1, l2); (None57_1) -> None64_1

lam_and_then_678 :: ((Parse66, Closure223), (Int64, (Vector Word8) )) -> Option64
lam_and_then_678 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_679 (l2, l3, l0) in lam_and_then_682 (l4, Variant741_0 (l3, l1))

dispatch_1106 :: (Closure222, (Int64, (Vector Word8) )) -> Option64
dispatch_1106 (l0, l1) =
  case l0 of (Variant222_0 l2) -> lam_and_then_678 (l2, l1)

lam_parse_from_685 :: (Int64, (Vector Word8) , Parse67) -> Option64
lam_parse_from_685 (l0, l1, l2) =
  let (Parse67_0 l3) = l2 in dispatch_1106 (l3, (l0, l1))

lam_skip_pre_684 :: (((Vector Word8) , Parse67), (Int64, Word8)) -> Option64
lam_skip_pre_684 ((l0, l1), (l2, l_0)) =
  lam_parse_from_685 (l2, l0, l1)

dispatch_1107 :: (Closure742, (Int64, Word8)) -> Option64
dispatch_1107 (l0, l1) =
  case l0 of (Variant742_0 l2) -> lam_skip_pre_684 (l2, l1)

lam_and_then_686 :: (Option9, Closure742) -> Option64
lam_and_then_686 (l0, l1) =
  case l0 of (Some9_0 l2) -> dispatch_1107 (l1, l2); (None9_1) -> None64_1

lam_skip_pre_683 :: ((Parse23, Parse67), (Int64, (Vector Word8) )) -> Option64
lam_skip_pre_683 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_502 (l2, l3, l0) in lam_and_then_686 (l4, Variant742_0 (l3, l1))

dispatch_1108 :: (Closure226, VarId6) -> Parse68
dispatch_1108 (l0, l1) =
  case l0 of (Variant226_0 l2) -> lam_parse_problem_687 (l2, l1)

dispatch_1109 :: (Closure224, (Int64, (Vector Word8) )) -> Option64
dispatch_1109 (l0, l1) =
  case l0 of (Variant224_0 l2) -> lam_skip_pre_683 (l2, l1)

lam_parse_from_695 :: (Int64, (Vector Word8) , Parse68) -> Option64
lam_parse_from_695 (l0, l1, l2) =
  let (Parse68_0 l3) = l2 in dispatch_1109 (l3, (l0, l1))

lam_and_then_694 :: (((Vector Word8) , Closure226), (Int64, VarId6)) -> Option64
lam_and_then_694 ((l0, l1), (l2, l3)) =
  lam_parse_from_695 (l2, l0, dispatch_1108 (l1, l3))

dispatch_1110 :: (Closure743, (Int64, VarId6)) -> Option64
dispatch_1110 (l0, l1) =
  case l0 of (Variant743_0 l2) -> lam_and_then_694 (l2, l1)

lam_and_then_696 :: (Option22, Closure743) -> Option64
lam_and_then_696 (l0, l1) =
  case l0 of (Some22_0 l2) -> dispatch_1110 (l1, l2); (None22_1) -> None64_1

lam_and_then_693 :: ((Parse25, Closure226), (Int64, (Vector Word8) )) -> Option64
lam_and_then_693 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_508 (l2, l3, l0) in lam_and_then_696 (l4, Variant743_0 (l3, l1))

dispatch_1111 :: (Closure225, (Int64, (Vector Word8) )) -> Option64
dispatch_1111 (l0, l1) =
  case l0 of (Variant225_0 l2) -> lam_and_then_693 (l2, l1)

lam_parse_from_699 :: (Int64, (Vector Word8) , Parse69) -> Option64
lam_parse_from_699 (l0, l1, l2) =
  let (Parse69_0 l3) = l2 in dispatch_1111 (l3, (l0, l1))

lam_skip_pre_698 :: (((Vector Word8) , Parse69), (Int64, Word8)) -> Option64
lam_skip_pre_698 ((l0, l1), (l2, l_0)) =
  lam_parse_from_699 (l2, l0, l1)

dispatch_1112 :: (Closure744, (Int64, Word8)) -> Option64
dispatch_1112 (l0, l1) =
  case l0 of (Variant744_0 l2) -> lam_skip_pre_698 (l2, l1)

lam_and_then_700 :: (Option9, Closure744) -> Option64
lam_and_then_700 (l0, l1) =
  case l0 of (Some9_0 l2) -> dispatch_1112 (l1, l2); (None9_1) -> None64_1

lam_skip_pre_697 :: ((Parse23, Parse69), (Int64, (Vector Word8) )) -> Option64
lam_skip_pre_697 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_502 (l2, l3, l0) in lam_and_then_700 (l4, Variant744_0 (l3, l1))

dispatch_1113 :: (Closure227, (Int64, (Vector Word8) )) -> Option64
dispatch_1113 (l0, l1) =
  case l0 of (Variant227_0 l2) -> lam_skip_pre_697 (l2, l1)

lam_parse_from_706 :: (Int64, (Vector Word8) , Parse70) -> Option64
lam_parse_from_706 (l0, l1, l2) =
  let (Parse70_0 l3) = l2 in dispatch_1113 (l3, (l0, l1))

lam_and_then_705 :: (((Vector Word8) , Closure229), (Int64, Int64)) -> Option64
lam_and_then_705 ((l0, l1), (l2, l3)) =
  lam_parse_from_706 (l2, l0, lam_parse_problem_701 l3)

dispatch_1114 :: (Closure745, (Int64, Int64)) -> Option64
dispatch_1114 (l0, l1) =
  case l0 of (Variant745_0 l2) -> lam_and_then_705 (l2, l1)

lam_and_then_707 :: (Option13, Closure745) -> Option64
lam_and_then_707 (l0, l1) =
  case l0 of (Some13_0 l2) -> dispatch_1114 (l1, l2); (None13_1) -> None64_1

lam_and_then_704 :: ((Parse21, Closure229), (Int64, (Vector Word8) )) -> Option64
lam_and_then_704 ((l0, l1), (l2, l3)) =
  let l4 = lam_parse_from_498 (l2, l3, l0) in lam_and_then_707 (l4, Variant745_0 (l3, l1))

dispatch_1115 :: (Closure228, (Int64, (Vector Word8) )) -> Option64
dispatch_1115 (l0, l1) =
  case l0 of (Variant228_0 l2) -> lam_and_then_704 (l2, l1)

lam_parse_from_710 :: (Int64, (Vector Word8) , Parse71) -> Option64
lam_parse_from_710 (l0, l1, l2) =
  let (Parse71_0 l3) = l2 in dispatch_1115 (l3, (l0, l1))

lam_parse_prefix_709 :: ((Vector Word8) , Parse71) -> Option64
lam_parse_prefix_709 (l0, l1) =
  lam_parse_from_710 (0, l0, l1)

dispatch_1116 :: (Closure746, (Int64, Problem5)) -> Option72
dispatch_1116 (l0, l1) =
  case l0 of (Variant746_0 l2) -> lam_parse_all_711 (l2, l1)

lam_and_then_712 :: (Option64, Closure746) -> Option72
lam_and_then_712 (l0, l1) =
  case l0 of (Some64_0 l2) -> dispatch_1116 (l1, l2); (None64_1) -> None72_1

lam_parse_all_708 :: ((Vector Word8) , Parse71) -> Option72
lam_parse_all_708 (l0, l1) =
  let l2 = (let l2 = l0 in lam_parse_prefix_709 (l2, l1)) in lam_and_then_712 (l2, Variant746_0 l0)

lam_read_problems_rec_439 :: (Vector Problem5)  -> (Vector Problem5) 
lam_read_problems_rec_439 l0 =
  let l1 = input () in (case uncurry (==) (intrinsicLen l1, 0) of True -> l0; False -> (case lam_parse_all_708 (l1, parse_problem_194 ()) of (Some72_0 l2) -> (let l3 = (let l3 = l0 in intrinsicPush l3 l2) in lam_read_problems_rec_439 l3); (None72_1) -> panic ((V.fromList [112, 97, 114, 115, 101, 32, 101, 114, 114, 111, 114]))))

lam_wrapped_read_problems_rec_438 :: (Vector Problem5)  -> (Vector Problem5) 
lam_wrapped_read_problems_rec_438 l0 =
  lam_read_problems_rec_439 l0

lam_read_problems_437 :: () -> (Vector Problem5) 
lam_read_problems_437 () =
  lam_wrapped_read_problems_rec_438 ((V.fromList []))

dispatch_1117 :: (Closure232, Int64) -> Problem5
dispatch_1117 (l0, l1) =
  case l0 of (Variant232_0 l2) -> lam_items_717 (l2, l1)

dispatch_1118 :: (Closure230, ()) -> Option75
dispatch_1118 (l0, l1) =
  case l0 of (Variant230_0 l2) -> lam_range_716 (l2, l1)

lam_next_720 :: Iter74 -> Option75
lam_next_720 l0 =
  let (Iter74_0 l1) = l0 in dispatch_1118 (l1, ())

lam_map_719 :: ((Iter74, Closure232), ()) -> Option77
lam_map_719 ((l0, l1), ()) =
  case lam_next_720 l0 of (Some75_0 (l2, l3)) -> Some77_0 (dispatch_1117 (l1, l2), lam_map_718 (l3, l1)); (None75_1) -> None77_1

dispatch_1119 :: (Closure235, Int64) -> (VarId6, Expr7)
dispatch_1119 (l0, l1) =
  case l0 of (Variant235_0 l2) -> lam_items_725 (l2, l1)

dispatch_1120 :: (Closure233, ()) -> Option79
dispatch_1120 (l0, l1) =
  case l0 of (Variant233_0 l2) -> lam_range_724 (l2, l1)

lam_next_728 :: Iter78 -> Option79
lam_next_728 l0 =
  let (Iter78_0 l1) = l0 in dispatch_1120 (l1, ())

lam_map_727 :: ((Iter78, Closure235), ()) -> Option81
lam_map_727 ((l0, l1), ()) =
  case lam_next_728 l0 of (Some79_0 (l2, l3)) -> Some81_0 (dispatch_1119 (l1, l2), lam_map_726 (l3, l1)); (None79_1) -> None81_1

dispatch_1121 :: (Closure243, VarId6) -> State84
dispatch_1121 (l0, l1) =
  case l0 of (Variant243_0 l2) -> lam_mk_expr_730 (l2, l1)

dispatch_1123 :: (Closure236, Terms82) -> (Terms82, (Vector VarId6) )
dispatch_1123 (l0, l1) =
  case l0 of (Variant236_0 l2) -> lam_pure_729 (l2, l1)

lam_run_734 :: (State84, Terms82) -> (Terms82, (Vector VarId6) )
lam_run_734 (l0, l1) =
  let (State84_0 l2) = l0 in dispatch_1123 (l2, l1)

dispatch_1124 :: (Closure241, Int64) -> Expr7
dispatch_1124 (l0, l1) =
  case l0 of (Variant241_0 l2) -> lam_items_740 (l2, l1)

dispatch_1125 :: (Closure239, ()) -> Option87
dispatch_1125 (l0, l1) =
  case l0 of (Variant239_0 l2) -> lam_range_739 (l2, l1)

lam_next_743 :: Iter86 -> Option87
lam_next_743 l0 =
  let (Iter86_0 l1) = l0 in dispatch_1125 (l1, ())

lam_map_742 :: ((Iter86, Closure241), ()) -> Option89
lam_map_742 ((l0, l1), ()) =
  case lam_next_743 l0 of (Some87_0 (l2, l3)) -> Some89_0 (dispatch_1124 (l1, l2), lam_map_741 (l3, l1)); (None87_1) -> None89_1

lam_map_1000 :: ((Iter86, Closure241), ()) -> Option145
lam_map_1000 ((l0, l1), ()) =
  case lam_next_743 l0 of (Some87_0 (l2, l3)) -> Some145_0 (dispatch_1124 (l1, l2), lam_map_999 (l3, l1)); (None87_1) -> None145_1

dispatch_1126 :: (Closure240, ()) -> Option89
dispatch_1126 (l0, l1) =
  case l0 of (Variant240_0 l2) -> lam_map_742 (l2, l1)

lam_next_747 :: Iter88 -> Option89
lam_next_747 l0 =
  let (Iter88_0 l1) = l0 in dispatch_1126 (l1, ())

lam_for_each_accum_746 :: (Iter88, (Vector VarId6) , Closure246) -> State91
lam_for_each_accum_746 (l0, l1, l2) =
  case lam_next_747 l0 of (Some89_0 (l3, l4)) -> lam_bind_749 (lam_mk_expr_744 (l1, l3), Variant245_0 (l4, l2)); (None89_1) -> lam_pure_753 l1

lam_mk_expr_744 (l0, l1) =
  lam_bind_745 (lam_mk_expr_735 l1, Variant243_0 l0)

lam_mk_expr_735 l0 =
  case l0 of (EVar7_0 l1) -> lam_pure_736 l1; (EHead7_1 (l1, l2)) -> lam_bind_758 (lam_wrapped_for_each_accum_761 (lam_items_762 l2, (V.fromList []), Variant246_0 ()), Variant238_0 l1)

lam_wrapped_for_each_accum_761 l0 =
  lam_for_each_accum_746 l0

lam_for_each_accum_748 :: ((Iter88, Closure246), (Vector VarId6) ) -> State91
lam_for_each_accum_748 ((l0, l1), l2) =
  lam_for_each_accum_746 (l0, l2, l1)

lam_wrapped_mk_expr_892 :: Expr7 -> State85
lam_wrapped_mk_expr_892 l0 =
  lam_mk_expr_735 l0

lam_solve_890 :: (VarId6, Expr7) -> State122
lam_solve_890 (l0, l1) =
  lam_bind_891 (lam_wrapped_mk_expr_892 l1, Variant294_0 l0)

dispatch_1127 :: (Closure245, (Vector VarId6) ) -> State91
dispatch_1127 (l0, l1) =
  case l0 of (Variant245_0 l2) -> lam_for_each_accum_748 (l2, l1)

dispatch_1130 :: (Closure238, (Vector VarId6) ) -> State92
dispatch_1130 (l0, l1) =
  case l0 of (Variant238_0 l2) -> lam_mk_expr_755 (l2, l1)

dispatch_1131 :: (Closure247, Terms82) -> (Terms82, VarId6)
dispatch_1131 (l0, l1) =
  case l0 of (Variant247_0 l2) -> lam_new_term_754 (l2, l1)

lam_run_760 :: (State92, Terms82) -> (Terms82, VarId6)
lam_run_760 (l0, l1) =
  let (State92_0 l2) = l0 in dispatch_1131 (l2, l1)

lam_bind_759 :: ((State91, Closure238), Terms82) -> (Terms82, VarId6)
lam_bind_759 ((l0, l1), l2) =
  let (l3, l4) = lam_run_752 (l0, l2) in lam_run_760 (dispatch_1130 (l1, l4), l3)

lam_run_752 (l0, l1) =
  let (State91_0 l2) = l0 in dispatch_1129 (l2, l1)

dispatch_1129 (l0, l1) =
  case l0 of (Variant244_0 l2) -> lam_pure_729 (l2, l1); (Variant244_1 l2) -> lam_bind_750 (l2, l1)

lam_bind_750 ((l0, l1), l2) =
  let (l3, l4) = lam_run_751 (l0, l2) in lam_run_752 (dispatch_1127 (l1, l4), l3)

lam_run_751 (l0, l1) =
  let (State90_0 l2) = l0 in dispatch_1128 (l2, l1)

dispatch_1128 (l0, l1) =
  case l0 of (Variant242_0 l2) -> lam_bind_732 (l2, l1)

lam_bind_732 ((l0, l1), l2) =
  let (l3, l4) = lam_run_733 (l0, l2) in lam_run_734 (dispatch_1121 (l1, l4), l3)

lam_run_733 (l0, l1) =
  let (State85_0 l2) = l0 in dispatch_1122 (l2, l1)

dispatch_1122 (l0, l1) =
  case l0 of (Variant237_0 l2) -> lam_pure_737 (l2, l1); (Variant237_1 l2) -> lam_bind_759 (l2, l1)

dispatch_1132 :: (Closure250, Int64) -> VarId6
dispatch_1132 (l0, l1) =
  case l0 of (Variant250_0 l2) -> lam_items_768 (l2, l1)

dispatch_1133 :: (Closure248, ()) -> Option94
dispatch_1133 (l0, l1) =
  case l0 of (Variant248_0 l2) -> lam_range_767 (l2, l1)

lam_next_771 :: Iter93 -> Option94
lam_next_771 l0 =
  let (Iter93_0 l1) = l0 in dispatch_1133 (l1, ())

lam_map_770 :: ((Iter93, Closure250), ()) -> Option96
lam_map_770 ((l0, l1), ()) =
  case lam_next_771 l0 of (Some94_0 (l2, l3)) -> Some96_0 (dispatch_1132 (l1, l2), lam_map_769 (l3, l1)); (None94_1) -> None96_1

lam_map_773 :: ((Iter93, Closure250), ()) -> Option98
lam_map_773 ((l0, l1), ()) =
  case lam_next_771 l0 of (Some94_0 (l2, l3)) -> Some98_0 (dispatch_1132 (l1, l2), lam_map_772 (l3, l1)); (None94_1) -> None98_1

lam_map_909 :: ((Iter93, Closure250), ()) -> Option127
lam_map_909 ((l0, l1), ()) =
  case lam_next_771 l0 of (Some94_0 (l2, l3)) -> Some127_0 (dispatch_1132 (l1, l2), lam_map_908 (l3, l1)); (None94_1) -> None127_1

dispatch_1134 :: (Closure249, ()) -> Option96
dispatch_1134 (l0, l1) =
  case l0 of (Variant249_0 l2) -> lam_map_770 (l2, l1)

lam_next_776 :: Iter95 -> Option96
lam_next_776 l0 =
  let (Iter95_0 l1) = l0 in dispatch_1134 (l1, ())

dispatch_1135 :: (Closure251, ()) -> Option98
dispatch_1135 (l0, l1) =
  case l0 of (Variant251_0 l2) -> lam_map_773 (l2, l1)

lam_next_777 :: Iter97 -> Option98
lam_next_777 l0 =
  let (Iter97_0 l1) = l0 in dispatch_1135 (l1, ())

lam_zip_775 :: ((Iter95, Iter97), ()) -> Option100
lam_zip_775 ((l0, l1), ()) =
  case (lam_next_776 l0, lam_next_777 l1) of ((Some96_0 (l2, l3)), (Some98_0 (l4, l5))) -> Some100_0 ((l2, l4), lam_zip_774 (l3, l5)); l_0 -> None100_1

dispatch_1136 :: (Closure252, ()) -> Option100
dispatch_1136 (l0, l1) =
  case l0 of (Variant252_0 l2) -> lam_zip_775 (l2, l1)

lam_next_779 :: Iter99 -> Option100
lam_next_779 l0 =
  let (Iter99_0 l1) = l0 in dispatch_1136 (l1, ())

lam_for_each_checked_778 :: (Iter99, Closure255) -> State101
lam_for_each_checked_778 (l0, l1) =
  case lam_next_779 l0 of (Some100_0 (l2, l3)) -> lam_bind_782 (lam_unify_816 l2, Variant254_0 (l3, l1)); (None100_1) -> lam_pure_781 True

lam_for_each_checked_780 :: ((Iter99, Closure255), Bool) -> State101
lam_for_each_checked_780 ((l0, l1), l2) =
  case l2 of True -> lam_for_each_checked_778 (l0, l1); False -> lam_pure_781 False

lam_wrapped_for_each_checked_815 :: (Iter99, Closure255) -> State101
lam_wrapped_for_each_checked_815 l0 =
  lam_for_each_checked_778 l0

lam_unify_805 :: ((HeadId8, HeadId8, (Vector VarId6) , (Vector VarId6) , VarId6, VarId6), ()) -> State107
lam_unify_805 ((l0, l1, l2, l3, l4, l5), ()) =
  case not (case lam_head_id_eq_806 (l0, l1) of False -> False; True -> uncurry (==) (intrinsicLen l2, intrinsicLen l3)) of True -> lam_pure_807 False; False -> (let l6 = lam_wrapped_zip_808 (lam_items_809 l2, lam_items_812 l3) in lam_bind_814 (lam_wrapped_for_each_checked_815 (l6, unify_272 ()), Variant266_0 (l4, l5, l1, l3)))

dispatch_1137 :: (Closure254, Bool) -> State101
dispatch_1137 (l0, l1) =
  case l0 of (Variant254_0 l2) -> lam_for_each_checked_780 (l2, l1)

dispatch_1140 :: (Closure749, ((Vector Term83) , Int64)) -> (Term83, Closure748)
dispatch_1140 (l0, l1) =
  case l0 of (Variant749_0) -> (let (l2, l3) = l1 in (let (l4, l5) = intrinsicExtract l2 l3 in (l4, Variant748_0 l5)))

dispatch_1141 :: (Closure748, Term83) -> (Vector Term83) 
dispatch_1141 (l0, l1) =
  case l0 of (Variant748_0 l2) -> intrinsicReplace l2 l1

lam_set_787 :: ((Vector Term83) , Int64, Term83) -> (Vector Term83) 
lam_set_787 (l0, l1, l2) =
  let (l_0, l3) = dispatch_1140 (Variant749_0, (l0, l1)) in l_0 `seq` dispatch_1141 (l3, l2)

lam_set_term_786 :: ((VarId6, Term83), Terms82) -> Terms82
lam_set_term_786 ((l0, l1), l2) =
  let (VarId6_0 l3) = l0; (Terms82_0 l4) = l2; l5 = lam_set_787 (l4, l3, l1) in Terms82_0 l5

dispatch_1142 :: (Closure260, Terms82) -> Terms82
dispatch_1142 (l0, l1) =
  case l0 of (Variant260_0 l2) -> lam_set_term_786 (l2, l1)

lam_modify_788 :: (Closure260, Terms82) -> (Terms82, ())
lam_modify_788 (l0, l1) =
  (dispatch_1142 (l0, l1), ())

dispatch_1143 :: (Closure259, Terms82) -> (Terms82, ())
dispatch_1143 (l0, l1) =
  case l0 of (Variant259_0 l2) -> lam_modify_788 (l2, l1)

lam_run_792 :: (State104, Terms82) -> (Terms82, ())
lam_run_792 (l0, l1) =
  let (State104_0 l2) = l0 in dispatch_1143 (l2, l1)

dispatch_1144 :: (Closure258, Terms82) -> (Terms82, Bool)
dispatch_1144 (l0, l1) =
  case l0 of (Variant258_0 l2) -> lam_pure_765 (l2, l1)

lam_run_793 :: (State103, Terms82) -> (Terms82, Bool)
lam_run_793 (l0, l1) =
  let (State103_0 l2) = l0 in dispatch_1144 (l2, l1)

lam_bind_791 :: ((State104, Closure262), Terms82) -> (Terms82, Bool)
lam_bind_791 ((l0, l1), l2) =
  let (l3, l4) = lam_run_792 (l0, l2) in lam_run_793 (lam_unify_789 l4, l3)

lam_bind_861 :: ((State104, Closure288), Terms82) -> (Terms82, Bool)
lam_bind_861 ((l0, l1), l2) =
  let (l3, l4) = lam_run_792 (l0, l2) in lam_run_793 (lam_unify_859 l4, l3)

lam_bind_864 :: ((State104, Closure289), Terms82) -> (Terms82, Bool)
lam_bind_864 ((l0, l1), l2) =
  let (l3, l4) = lam_run_792 (l0, l2) in lam_run_793 (lam_unify_862 l4, l3)

dispatch_1145 :: (Closure264, ()) -> State105
dispatch_1145 (l0, l1) =
  case l0 of (Variant264_0 l2) -> lam_unify_794 (l2, l1)

dispatch_1146 :: (Closure261, Terms82) -> (Terms82, Bool)
dispatch_1146 (l0, l1) =
  case l0 of (Variant261_0 l2) -> lam_bind_791 (l2, l1)

lam_run_799 :: (State105, Terms82) -> (Terms82, Bool)
lam_run_799 (l0, l1) =
  let (State105_0 l2) = l0 in dispatch_1146 (l2, l1)

lam_bind_798 :: ((State104, Closure264), Terms82) -> (Terms82, Bool)
lam_bind_798 ((l0, l1), l2) =
  let (l3, l4) = lam_run_792 (l0, l2) in lam_run_799 (dispatch_1145 (l1, l4), l3)

dispatch_1147 :: (Closure266, Bool) -> State106
dispatch_1147 (l0, l1) =
  case l0 of (Variant266_0 l2) -> lam_unify_800 (l2, l1)

dispatch_1148 :: (Closure263, Terms82) -> (Terms82, Bool)
dispatch_1148 (l0, l1) =
  case l0 of (Variant263_0 l2) -> lam_pure_765 (l2, l1); (Variant263_1 l2) -> lam_bind_798 (l2, l1)

lam_run_804 :: (State106, Terms82) -> (Terms82, Bool)
lam_run_804 (l0, l1) =
  let (State106_0 l2) = l0 in dispatch_1148 (l2, l1)

dispatch_1149 :: (Closure269, ()) -> State108
dispatch_1149 (l0, l1) =
  case l0 of (Variant269_0 l2) -> lam_follow_817 (l2, l1)

dispatch_1150 :: (Closure267, Terms82) -> (Terms82, VarId6)
dispatch_1150 (l0, l1) =
  case l0 of (Variant267_0 l2) -> lam_pure_737 (l2, l1)

lam_run_820 :: (State108, Terms82) -> (Terms82, VarId6)
lam_run_820 (l0, l1) =
  let (State108_0 l2) = l0 in dispatch_1150 (l2, l1)

lam_bind_819 :: ((State104, Closure269), Terms82) -> (Terms82, VarId6)
lam_bind_819 ((l0, l1), l2) =
  let (l3, l4) = lam_run_792 (l0, l2) in lam_run_820 (dispatch_1149 (l1, l4), l3)

dispatch_1151 :: (Closure275, VarId6) -> State109
dispatch_1151 (l0, l1) =
  case l0 of (Variant275_0 l2) -> lam_follow_821 (l2, l1)

dispatch_1153 :: (Closure268, Terms82) -> (Terms82, VarId6)
dispatch_1153 (l0, l1) =
  case l0 of (Variant268_0 l2) -> lam_pure_737 (l2, l1); (Variant268_1 l2) -> lam_bind_819 (l2, l1)

lam_run_827 :: (State109, Terms82) -> (Terms82, VarId6)
lam_run_827 (l0, l1) =
  let (State109_0 l2) = l0 in dispatch_1153 (l2, l1)

dispatch_1154 :: (Closure277, Terms82) -> State111
dispatch_1154 (l0, l1) =
  case l0 of (Variant277_0 l2) -> lam_get_term_831 (l2, l1)

dispatch_1155 :: (Closure272, Terms82) -> (Terms82, Term83)
dispatch_1155 (l0, l1) =
  case l0 of (Variant272_0 l2) -> lam_pure_830 (l2, l1)

lam_run_835 :: (State111, Terms82) -> (Terms82, Term83)
lam_run_835 (l0, l1) =
  let (State111_0 l2) = l0 in dispatch_1155 (l2, l1)

lam_bind_833 :: ((State112, Closure277), Terms82) -> (Terms82, Term83)
lam_bind_833 ((l0, l1), l2) =
  let (l3, l4) = lam_run_834 (l0, l2) in lam_run_835 (dispatch_1154 (l1, l4), l3)

dispatch_1156 :: (Closure271, Term83) -> State113
dispatch_1156 (l0, l1) =
  case l0 of (Variant271_0 l2) -> lam_follow_836 (l2, l1)

dispatch_1157 :: (Closure276, Terms82) -> (Terms82, Term83)
dispatch_1157 (l0, l1) =
  case l0 of (Variant276_0 l2) -> lam_bind_833 (l2, l1)

lam_run_841 :: (State114, Terms82) -> (Terms82, Term83)
lam_run_841 (l0, l1) =
  let (State114_0 l2) = l0 in dispatch_1157 (l2, l1)

dispatch_1158 :: (Closure274, Terms82) -> (Terms82, VarId6)
dispatch_1158 (l0, l1) =
  case l0 of (Variant274_0 l2) -> lam_pure_737 (l2, l1); (Variant274_1 l2) -> lam_bind_825 (l2, l1)

lam_bind_825 ((l0, l1), l2) =
  let (l3, l4) = lam_run_826 (l0, l2) in lam_run_827 (dispatch_1151 (l1, l4), l3)

lam_run_826 (l0, l1) =
  let (State110_0 l2) = l0 in dispatch_1152 (l2, l1)

dispatch_1152 (l0, l1) =
  case l0 of (Variant270_0 l2) -> lam_bind_840 (l2, l1)

lam_bind_840 ((l0, l1), l2) =
  let (l3, l4) = lam_run_841 (l0, l2) in lam_run_842 (dispatch_1156 (l1, l4), l3)

lam_run_842 (l0, l1) =
  let (State113_0 l2) = l0 in dispatch_1158 (l2, l1)

dispatch_1160 :: (Closure281, Term83) -> State117
dispatch_1160 (l0, l1) =
  case l0 of (Variant281_0 l2) -> lam_follow_849 (l2, l1)

dispatch_1161 :: (Closure282, Terms82) -> (Terms82, VarId6)
dispatch_1161 (l0, l1) =
  case l0 of (Variant282_0 l2) -> lam_pure_737 (l2, l1); (Variant282_1 l2) -> lam_bind_846 (l2, l1)

lam_bind_846 ((l0, l1), l2) =
  let (l3, l4) = lam_run_847 (l0, l2) in lam_run_827 (dispatch_1151 (l1, l4), l3)

lam_run_847 (l0, l1) =
  let (State116_0 l2) = l0 in dispatch_1159 (l2, l1)

dispatch_1159 (l0, l1) =
  case l0 of (Variant280_0 l2) -> lam_bind_853 (l2, l1)

lam_bind_853 ((l0, l1), l2) =
  let (l3, l4) = lam_run_841 (l0, l2) in lam_run_854 (dispatch_1160 (l1, l4), l3)

lam_run_854 (l0, l1) =
  let (State117_0 l2) = l0 in dispatch_1161 (l2, l1)

dispatch_1162 :: (Closure292, ()) -> State107
dispatch_1162 (l0, l1) =
  case l0 of (Variant292_0 l2) -> lam_unify_805 (l2, l1)

dispatch_1164 :: (Closure290, ()) -> State121
dispatch_1164 (l0, l1) =
  case l0 of (Variant290_0 l2) -> lam_unify_865 (l2, l1)

dispatch_1166 :: (Closure286, Term83) -> State120
dispatch_1166 (l0, l1) =
  case l0 of (Variant286_0 l2) -> lam_unify_858 (l2, l1)

dispatch_1168 :: (Closure284, Term83) -> State119
dispatch_1168 (l0, l1) =
  case l0 of (Variant284_0 l2) -> lam_unify_857 (l2, l1)

dispatch_1170 :: (Closure279, VarId6) -> State118
dispatch_1170 (l0, l1) =
  case l0 of (Variant279_0 l2) -> lam_unify_855 (l2, l1)

dispatch_1172 :: (Closure257, VarId6) -> State115
dispatch_1172 (l0, l1) =
  case l0 of (Variant257_0 l2) -> lam_unify_845 (l2, l1)

dispatch_1173 :: (Closure278, Terms82) -> (Terms82, Bool)
dispatch_1173 (l0, l1) =
  case l0 of (Variant278_0 l2) -> lam_bind_880 (l2, l1)

lam_bind_880 ((l0, l1), l2) =
  let (l3, l4) = lam_run_847 (l0, l2) in lam_run_881 (dispatch_1170 (l1, l4), l3)

lam_run_881 (l0, l1) =
  let (State118_0 l2) = l0 in dispatch_1171 (l2, l1)

dispatch_1171 (l0, l1) =
  case l0 of (Variant283_0 l2) -> lam_pure_765 (l2, l1); (Variant283_1 l2) -> lam_bind_877 (l2, l1)

lam_bind_877 ((l0, l1), l2) =
  let (l3, l4) = lam_run_841 (l0, l2) in lam_run_878 (dispatch_1168 (l1, l4), l3)

lam_run_878 (l0, l1) =
  let (State119_0 l2) = l0 in dispatch_1169 (l2, l1)

dispatch_1169 (l0, l1) =
  case l0 of (Variant285_0 l2) -> lam_bind_874 (l2, l1)

lam_bind_874 ((l0, l1), l2) =
  let (l3, l4) = lam_run_841 (l0, l2) in lam_run_875 (dispatch_1166 (l1, l4), l3)

lam_run_875 (l0, l1) =
  let (State120_0 l2) = l0 in dispatch_1167 (l2, l1)

dispatch_1167 (l0, l1) =
  case l0 of (Variant287_0 l2) -> lam_pure_765 (l2, l1); (Variant287_1 l2) -> lam_bind_861 (l2, l1); (Variant287_2 l2) -> lam_bind_864 (l2, l1); (Variant287_3 l2) -> lam_bind_870 (l2, l1)

lam_bind_870 ((l0, l1), l2) =
  let (l3, l4) = lam_run_792 (l0, l2) in lam_run_871 (dispatch_1164 (l1, l4), l3)

lam_run_871 (l0, l1) =
  let (State121_0 l2) = l0 in dispatch_1165 (l2, l1)

dispatch_1165 (l0, l1) =
  case l0 of (Variant291_0 l2) -> lam_bind_867 (l2, l1)

lam_bind_867 ((l0, l1), l2) =
  let (l3, l4) = lam_run_792 (l0, l2) in lam_run_868 (dispatch_1162 (l1, l4), l3)

lam_run_868 (l0, l1) =
  let (State107_0 l2) = l0 in dispatch_1163 (l2, l1)

dispatch_1163 (l0, l1) =
  case l0 of (Variant265_0 l2) -> lam_pure_765 (l2, l1); (Variant265_1 l2) -> lam_bind_803 (l2, l1)

lam_bind_803 ((l0, l1), l2) =
  let (l3, l4) = lam_run_785 (l0, l2) in lam_run_804 (dispatch_1147 (l1, l4), l3)

lam_run_785 (l0, l1) =
  let (State101_0 l2) = l0 in dispatch_1139 (l2, l1)

dispatch_1139 (l0, l1) =
  case l0 of (Variant253_0 l2) -> lam_pure_765 (l2, l1); (Variant253_1 l2) -> lam_bind_783 (l2, l1)

lam_bind_783 ((l0, l1), l2) =
  let (l3, l4) = lam_run_784 (l0, l2) in lam_run_785 (dispatch_1137 (l1, l4), l3)

lam_run_784 (l0, l1) =
  let (State102_0 l2) = l0 in dispatch_1138 (l2, l1)

dispatch_1138 (l0, l1) =
  case l0 of (Variant256_0 l2) -> lam_bind_884 (l2, l1)

lam_bind_884 ((l0, l1), l2) =
  let (l3, l4) = lam_run_826 (l0, l2) in lam_run_885 (dispatch_1172 (l1, l4), l3)

lam_run_885 (l0, l1) =
  let (State115_0 l2) = l0 in dispatch_1173 (l2, l1)

dispatch_1174 :: (Closure294, VarId6) -> State102
dispatch_1174 (l0, l1) =
  case l0 of (Variant294_0 l2) -> lam_solve_887 (l2, l1)

lam_bind_889 :: ((State85, Closure294), Terms82) -> (Terms82, Bool)
lam_bind_889 ((l0, l1), l2) =
  let (l3, l4) = lam_run_733 (l0, l2) in lam_run_784 (dispatch_1174 (l1, l4), l3)

dispatch_1175 :: (Closure234, ()) -> Option81
dispatch_1175 (l0, l1) =
  case l0 of (Variant234_0 l2) -> lam_map_727 (l2, l1)

lam_next_894 :: Iter80 -> Option81
lam_next_894 l0 =
  let (Iter80_0 l1) = l0 in dispatch_1175 (l1, ())

lam_for_each_checked_893 :: (Iter80, Closure297) -> State123
lam_for_each_checked_893 (l0, l1) =
  case lam_next_894 l0 of (Some81_0 (l2, l3)) -> lam_bind_897 (lam_solve_890 l2, Variant296_0 (l3, l1)); (None81_1) -> lam_pure_896 True

lam_for_each_checked_895 :: ((Iter80, Closure297), Bool) -> State123
lam_for_each_checked_895 ((l0, l1), l2) =
  case l2 of True -> lam_for_each_checked_893 (l0, l1); False -> lam_pure_896 False

lam_wrapped_for_each_checked_960 :: (Iter80, Closure297) -> State123
lam_wrapped_for_each_checked_960 l0 =
  lam_for_each_checked_893 l0

dispatch_1176 :: (Closure296, Bool) -> State123
dispatch_1176 (l0, l1) =
  case l0 of (Variant296_0 l2) -> lam_for_each_checked_895 (l2, l1)

dispatch_1177 :: (Closure293, Terms82) -> (Terms82, Bool)
dispatch_1177 (l0, l1) =
  case l0 of (Variant293_0 l2) -> lam_bind_889 (l2, l1)

lam_run_899 :: (State122, Terms82) -> (Terms82, Bool)
lam_run_899 (l0, l1) =
  let (State122_0 l2) = l0 in dispatch_1177 (l2, l1)

dispatch_1178 :: (Closure295, Terms82) -> (Terms82, Bool)
dispatch_1178 (l0, l1) =
  case l0 of (Variant295_0 l2) -> lam_pure_765 (l2, l1); (Variant295_1 l2) -> lam_bind_898 (l2, l1)

lam_bind_898 ((l0, l1), l2) =
  let (l3, l4) = lam_run_899 (l0, l2) in lam_run_900 (dispatch_1176 (l1, l4), l3)

lam_run_900 (l0, l1) =
  let (State123_0 l2) = l0 in dispatch_1178 (l2, l1)

dispatch_1179 :: (Closure303, Expr7) -> State124
dispatch_1179 (l0, l1) =
  case l0 of (Variant303_0 l2) -> lam_get_expr_902 (l2, l1)

dispatch_1181 :: (Closure298, Terms82) -> (Terms82, (Vector Expr7) )
dispatch_1181 (l0, l1) =
  case l0 of (Variant298_0 l2) -> lam_pure_901 (l2, l1)

lam_run_906 :: (State124, Terms82) -> (Terms82, (Vector Expr7) )
lam_run_906 (l0, l1) =
  let (State124_0 l2) = l0 in dispatch_1181 (l2, l1)

dispatch_1183 :: (Closure305, Term83) -> State130
dispatch_1183 (l0, l1) =
  case l0 of (Variant305_0 l2) -> lam_follow_916 (l2, l1)

dispatch_1184 :: (Closure306, Terms82) -> (Terms82, VarId6)
dispatch_1184 (l0, l1) =
  case l0 of (Variant306_0 l2) -> lam_pure_737 (l2, l1); (Variant306_1 l2) -> lam_bind_913 (l2, l1)

lam_bind_913 ((l0, l1), l2) =
  let (l3, l4) = lam_run_914 (l0, l2) in lam_run_827 (dispatch_1151 (l1, l4), l3)

lam_run_914 (l0, l1) =
  let (State129_0 l2) = l0 in dispatch_1182 (l2, l1)

dispatch_1182 (l0, l1) =
  case l0 of (Variant304_0 l2) -> lam_bind_920 (l2, l1)

lam_bind_920 ((l0, l1), l2) =
  let (l3, l4) = lam_run_841 (l0, l2) in lam_run_921 (dispatch_1183 (l1, l4), l3)

lam_run_921 (l0, l1) =
  let (State130_0 l2) = l0 in dispatch_1184 (l2, l1)

dispatch_1185 :: (Closure310, (Vector Expr7) ) -> State133
dispatch_1185 (l0, l1) =
  case l0 of (Variant310_0 l2) -> lam_get_expr_925 (l2, l1)

dispatch_1187 :: (Closure311, Terms82) -> (Terms82, Expr7)
dispatch_1187 (l0, l1) =
  case l0 of (Variant311_0 l2) -> lam_pure_907 (l2, l1)

lam_run_930 :: (State133, Terms82) -> (Terms82, Expr7)
lam_run_930 (l0, l1) =
  let (State133_0 l2) = l0 in dispatch_1187 (l2, l1)

dispatch_1188 :: (Closure301, ()) -> Option127
dispatch_1188 (l0, l1) =
  case l0 of (Variant301_0 l2) -> lam_map_909 (l2, l1)

lam_next_933 :: Iter126 -> Option127
lam_next_933 l0 =
  let (Iter126_0 l1) = l0 in dispatch_1188 (l1, ())

lam_for_each_accum_932 :: (Iter126, (Vector Expr7) , Closure314) -> State134
lam_for_each_accum_932 (l0, l1, l2) =
  case lam_next_933 l0 of (Some127_0 (l3, l4)) -> lam_bind_935 (lam_get_expr_910 (l1, l3), Variant313_0 (l4, l2)); (None127_1) -> lam_pure_938 l1

lam_wrapped_for_each_accum_931 :: (Iter126, (Vector Expr7) , Closure314) -> State134
lam_wrapped_for_each_accum_931 l0 =
  lam_for_each_accum_932 l0

lam_get_expr_923 :: (VarId6, Term83) -> State132
lam_get_expr_923 (l0, l1) =
  case l1 of (Equal83_1 l_0) -> panic ((V.fromList [102, 111, 108, 108, 111, 119, 32, 115, 104, 111, 117, 108, 100, 32, 110, 101, 118, 101, 114, 32, 114, 101, 116, 117, 114, 110, 32, 97, 32, 112, 111, 105, 110, 116, 101, 114, 32, 116, 111, 32, 97, 110, 32, 69, 113, 117, 97, 108])); (Recursing83_3) -> panic ((V.fromList [82, 101, 99, 117, 114, 115, 105, 110, 103, 32, 115, 104, 111, 117, 108, 100, 32, 110, 111, 116, 32, 111, 99, 99, 117, 114, 32, 105, 110, 32, 97, 32, 118, 97, 108, 105, 100, 32, 115, 111, 108, 117, 116, 105, 111, 110])); (Unknown83_0) -> lam_pure_924 (EVar7_0 l0); (Term83_2 (l2, l3)) -> lam_bind_927 (lam_wrapped_for_each_accum_931 (lam_items_939 l3, (V.fromList []), Variant314_0 ()), Variant310_0 l2)

lam_for_each_accum_934 :: ((Iter126, Closure314), (Vector Expr7) ) -> State134
lam_for_each_accum_934 ((l0, l1), l2) =
  lam_for_each_accum_932 (l0, l2, l1)

dispatch_1189 :: (Closure313, (Vector Expr7) ) -> State134
dispatch_1189 (l0, l1) =
  case l0 of (Variant313_0 l2) -> lam_for_each_accum_934 (l2, l1)

dispatch_1191 :: (Closure308, Term83) -> State132
dispatch_1191 (l0, l1) =
  case l0 of (Variant308_0 l2) -> lam_get_expr_923 (l2, l1)

dispatch_1193 :: (Closure307, Terms82) -> (Terms82, Expr7)
dispatch_1193 (l0, l1) =
  case l0 of (Variant307_0 l2) -> lam_bind_942 (l2, l1)

lam_bind_942 ((l0, l1), l2) =
  let (l3, l4) = lam_run_841 (l0, l2) in lam_run_943 (dispatch_1191 (l1, l4), l3)

lam_run_943 (l0, l1) =
  let (State132_0 l2) = l0 in dispatch_1192 (l2, l1)

dispatch_1192 (l0, l1) =
  case l0 of (Variant309_0 l2) -> lam_pure_907 (l2, l1); (Variant309_1 l2) -> lam_bind_928 (l2, l1)

lam_bind_928 ((l0, l1), l2) =
  let (l3, l4) = lam_run_929 (l0, l2) in lam_run_930 (dispatch_1185 (l1, l4), l3)

lam_run_929 (l0, l1) =
  let (State134_0 l2) = l0 in dispatch_1186 (l2, l1)

dispatch_1186 (l0, l1) =
  case l0 of (Variant312_0 l2) -> lam_pure_901 (l2, l1); (Variant312_1 l2) -> lam_bind_936 (l2, l1)

lam_bind_936 ((l0, l1), l2) =
  let (l3, l4) = lam_run_937 (l0, l2) in lam_run_929 (dispatch_1189 (l1, l4), l3)

lam_run_937 (l0, l1) =
  let (State128_0 l2) = l0 in dispatch_1190 (l2, l1)

dispatch_1190 (l0, l1) =
  case l0 of (Variant302_0 l2) -> lam_bind_904 (l2, l1)

lam_bind_904 ((l0, l1), l2) =
  let (l3, l4) = lam_run_905 (l0, l2) in lam_run_906 (dispatch_1179 (l1, l4), l3)

lam_run_905 (l0, l1) =
  let (State125_0 l2) = l0 in dispatch_1180 (l2, l1)

dispatch_1180 (l0, l1) =
  case l0 of (Variant299_0 l2) -> lam_bind_945 (l2, l1)

lam_bind_945 ((l0, l1), l2) =
  let (l3, l4) = lam_run_914 (l0, l2) in lam_run_946 (lam_get_expr_922 l4, l3)

lam_run_946 (l0, l1) =
  let (State131_0 l2) = l0 in dispatch_1193 (l2, l1)

dispatch_1194 :: (Closure315, Terms82) -> (Terms82, Option32)
dispatch_1194 (l0, l1) =
  case l0 of (Variant315_0 l2) -> lam_pure_948 (l2, l1)

lam_run_952 :: (State135, Terms82) -> (Terms82, Option32)
lam_run_952 (l0, l1) =
  let (State135_0 l2) = l0 in dispatch_1194 (l2, l1)

lam_bind_951 :: ((State125, Closure317), Terms82) -> (Terms82, Option32)
lam_bind_951 ((l0, l1), l2) =
  let (l3, l4) = lam_run_905 (l0, l2) in lam_run_952 (lam_solve_949 l4, l3)

dispatch_1195 :: (Closure319, Bool) -> State136
dispatch_1195 (l0, l1) =
  case l0 of (Variant319_0 l2) -> lam_solve_953 (l2, l1)

dispatch_1196 :: (Closure316, Terms82) -> (Terms82, Option32)
dispatch_1196 (l0, l1) =
  case l0 of (Variant316_0 l2) -> lam_pure_948 (l2, l1); (Variant316_1 l2) -> lam_bind_951 (l2, l1)

lam_run_958 :: (State136, Terms82) -> (Terms82, Option32)
lam_run_958 (l0, l1) =
  let (State136_0 l2) = l0 in dispatch_1196 (l2, l1)

lam_bind_957 :: ((State123, Closure319), Terms82) -> (Terms82, Option32)
lam_bind_957 ((l0, l1), l2) =
  let (l3, l4) = lam_run_900 (l0, l2) in lam_run_958 (dispatch_1195 (l1, l4), l3)

dispatch_1197 :: (Closure318, Terms82) -> (Terms82, Option32)
dispatch_1197 (l0, l1) =
  case l0 of (Variant318_0 l2) -> lam_bind_957 (l2, l1)

lam_run_968 :: (State137, Terms82) -> (Terms82, Option32)
lam_run_968 (l0, l1) =
  let (State137_0 l2) = l0 in dispatch_1197 (l2, l1)

lam_solve_722 :: Problem5 -> Option32
lam_solve_722 l0 =
  let (Problem5_0 (l1, l2, l3)) = l0; l4 = lam_bind_959 (lam_wrapped_for_each_checked_960 (lam_items_961 l3, Variant297_0 ()), Variant319_0 l2); l5 = lam_fill_with_965 (l1, Variant747_0 ()); (l_0, l6) = lam_run_968 (l4, Terms82_0 l5) in l_0 `seq` l6

dispatch_1198 :: (Closure231, ()) -> Option77
dispatch_1198 (l0, l1) =
  case l0 of (Variant231_0 l2) -> lam_map_719 (l2, l1)

lam_next_971 :: Iter76 -> Option77
lam_next_971 l0 =
  let (Iter76_0 l1) = l0 in dispatch_1198 (l1, ())

lam_map_970 :: ((Iter76, Closure321), ()) -> Option139
lam_map_970 ((l0, l1), ()) =
  case lam_next_971 l0 of (Some77_0 (l2, l3)) -> Some139_0 (lam_solve_722 l2, lam_map_969 (l3, l1)); (None77_1) -> None139_1

dispatch_1199 :: (Closure320, ()) -> Option139
dispatch_1199 (l0, l1) =
  case l0 of (Variant320_0 l2) -> lam_map_970 (l2, l1)

lam_next_977 :: Iter138 -> Option139
lam_next_977 l0 =
  let (Iter138_0 l1) = l0 in dispatch_1199 (l1, ())

lam_foldl_976 :: (Iter138, (Vector Option32) , Closure672) -> (Vector Option32) 
lam_foldl_976 (l0, l1, l2) =
  case lam_next_977 l0 of (Some139_0 (l3, l4)) -> lam_foldl_976 (l4, intrinsicPush l1 l3, l2); (None139_1) -> l1

lam_wrapped_foldl_975 :: (Iter138, (Vector Option32) , Closure672) -> (Vector Option32) 
lam_wrapped_foldl_975 l0 =
  lam_foldl_976 l0

lam_from_iter_with_capacity_974 :: (Iter138, Int64) -> (Vector Option32) 
lam_from_iter_with_capacity_974 (l0, l1) =
  lam_wrapped_foldl_975 (l0, intrinsicReserve ((V.fromList [])) l1, push_379 ())

lam_from_iter_973 :: Iter138 -> (Vector Option32) 
lam_from_iter_973 l0 =
  lam_from_iter_with_capacity_974 (l0, 0)

lam_solve_problems_721 :: Iter76 -> (Vector Option32) 
lam_solve_problems_721 l0 =
  let l1 = (let l1 = l0 in lam_wrapped_map_972 (l1, solve_375 ())) in lam_from_iter_973 l1

lam_main_714 :: ((Vector Problem5) , ()) -> (Vector Option32) 
lam_main_714 (l0, ()) =
  lam_solve_problems_721 (lam_items_978 l0)

dispatch_1200 :: (Closure707, ()) -> (Vector Option32) 
dispatch_1200 (l0, l1) =
  case l0 of (Variant707_0 l2) -> lam_main_714 (l2, l1)

lam_repeat_982 :: (Int64, Closure707) -> Option73
lam_repeat_982 (l0, l1) =
  case uncurry (<) (l0, 1) of True -> None73_1; False -> (let l2 = dispatch_1200 (l1, ()) in (case uncurry (==) (l0, 1) of True -> Some73_0 l2; False -> lam_repeat_982 (uncurry (-) (l0, 1), l1)))

lam_wrapped_repeat_981 :: (Int64, Closure707) -> Option73
lam_wrapped_repeat_981 l0 =
  lam_repeat_982 l0

dispatch_1201 :: (Closure324, Int64) -> Option32
dispatch_1201 (l0, l1) =
  case l0 of (Variant324_0 l2) -> lam_items_985 (l2, l1)

dispatch_1202 :: (Closure322, ()) -> Option141
dispatch_1202 (l0, l1) =
  case l0 of (Variant322_0 l2) -> lam_range_984 (l2, l1)

lam_next_988 :: Iter140 -> Option141
lam_next_988 l0 =
  let (Iter140_0 l1) = l0 in dispatch_1202 (l1, ())

lam_map_987 :: ((Iter140, Closure324), ()) -> Option143
lam_map_987 ((l0, l1), ()) =
  case lam_next_988 l0 of (Some141_0 (l2, l3)) -> Some143_0 (dispatch_1201 (l1, l2), lam_map_986 (l3, l1)); (None141_1) -> None143_1

dispatch_1203 :: (Closure325, ()) -> Option145
dispatch_1203 (l0, l1) =
  case l0 of (Variant325_0 l2) -> lam_map_1000 (l2, l1)

lam_next_1003 :: Iter144 -> Option145
lam_next_1003 l0 =
  let (Iter144_0 l1) = l0 in dispatch_1203 (l1, ())

lam_for_each_1002 :: (Iter144, Closure697) -> ()
lam_for_each_1002 (l0, l1) =
  case lam_next_1003 l0 of (Some145_0 (l2, l3)) -> (let l_0 = lam_output_expr_991 l2 in l_0 `seq` lam_for_each_1002 (l3, l1)); (None145_1) -> ()

lam_output_expr_991 l0 =
  case l0 of (EVar7_0 (VarId6_0 l1)) -> (let l_0 = output ((V.fromList [63])) in l_0 `seq` output (lam_int_to_string_992 l1)); (EHead7_1 ((HeadId8_0 l1), l2)) -> (let l_0 = output ((V.fromList [33])); l_1 = output (lam_int_to_string_992 l1); l_2 = output ((V.fromList [40])); l_3 = lam_wrapped_for_each_1001 (lam_items_1004 l2, output_expr_406 ()) in l_0 `seq` l_1 `seq` l_2 `seq` l_3 `seq` output ((V.fromList [41])))

lam_wrapped_for_each_1001 l0 =
  lam_for_each_1002 l0

lam_wrapped_output_expr_990 :: Expr7 -> ()
lam_wrapped_output_expr_990 l0 =
  lam_output_expr_991 l0

lam_main_989 :: Option32 -> ()
lam_main_989 l0 =
  case l0 of (Some32_0 l1) -> (let l_0 = lam_wrapped_output_expr_990 l1 in l_0 `seq` output ((V.fromList [10]))); (None32_1) -> output ((V.fromList [117, 110, 115, 97, 116, 10]))

dispatch_1204 :: (Closure323, ()) -> Option143
dispatch_1204 (l0, l1) =
  case l0 of (Variant323_0 l2) -> lam_map_987 (l2, l1)

lam_next_1008 :: Iter142 -> Option143
lam_next_1008 l0 =
  let (Iter142_0 l1) = l0 in dispatch_1204 (l1, ())

lam_for_each_1007 :: (Iter142, Closure708) -> ()
lam_for_each_1007 (l0, l1) =
  case lam_next_1008 l0 of (Some143_0 (l2, l3)) -> (let l_0 = lam_main_989 l2 in l_0 `seq` lam_for_each_1007 (l3, l1)); (None143_1) -> ()

lam_wrapped_for_each_1006 :: (Iter142, Closure708) -> ()
lam_wrapped_for_each_1006 l0 =
  lam_for_each_1007 l0

lam_main_416 :: () -> ()
lam_main_416 () =
  case lam_string_to_nat_417 (input ()) of (Some0_0 l0) -> (let l1 = lam_read_problems_437 (); l2 = lam_wrapped_repeat_981 (l0, Variant707_0 l1) in (case l2 of (Some73_0 l3) -> lam_wrapped_for_each_1006 (lam_items_1009 l3, Variant708_0 ()); (None73_1) -> ())); (None0_1) -> lam_writeln_1012 ((V.fromList [80, 108, 101, 97, 115, 101, 32, 101, 110, 116, 101, 114, 32, 97, 110, 32, 105, 116, 101, 114, 97, 116, 105, 111, 110, 32, 99, 111, 117, 110, 116]))

dispatch_1205 :: (Closure706, ()) -> ()
dispatch_1205 (l0, l1) =
  case l0 of (Variant706_0 l2) -> lam_main_416 l1

main_wrapper_1013 :: () -> ()
main_wrapper_1013 () =
  dispatch_1205 (main_415 (), ())


main :: IO ()
main = main_wrapper_1013 () `seq` return ()
