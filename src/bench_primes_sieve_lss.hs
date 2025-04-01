
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

data Primality5
  = Prime5_0
  | Composite5_1

data Option14
  = Some14_0 (Vector Primality5) 
  | None14_1

data Closure23
  = Variant23_0 (Int64, Int64)

data Iter1
  = Iter1_0 Closure23

data Option2
  = Some2_0 (Int64, Iter1)
  | None2_1

data Closure25
  = Variant25_0 (Vector Word8) 

data Closure24
  = Variant24_0 (Iter1, Closure25)

data Iter3
  = Iter3_0 Closure24

data Option4
  = Some4_0 (Word8, Iter3)
  | None4_1

data Closure26
  = Variant26_0 (Int64, Int64)

data Iter6
  = Iter6_0 Closure26

data Option7
  = Some7_0 (Int64, Iter6)
  | None7_1

data Closure27
  = Variant27_0 Int64

data Iter8
  = Iter8_0 Closure27

data Option9
  = Some9_0 (Int64, Iter8)
  | None9_1

data Closure29
  = Variant29_0 Int64

data Closure28
  = Variant28_0 (Iter8, Closure29)

data Iter10
  = Iter10_0 Closure28

data Option11
  = Some11_0 (Int64, Iter10)
  | None11_1

data Closure31
  = Variant31_0 Int64

data Closure30
  = Variant30_0 (Iter10, Closure31)

data Iter12
  = Iter12_0 Closure30

data Option13
  = Some13_0 (Int64, Iter12)
  | None13_1

data Closure32
  = Variant32_0 (Int64, Int64)

data Iter15
  = Iter15_0 Closure32

data Option16
  = Some16_0 (Int64, Iter15)
  | None16_1

data Closure34
  = Variant34_0 (Vector Primality5) 

data Closure33
  = Variant33_0 (Iter15, Closure34)

data Iter17
  = Iter17_0 Closure33

data Option18
  = Some18_0 (Primality5, Iter17)
  | None18_1

data Closure35
  = Variant35_0 Int64

data Iter19
  = Iter19_0 Closure35

data Option20
  = Some20_0 (Int64, Iter19)
  | None20_1

data Closure36
  = Variant36_0 (Iter19, Iter17)

data Iter21
  = Iter21_0 Closure36

data Option22
  = Some22_0 ((Int64, Primality5), Iter21)
  | None22_1

data Closure37
  = Variant37_0 ()

data Closure38
  = Variant38_0 ()

data Closure39
  = Variant39_0 ()

data Closure40
  = Variant40_0 ()

data Closure41
  = Variant41_0 ()

data Closure42
  = Variant42_0 ()

data Closure43
  = Variant43_0 ()

data Closure44
  = Variant44_0 ()

data Closure45
  = Variant45_0 ()

data Closure46
  = Variant46_0 ()

data Closure47
  = Variant47_0 ()

data Closure48
  = Variant48_0 ()

data Closure49
  = Variant49_0 ()

data Closure50
  = Variant50_0 ()

data Closure51
  = Variant51_0 ()

data Closure52
  = Variant52_0

data Closure53
  = Variant53_0 ()

data Closure54
  = Variant54_0 ()

data Closure55
  = Variant55_0 ()

data Closure56
  = Variant56_0 ()

data Closure57
  = Variant57_0 ()

data Closure58
  = Variant58_0 ()

data Closure59
  = Variant59_0

data Closure60
  = Variant60_0 ()

data Closure61
  = Variant61_0 ()

data Closure62
  = Variant62_0 ()

data Closure63
  = Variant63_0 ()

data Closure64
  = Variant64_0 ()

data Closure65
  = Variant65_0 ()

data Closure66
  = Variant66_0 ()

data Closure67
  = Variant67_0 ()

data Closure68
  = Variant68_0 ()

data Closure69
  = Variant69_0 ()

data Closure70
  = Variant70_0 ()

data Closure71
  = Variant71_0 ()

data Closure72
  = Variant72_0 ()

data Closure73
  = Variant73_0 ()

data Closure74
  = Variant74_0 ()

data Closure75
  = Variant75_0 ()

data Closure76
  = Variant76_0 ()

data Closure77
  = Variant77_0 ()

data Closure78
  = Variant78_0 ()

data Closure79
  = Variant79_0 ()

data Closure80
  = Variant80_0 ()

data Closure81
  = Variant81_0 ()

data Closure82
  = Variant82_0 ()

data Closure83
  = Variant83_0 ()

data Closure84
  = Variant84_0 ()

data Closure85
  = Variant85_0 ()

data Closure86
  = Variant86_0 ()

data Closure87
  = Variant87_0 ()

data Closure88
  = Variant88_0 ()

data Closure89
  = Variant89_0 ()

data Closure90
  = Variant90_0 ()

data Closure91
  = Variant91_0

data Closure92
  = Variant92_0 ()

data Closure93
  = Variant93_0

data Closure94
  = Variant94_0

data Closure95
  = Variant95_0 ()

data Closure96
  = Variant96_0 ()

data Closure97
  = Variant97_0 ()

data Closure98
  = Variant98_0 ()

data Closure99
  = Variant99_0 ()

data Closure100
  = Variant100_0 ()

data Closure101
  = Variant101_0 ()

data Closure102
  = Variant102_0 ()

data Closure103
  = Variant103_0 ()

data Closure104
  = Variant104_0 Int64

data Closure105
  = Variant105_0 ()

data Closure106
  = Variant106_0 ()

data Closure107
  = Variant107_0 Word8

data Closure108
  = Variant108_0 Int64

data Closure109
  = Variant109_0 Int64

data Closure110
  = Variant110_0 Primality5

data Closure111
  = Variant111_0 (Primality5 -> Vector Primality5)

data Closure112
  = Variant112_0

data Closure113
  = Variant113_0 ()

data Closure114
  = Variant114_0 ()

range_0 :: () -> Closure37
range_0 () =
  Variant37_0 ()

next_1 :: () -> Closure38
next_1 () =
  Variant38_0 ()

map_2 :: () -> Closure39
map_2 () =
  Variant39_0 ()

next_3 :: () -> Closure40
next_3 () =
  Variant40_0 ()

map_4 :: () -> Closure41
map_4 () =
  Variant41_0 ()

digit_to_nat_5 :: () -> Closure42
digit_to_nat_5 () =
  Variant42_0 ()

and_then_6 :: () -> Closure43
and_then_6 () =
  Variant43_0 ()

foldl_7 :: () -> Closure44
foldl_7 () =
  Variant44_0 ()

wrapped_foldl_8 :: () -> Closure45
wrapped_foldl_8 () =
  Variant45_0 ()

chars_to_nat_9 :: () -> Closure46
chars_to_nat_9 () =
  Variant46_0 ()

wrapped_map_10 :: () -> Closure47
wrapped_map_10 () =
  Variant47_0 ()

wrapped_range_11 :: () -> Closure48
wrapped_range_11 () =
  Variant48_0 ()

items_12 :: () -> Closure49
items_12 () =
  Variant49_0 ()

string_to_nat_13 :: () -> Closure50
string_to_nat_13 () =
  Variant50_0 ()

fill_with_rec_14 :: () -> Closure51
fill_with_rec_14 () =
  Variant51_0 ()

push__16 :: () -> Closure52
push__16 () =
  Variant52_0

push_15 :: () -> Closure52
push_15 () =
  push__16 ()

wrapped_fill_with_rec_17 :: () -> Closure53
wrapped_fill_with_rec_17 () =
  Variant53_0 ()

fill_with_18 :: () -> Closure54
fill_with_18 () =
  Variant54_0 ()

fill_19 :: () -> Closure55
fill_19 () =
  Variant55_0 ()

set_20 :: () -> Closure56
set_20 () =
  Variant56_0 ()

range_21 :: () -> Closure57
range_21 () =
  Variant57_0 ()

wrapped_range_22 :: () -> Closure58
wrapped_range_22 () =
  Variant58_0 ()

get__24 :: () -> Closure59
get__24 () =
  Variant59_0

get_23 :: () -> Closure59
get_23 () =
  get__24 ()

ints_25 :: () -> Closure60
ints_25 () =
  Variant60_0 ()

next_26 :: () -> Closure61
next_26 () =
  Variant61_0 ()

map_27 :: () -> Closure62
map_27 () =
  Variant62_0 ()

next_28 :: () -> Closure63
next_28 () =
  Variant63_0 ()

take_while_29 :: () -> Closure64
take_while_29 () =
  Variant64_0 ()

wrapped_ints_30 :: () -> Closure65
wrapped_ints_30 () =
  Variant65_0 ()

wrapped_map_31 :: () -> Closure66
wrapped_map_31 () =
  Variant66_0 ()

wrapped_take_while_32 :: () -> Closure67
wrapped_take_while_32 () =
  Variant67_0 ()

next_33 :: () -> Closure68
next_33 () =
  Variant68_0 ()

foldl_34 :: () -> Closure69
foldl_34 () =
  Variant69_0 ()

wrapped_foldl_35 :: () -> Closure70
wrapped_foldl_35 () =
  Variant70_0 ()

next_36 :: () -> Closure71
next_36 () =
  Variant71_0 ()

foldl_37 :: () -> Closure72
foldl_37 () =
  Variant72_0 ()

wrapped_foldl_38 :: () -> Closure73
wrapped_foldl_38 () =
  Variant73_0 ()

sieve_39 :: () -> Closure74
sieve_39 () =
  Variant74_0 ()

repeat_40 :: () -> Closure75
repeat_40 () =
  Variant75_0 ()

wrapped_repeat_41 :: () -> Closure76
wrapped_repeat_41 () =
  Variant76_0 ()

range_42 :: () -> Closure77
range_42 () =
  Variant77_0 ()

next_43 :: () -> Closure78
next_43 () =
  Variant78_0 ()

map_44 :: () -> Closure79
map_44 () =
  Variant79_0 ()

ints_45 :: () -> Closure80
ints_45 () =
  Variant80_0 ()

next_46 :: () -> Closure81
next_46 () =
  Variant81_0 ()

next_47 :: () -> Closure82
next_47 () =
  Variant82_0 ()

zip_48 :: () -> Closure83
zip_48 () =
  Variant83_0 ()

wrapped_map_49 :: () -> Closure84
wrapped_map_49 () =
  Variant84_0 ()

wrapped_range_50 :: () -> Closure85
wrapped_range_50 () =
  Variant85_0 ()

items_51 :: () -> Closure86
items_51 () =
  Variant86_0 ()

wrapped_zip_52 :: () -> Closure87
wrapped_zip_52 () =
  Variant87_0 ()

wrapped_ints_53 :: () -> Closure88
wrapped_ints_53 () =
  Variant88_0 ()

enumerate_54 :: () -> Closure89
enumerate_54 () =
  Variant89_0 ()

writeln_55 :: () -> Closure90
writeln_55 () =
  Variant90_0 ()

len__57 :: () -> Closure91
len__57 () =
  Variant91_0

len_56 :: () -> Closure91
len_56 () =
  len__57 ()

concat_from_58 :: () -> Closure92
concat_from_58 () =
  Variant92_0 ()

push__60 :: () -> Closure93
push__60 () =
  Variant93_0

push_59 :: () -> Closure93
push_59 () =
  push__60 ()

get__62 :: () -> Closure94
get__62 () =
  Variant94_0

get_61 :: () -> Closure94
get_61 () =
  get__62 ()

wrapped_concat_from_63 :: () -> Closure95
wrapped_concat_from_63 () =
  Variant95_0 ()

concat_64 :: () -> Closure96
concat_64 () =
  Variant96_0 ()

nat_to_string_65 :: () -> Closure97
nat_to_string_65 () =
  Variant97_0 ()

wrapped_nat_to_string_66 :: () -> Closure98
wrapped_nat_to_string_66 () =
  Variant98_0 ()

int_to_string_67 :: () -> Closure99
int_to_string_67 () =
  Variant99_0 ()

next_68 :: () -> Closure100
next_68 () =
  Variant100_0 ()

for_each_69 :: () -> Closure101
for_each_69 () =
  Variant101_0 ()

wrapped_for_each_70 :: () -> Closure102
wrapped_for_each_70 () =
  Variant102_0 ()

main_71 :: () -> Closure103
main_71 () =
  Variant103_0 ()

lam_range_74 :: (Int64, Int64) -> Iter1
lam_range_74 (l0, l1) =
  Iter1_0 (Variant23_0 (l0, l1))

lam_range_75 :: ((Int64, Int64), ()) -> Option2
lam_range_75 ((l0, l1), ()) =
  case uncurry (<) (l0, l1) of True -> Some2_0 (l0, lam_range_74 (uncurry (+) (l0, 1), l1)); False -> None2_1

lam_items_76 :: ((Vector Word8) , Int64) -> Word8
lam_items_76 (l0, l1) =
  intrinsicGet l0 l1

lam_map_77 :: (Iter1, Closure25) -> Iter3
lam_map_77 (l0, l1) =
  Iter3_0 (Variant24_0 (l0, l1))

lam_chars_to_nat_84 :: (Int64, Int64) -> Int64
lam_chars_to_nat_84 (l0, l1) =
  uncurry (+) (uncurry (*) (l0, 10), l1)

lam_digit_to_nat_86 :: Word8 -> Option0
lam_digit_to_nat_86 l0 =
  case l0 of 48 -> Some0_0 0; 49 -> Some0_0 1; 50 -> Some0_0 2; 51 -> Some0_0 3; 52 -> Some0_0 4; 53 -> Some0_0 5; 54 -> Some0_0 6; 55 -> Some0_0 7; 56 -> Some0_0 8; 57 -> Some0_0 9; l_0 -> None0_1

lam_wrapped_map_91 :: (Iter1, Closure25) -> Iter3
lam_wrapped_map_91 l0 =
  lam_map_77 l0

lam_wrapped_range_92 :: (Int64, Int64) -> Iter1
lam_wrapped_range_92 l0 =
  lam_range_74 l0

lam_items_90 :: (Vector Word8)  -> Iter3
lam_items_90 l0 =
  lam_wrapped_map_91 (lam_wrapped_range_92 (0, intrinsicLen l0), Variant25_0 l0)

lam_fill_96 :: (Primality5, ()) -> Primality5
lam_fill_96 (l0, ()) =
  l0

lam_range_101 :: (Int64, Int64) -> Iter6
lam_range_101 (l0, l1) =
  Iter6_0 (Variant26_0 (l0, l1))

lam_range_102 :: ((Int64, Int64), ()) -> Option7
lam_range_102 ((l0, l1), ()) =
  case uncurry (<) (l0, l1) of True -> Some7_0 (l0, lam_range_101 (uncurry (+) (l0, 1), l1)); False -> None7_1

lam_wrapped_range_103 :: (Int64, Int64) -> Iter6
lam_wrapped_range_103 l0 =
  lam_range_101 l0

lam_ints_105 :: Int64 -> Iter8
lam_ints_105 l0 =
  Iter8_0 (Variant27_0 l0)

lam_ints_106 :: (Int64, ()) -> Option9
lam_ints_106 (l0, ()) =
  Some9_0 (l0, lam_ints_105 (uncurry (+) (l0, 1)))

lam_sieve_107 :: (Int64, Int64) -> Int64
lam_sieve_107 (l0, l1) =
  uncurry (*) (l1, l0)

lam_map_108 :: (Iter8, Closure29) -> Iter10
lam_map_108 (l0, l1) =
  Iter10_0 (Variant28_0 (l0, l1))

lam_sieve_111 :: (Int64, Int64) -> Bool
lam_sieve_111 (l0, l1) =
  uncurry (<) (l1, l0)

lam_take_while_112 :: (Iter10, Closure31) -> Iter12
lam_take_while_112 (l0, l1) =
  Iter12_0 (Variant30_0 (l0, l1))

lam_wrapped_ints_115 :: Int64 -> Iter8
lam_wrapped_ints_115 l0 =
  lam_ints_105 l0

lam_wrapped_map_116 :: (Iter8, Closure29) -> Iter10
lam_wrapped_map_116 l0 =
  lam_map_108 l0

lam_wrapped_take_while_117 :: (Iter10, Closure31) -> Iter12
lam_wrapped_take_while_117 l0 =
  lam_take_while_112 l0

lam_range_127 :: (Int64, Int64) -> Iter15
lam_range_127 (l0, l1) =
  Iter15_0 (Variant32_0 (l0, l1))

lam_range_128 :: ((Int64, Int64), ()) -> Option16
lam_range_128 ((l0, l1), ()) =
  case uncurry (<) (l0, l1) of True -> Some16_0 (l0, lam_range_127 (uncurry (+) (l0, 1), l1)); False -> None16_1

lam_items_129 :: ((Vector Primality5) , Int64) -> Primality5
lam_items_129 (l0, l1) =
  intrinsicGet l0 l1

lam_map_130 :: (Iter15, Closure34) -> Iter17
lam_map_130 (l0, l1) =
  Iter17_0 (Variant33_0 (l0, l1))

lam_ints_133 :: Int64 -> Iter19
lam_ints_133 l0 =
  Iter19_0 (Variant35_0 l0)

lam_ints_134 :: (Int64, ()) -> Option20
lam_ints_134 (l0, ()) =
  Some20_0 (l0, lam_ints_133 (uncurry (+) (l0, 1)))

lam_zip_135 :: (Iter19, Iter17) -> Iter21
lam_zip_135 (l0, l1) =
  Iter21_0 (Variant36_0 (l0, l1))

lam_wrapped_map_140 :: (Iter15, Closure34) -> Iter17
lam_wrapped_map_140 l0 =
  lam_map_130 l0

lam_wrapped_range_141 :: (Int64, Int64) -> Iter15
lam_wrapped_range_141 l0 =
  lam_range_127 l0

lam_items_139 :: (Vector Primality5)  -> Iter17
lam_items_139 l0 =
  lam_wrapped_map_140 (lam_wrapped_range_141 (0, intrinsicLen l0), Variant34_0 l0)

lam_wrapped_zip_143 :: (Iter19, Iter17) -> Iter21
lam_wrapped_zip_143 l0 =
  lam_zip_135 l0

lam_wrapped_ints_144 :: Int64 -> Iter19
lam_wrapped_ints_144 l0 =
  lam_ints_133 l0

lam_enumerate_142 :: Iter17 -> Iter21
lam_enumerate_142 l0 =
  lam_wrapped_zip_143 (lam_wrapped_ints_144 0, l0)

lam_writeln_146 :: (Vector Word8)  -> ()
lam_writeln_146 l0 =
  let l_0 = output l0 in l_0 `seq` output ((V.fromList [10]))

lam_concat_from_150 :: ((Vector Word8) , (Vector Word8) , Int64) -> (Vector Word8) 
lam_concat_from_150 (l0, l1, l2) =
  case uncurry (==) (l2, intrinsicLen l1) of True -> l0; False -> lam_concat_from_150 (intrinsicPush l0 (intrinsicGet l1 l2), l1, uncurry (+) (l2, 1))

lam_wrapped_concat_from_149 :: ((Vector Word8) , (Vector Word8) , Int64) -> (Vector Word8) 
lam_wrapped_concat_from_149 l0 =
  lam_concat_from_150 l0

lam_concat_148 :: ((Vector Word8) , (Vector Word8) ) -> (Vector Word8) 
lam_concat_148 (l0, l1) =
  lam_wrapped_concat_from_149 (l0, l1, 0)

lam_nat_to_string_153 :: Int64 -> (Vector Word8) 
lam_nat_to_string_153 l0 =
  case l0 of 0 -> (V.fromList [48]); 1 -> (V.fromList [49]); 2 -> (V.fromList [50]); 3 -> (V.fromList [51]); 4 -> (V.fromList [52]); 5 -> (V.fromList [53]); 6 -> (V.fromList [54]); 7 -> (V.fromList [55]); 8 -> (V.fromList [56]); 9 -> (V.fromList [57]); l_1 -> (V.fromList [])

lam_nat_to_string_152 :: Int64 -> (Vector Word8) 
lam_nat_to_string_152 l0 =
  let l1 = Variant114_0 () in (case uncurry (==) (l0, 0) of True -> (V.fromList []); False -> lam_concat_148 (lam_nat_to_string_152 (uncurry divv (l0, 10)), lam_nat_to_string_153 (uncurry (-) (l0, uncurry (*) (uncurry divv (l0, 10), 10)))))

lam_wrapped_nat_to_string_151 :: Int64 -> (Vector Word8) 
lam_wrapped_nat_to_string_151 l0 =
  lam_nat_to_string_152 l0

lam_int_to_string_147 :: Int64 -> (Vector Word8) 
lam_int_to_string_147 l0 =
  case uncurry (==) (l0, 0) of True -> (V.fromList [48]); False -> (case uncurry (<) (l0, 0) of True -> lam_concat_148 ((V.fromList [45]), lam_wrapped_nat_to_string_151 (uncurry (-) (0, l0))); False -> lam_wrapped_nat_to_string_151 l0)

lam_main_145 :: (Int64, Primality5) -> ()
lam_main_145 (l0, l1) =
  case l1 of (Prime5_0) -> lam_writeln_146 (lam_int_to_string_147 l0); (Composite5_1) -> ()

dispatch_158 :: (Closure25, Int64) -> Word8
dispatch_158 (l0, l1) =
  case l0 of (Variant25_0 l2) -> lam_items_76 (l2, l1)

dispatch_159 :: (Closure23, ()) -> Option2
dispatch_159 (l0, l1) =
  case l0 of (Variant23_0 l2) -> lam_range_75 (l2, l1)

lam_next_79 :: Iter1 -> Option2
lam_next_79 l0 =
  let (Iter1_0 l1) = l0 in dispatch_159 (l1, ())

lam_map_78 :: ((Iter1, Closure25), ()) -> Option4
lam_map_78 ((l0, l1), ()) =
  case lam_next_79 l0 of (Some2_0 (l2, l3)) -> Some4_0 (dispatch_158 (l1, l2), lam_map_77 (l3, l1)); (None2_1) -> None4_1

dispatch_160 :: (Closure24, ()) -> Option4
dispatch_160 (l0, l1) =
  case l0 of (Variant24_0 l2) -> lam_map_78 (l2, l1)

lam_next_81 :: Iter3 -> Option4
lam_next_81 l0 =
  let (Iter3_0 l1) = l0 in dispatch_160 (l1, ())

dispatch_161 :: (Closure108, Int64) -> Int64
dispatch_161 (l0, l1) =
  case l0 of (Variant108_0 l2) -> lam_chars_to_nat_84 (l2, l1)

lam_map_85 :: (Option0, Closure108) -> Option0
lam_map_85 (l0, l1) =
  case l0 of (Some0_0 l2) -> Some0_0 (dispatch_161 (l1, l2)); (None0_1) -> None0_1

lam_chars_to_nat_83 :: (Word8, Int64) -> Option0
lam_chars_to_nat_83 (l0, l1) =
  lam_map_85 (lam_digit_to_nat_86 l0, Variant108_0 l1)

dispatch_162 :: (Closure107, Int64) -> Option0
dispatch_162 (l0, l1) =
  case l0 of (Variant107_0 l2) -> lam_chars_to_nat_83 (l2, l1)

lam_and_then_87 :: (Option0, Closure107) -> Option0
lam_and_then_87 (l0, l1) =
  case l0 of (Some0_0 l2) -> dispatch_162 (l1, l2); (None0_1) -> None0_1

lam_chars_to_nat_82 :: (Option0, Word8) -> Option0
lam_chars_to_nat_82 (l0, l1) =
  lam_and_then_87 (l0, Variant107_0 l1)

lam_foldl_89 :: (Iter3, Option0, Closure106) -> Option0
lam_foldl_89 (l0, l1, l2) =
  case lam_next_81 l0 of (Some4_0 (l3, l4)) -> lam_foldl_89 (l4, lam_chars_to_nat_82 (l1, l3), l2); (None4_1) -> l1

lam_wrapped_foldl_88 :: (Iter3, Option0, Closure106) -> Option0
lam_wrapped_foldl_88 l0 =
  lam_foldl_89 l0

lam_chars_to_nat_80 :: Iter3 -> Option0
lam_chars_to_nat_80 l0 =
  case lam_next_81 l0 of (None4_1) -> None0_1; (Some4_0 l_0) -> lam_wrapped_foldl_88 (l0, Some0_0 0, Variant106_0 ())

lam_string_to_nat_73 :: (Vector Word8)  -> Option0
lam_string_to_nat_73 l0 =
  lam_chars_to_nat_80 (lam_items_90 l0)

dispatch_163 :: (Closure110, ()) -> Primality5
dispatch_163 (l0, l1) =
  case l0 of (Variant110_0 l2) -> lam_fill_96 (l2, l1)

lam_fill_with_rec_99 :: ((Vector Primality5) , Int64, Closure110) -> (Vector Primality5) 
lam_fill_with_rec_99 (l0, l1, l2) =
  case uncurry (>) (l1, 0) of True -> lam_fill_with_rec_99 (intrinsicPush l0 (dispatch_163 (l2, ())), uncurry (-) (l1, 1), l2); False -> l0

lam_wrapped_fill_with_rec_98 :: ((Vector Primality5) , Int64, Closure110) -> (Vector Primality5) 
lam_wrapped_fill_with_rec_98 l0 =
  lam_fill_with_rec_99 l0

lam_fill_with_97 :: (Int64, Closure110) -> (Vector Primality5) 
lam_fill_with_97 (l0, l1) =
  lam_wrapped_fill_with_rec_98 (intrinsicReserve ((V.fromList [])) l0, l0, l1)

lam_fill_95 :: (Int64, Primality5) -> (Vector Primality5) 
lam_fill_95 (l0, l1) =
  lam_fill_with_97 (l0, Variant110_0 l1)

dispatch_164 :: (Closure112, ((Vector Primality5) , Int64)) -> (Primality5, Closure111)
dispatch_164 (l0, l1) =
  case l0 of (Variant112_0) -> (let (l2, l3) = l1 in (let (l4, l5) = intrinsicExtract l2 l3 in (l4, Variant111_0 l5)))

dispatch_165 :: (Closure111, Primality5) -> (Vector Primality5) 
dispatch_165 (l0, l1) =
  case l0 of (Variant111_0 l2) -> intrinsicReplace l2 l1

lam_set_100 :: ((Vector Primality5) , Int64, Primality5) -> (Vector Primality5) 
lam_set_100 (l0, l1, l2) =
  let (l_0, l3) = dispatch_164 (Variant112_0, (l0, l1)) in l_0 `seq` dispatch_165 (l3, l2)

lam_sieve_118 :: ((Vector Primality5) , Int64) -> (Vector Primality5) 
lam_sieve_118 (l0, l1) =
  lam_set_100 (l0, l1, Composite5_1)

dispatch_166 :: (Closure29, Int64) -> Int64
dispatch_166 (l0, l1) =
  case l0 of (Variant29_0 l2) -> lam_sieve_107 (l2, l1)

dispatch_167 :: (Closure27, ()) -> Option9
dispatch_167 (l0, l1) =
  case l0 of (Variant27_0 l2) -> lam_ints_106 (l2, l1)

lam_next_110 :: Iter8 -> Option9
lam_next_110 l0 =
  let (Iter8_0 l1) = l0 in dispatch_167 (l1, ())

lam_map_109 :: ((Iter8, Closure29), ()) -> Option11
lam_map_109 ((l0, l1), ()) =
  case lam_next_110 l0 of (Some9_0 (l2, l3)) -> Some11_0 (dispatch_166 (l1, l2), lam_map_108 (l3, l1)); (None9_1) -> None11_1

dispatch_168 :: (Closure31, Int64) -> Bool
dispatch_168 (l0, l1) =
  case l0 of (Variant31_0 l2) -> lam_sieve_111 (l2, l1)

dispatch_169 :: (Closure28, ()) -> Option11
dispatch_169 (l0, l1) =
  case l0 of (Variant28_0 l2) -> lam_map_109 (l2, l1)

lam_next_114 :: Iter10 -> Option11
lam_next_114 l0 =
  let (Iter10_0 l1) = l0 in dispatch_169 (l1, ())

lam_take_while_113 :: ((Iter10, Closure31), ()) -> Option13
lam_take_while_113 ((l0, l1), ()) =
  case lam_next_114 l0 of (Some11_0 (l2, l3)) -> (case dispatch_168 (l1, l2) of True -> Some13_0 (l2, lam_take_while_112 (l3, l1)); False -> None13_1); (None11_1) -> None13_1

dispatch_170 :: (Closure30, ()) -> Option13
dispatch_170 (l0, l1) =
  case l0 of (Variant30_0 l2) -> lam_take_while_113 (l2, l1)

lam_next_121 :: Iter12 -> Option13
lam_next_121 l0 =
  let (Iter12_0 l1) = l0 in dispatch_170 (l1, ())

lam_foldl_120 :: (Iter12, (Vector Primality5) , Closure113) -> (Vector Primality5) 
lam_foldl_120 (l0, l1, l2) =
  case lam_next_121 l0 of (Some13_0 (l3, l4)) -> lam_foldl_120 (l4, lam_sieve_118 (l1, l3), l2); (None13_1) -> l1

lam_wrapped_foldl_119 :: (Iter12, (Vector Primality5) , Closure113) -> (Vector Primality5) 
lam_wrapped_foldl_119 l0 =
  lam_foldl_120 l0

lam_sieve_104 :: (Int64, ((Vector Primality5) , Int64)) -> (Vector Primality5) 
lam_sieve_104 (l0, (l1, l2)) =
  case intrinsicGet l1 l2 of (Prime5_0) -> (let l3 = (let l3 = (let l3 = lam_wrapped_ints_115 2 in lam_wrapped_map_116 (l3, Variant29_0 l2)) in lam_wrapped_take_while_117 (l3, Variant31_0 l0)) in lam_wrapped_foldl_119 (l3, l1, Variant113_0 ())); (Composite5_1) -> l1

dispatch_171 :: (Closure109, ((Vector Primality5) , Int64)) -> (Vector Primality5) 
dispatch_171 (l0, l1) =
  case l0 of (Variant109_0 l2) -> lam_sieve_104 (l2, l1)

dispatch_172 :: (Closure26, ()) -> Option7
dispatch_172 (l0, l1) =
  case l0 of (Variant26_0 l2) -> lam_range_102 (l2, l1)

lam_next_124 :: Iter6 -> Option7
lam_next_124 l0 =
  let (Iter6_0 l1) = l0 in dispatch_172 (l1, ())

lam_foldl_123 :: (Iter6, (Vector Primality5) , Closure109) -> (Vector Primality5) 
lam_foldl_123 (l0, l1, l2) =
  case lam_next_124 l0 of (Some7_0 (l3, l4)) -> lam_foldl_123 (l4, dispatch_171 (l2, (l1, l3)), l2); (None7_1) -> l1

lam_wrapped_foldl_122 :: (Iter6, (Vector Primality5) , Closure109) -> (Vector Primality5) 
lam_wrapped_foldl_122 l0 =
  lam_foldl_123 l0

lam_sieve_94 :: Int64 -> (Vector Primality5) 
lam_sieve_94 l0 =
  let l1 = (let l1 = (let l1 = lam_fill_95 (l0, Prime5_0) in lam_set_100 (l1, 0, Composite5_1)) in lam_set_100 (l1, 1, Composite5_1)) in (let l2 = lam_wrapped_range_103 (2, l0) in lam_wrapped_foldl_122 (l2, l1, Variant109_0 l0))

lam_main_93 :: (Int64, ()) -> (Vector Primality5) 
lam_main_93 (l0, ()) =
  lam_sieve_94 l0

dispatch_173 :: (Closure104, ()) -> (Vector Primality5) 
dispatch_173 (l0, l1) =
  case l0 of (Variant104_0 l2) -> lam_main_93 (l2, l1)

lam_repeat_126 :: (Int64, Closure104) -> Option14
lam_repeat_126 (l0, l1) =
  case uncurry (<) (l0, 1) of True -> None14_1; False -> (let l2 = dispatch_173 (l1, ()) in (case uncurry (==) (l0, 1) of True -> Some14_0 l2; False -> lam_repeat_126 (uncurry (-) (l0, 1), l1)))

lam_wrapped_repeat_125 :: (Int64, Closure104) -> Option14
lam_wrapped_repeat_125 l0 =
  lam_repeat_126 l0

dispatch_174 :: (Closure34, Int64) -> Primality5
dispatch_174 (l0, l1) =
  case l0 of (Variant34_0 l2) -> lam_items_129 (l2, l1)

dispatch_175 :: (Closure32, ()) -> Option16
dispatch_175 (l0, l1) =
  case l0 of (Variant32_0 l2) -> lam_range_128 (l2, l1)

lam_next_132 :: Iter15 -> Option16
lam_next_132 l0 =
  let (Iter15_0 l1) = l0 in dispatch_175 (l1, ())

lam_map_131 :: ((Iter15, Closure34), ()) -> Option18
lam_map_131 ((l0, l1), ()) =
  case lam_next_132 l0 of (Some16_0 (l2, l3)) -> Some18_0 (dispatch_174 (l1, l2), lam_map_130 (l3, l1)); (None16_1) -> None18_1

dispatch_176 :: (Closure35, ()) -> Option20
dispatch_176 (l0, l1) =
  case l0 of (Variant35_0 l2) -> lam_ints_134 (l2, l1)

lam_next_137 :: Iter19 -> Option20
lam_next_137 l0 =
  let (Iter19_0 l1) = l0 in dispatch_176 (l1, ())

dispatch_177 :: (Closure33, ()) -> Option18
dispatch_177 (l0, l1) =
  case l0 of (Variant33_0 l2) -> lam_map_131 (l2, l1)

lam_next_138 :: Iter17 -> Option18
lam_next_138 l0 =
  let (Iter17_0 l1) = l0 in dispatch_177 (l1, ())

lam_zip_136 :: ((Iter19, Iter17), ()) -> Option22
lam_zip_136 ((l0, l1), ()) =
  case (lam_next_137 l0, lam_next_138 l1) of ((Some20_0 (l2, l3)), (Some18_0 (l4, l5))) -> Some22_0 ((l2, l4), lam_zip_135 (l3, l5)); l_0 -> None22_1

dispatch_178 :: (Closure36, ()) -> Option22
dispatch_178 (l0, l1) =
  case l0 of (Variant36_0 l2) -> lam_zip_136 (l2, l1)

lam_next_156 :: Iter21 -> Option22
lam_next_156 l0 =
  let (Iter21_0 l1) = l0 in dispatch_178 (l1, ())

lam_for_each_155 :: (Iter21, Closure105) -> ()
lam_for_each_155 (l0, l1) =
  case lam_next_156 l0 of (Some22_0 (l2, l3)) -> (let l_0 = lam_main_145 l2 in l_0 `seq` lam_for_each_155 (l3, l1)); (None22_1) -> ()

lam_wrapped_for_each_154 :: (Iter21, Closure105) -> ()
lam_wrapped_for_each_154 l0 =
  lam_for_each_155 l0

lam_main_72 :: () -> ()
lam_main_72 () =
  case (lam_string_to_nat_73 (input ()), lam_string_to_nat_73 (input ())) of ((Some0_0 l0), (Some0_0 l1)) -> (case lam_wrapped_repeat_125 (l0, Variant104_0 l1) of (Some14_0 l2) -> (let l3 = (let l3 = lam_items_139 l2 in lam_enumerate_142 l3) in lam_wrapped_for_each_154 (l3, Variant105_0 ())); (None14_1) -> ()); (l_0, l_1) -> lam_writeln_146 ((V.fromList [80, 108, 101, 97, 115, 101, 32, 101, 110, 116, 101, 114, 32, 116, 119, 111, 32, 112, 111, 115, 105, 116, 105, 118, 101, 32, 105, 110, 116, 101, 103, 101, 114, 115, 32, 40, 97, 110, 32, 105, 116, 101, 114, 97, 116, 105, 111, 110, 32, 99, 111, 117, 110, 116, 32, 97, 110, 100, 32, 97, 32, 108, 105, 109, 105, 116, 41]))

dispatch_179 :: (Closure103, ()) -> ()
dispatch_179 (l0, l1) =
  case l0 of (Variant103_0 l2) -> lam_main_72 l1

main_wrapper_157 :: () -> ()
main_wrapper_157 () =
  dispatch_179 (main_71 (), ())


main :: IO ()
main = main_wrapper_157 () `seq` return ()
