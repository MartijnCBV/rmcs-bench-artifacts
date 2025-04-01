
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

data Closure13
  = Variant13_0 (Int64, Int64)

data Iter1
  = Iter1_0 Closure13

data Option2
  = Some2_0 (Int64, Iter1)
  | None2_1

data Closure15
  = Variant15_0 (Vector Word8) 

data Closure14
  = Variant14_0 (Iter1, Closure15)

data Iter3
  = Iter3_0 Closure14

data Option4
  = Some4_0 (Word8, Iter3)
  | None4_1

data Closure16
  = Variant16_0 (Int64, Int64)

data Iter5
  = Iter5_0 Closure16

data Option6
  = Some6_0 (Int64, Iter5)
  | None6_1

data Closure17
  = Variant17_0 (Int64, Int64)

data Iter7
  = Iter7_0 Closure17

data Option8
  = Some8_0 (Int64, Iter7)
  | None8_1

data Closure19
  = Variant19_0 (Vector Int64) 

data Closure18
  = Variant18_0 (Iter7, Closure19)

data Iter9
  = Iter9_0 Closure18

data Option10
  = Some10_0 (Int64, Iter9)
  | None10_1

data Closure20
  = Variant20_0 (Iter9, Int64)
  | Variant20_1 ()

data Iter11
  = Iter11_0 Closure20

data Option12
  = Some12_0 (Int64, Iter11)
  | None12_1

data Closure21
  = Variant21_0 ()

data Closure22
  = Variant22_0 ()

data Closure23
  = Variant23_0 ()

data Closure24
  = Variant24_0 ()

data Closure25
  = Variant25_0 ()

data Closure26
  = Variant26_0 ()

data Closure27
  = Variant27_0 ()

data Closure28
  = Variant28_0 ()

data Closure29
  = Variant29_0 ()

data Closure30
  = Variant30_0 ()

data Closure31
  = Variant31_0 ()

data Closure32
  = Variant32_0 ()

data Closure33
  = Variant33_0 ()

data Closure34
  = Variant34_0 ()

data Closure35
  = Variant35_0 ()

data Closure36
  = Variant36_0 ()

data Closure37
  = Variant37_0 ()

data Closure38
  = Variant38_0

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
  = Variant52_0 ()

data Closure53
  = Variant53_0 ()

data Closure54
  = Variant54_0 ()

data Closure55
  = Variant55_0 ()

data Closure56
  = Variant56_0 ()

data Closure57
  = Variant57_0

data Closure58
  = Variant58_0 ()

data Closure59
  = Variant59_0 ()

data Closure60
  = Variant60_0 ()

data Closure61
  = Variant61_0

data Closure62
  = Variant62_0 ()

data Closure63
  = Variant63_0

data Closure64
  = Variant64_0

data Closure65
  = Variant65_0 ()

data Closure66
  = Variant66_0 ()

data Closure67
  = Variant67_0 ()

data Closure68
  = Variant68_0 ()

data Closure69
  = Variant69_0

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
  = Variant86_0 (Vector Int64) 

data Closure87
  = Variant87_0 ()

data Closure88
  = Variant88_0 Word8

data Closure89
  = Variant89_0 Int64

data Closure90
  = Variant90_0 (Int64, Int64)

data Closure91
  = Variant91_0 (Int64 -> Vector Int64)

data Closure92
  = Variant92_0

data Closure93
  = Variant93_0 ()

data Closure94
  = Variant94_0 ()

data Closure95
  = Variant95_0 Closure83

range_0 :: () -> Closure21
range_0 () =
  Variant21_0 ()

next_1 :: () -> Closure22
next_1 () =
  Variant22_0 ()

map_2 :: () -> Closure23
map_2 () =
  Variant23_0 ()

next_3 :: () -> Closure24
next_3 () =
  Variant24_0 ()

map_4 :: () -> Closure25
map_4 () =
  Variant25_0 ()

digit_to_nat_5 :: () -> Closure26
digit_to_nat_5 () =
  Variant26_0 ()

and_then_6 :: () -> Closure27
and_then_6 () =
  Variant27_0 ()

foldl_7 :: () -> Closure28
foldl_7 () =
  Variant28_0 ()

wrapped_foldl_8 :: () -> Closure29
wrapped_foldl_8 () =
  Variant29_0 ()

chars_to_nat_9 :: () -> Closure30
chars_to_nat_9 () =
  Variant30_0 ()

wrapped_map_10 :: () -> Closure31
wrapped_map_10 () =
  Variant31_0 ()

wrapped_range_11 :: () -> Closure32
wrapped_range_11 () =
  Variant32_0 ()

items_12 :: () -> Closure33
items_12 () =
  Variant33_0 ()

string_to_nat_13 :: () -> Closure34
string_to_nat_13 () =
  Variant34_0 ()

input_ints_help_14 :: () -> Closure35
input_ints_help_14 () =
  Variant35_0 ()

wrapped_input_ints_help_15 :: () -> Closure36
wrapped_input_ints_help_15 () =
  Variant36_0 ()

input_ints_16 :: () -> Closure37
input_ints_16 () =
  Variant37_0 ()

get__18 :: () -> Closure38
get__18 () =
  Variant38_0

get_17 :: () -> Closure38
get_17 () =
  get__18 ()

set_19 :: () -> Closure39
set_19 () =
  Variant39_0 ()

swap_20 :: () -> Closure40
swap_20 () =
  Variant40_0 ()

range_21 :: () -> Closure41
range_21 () =
  Variant41_0 ()

wrapped_range_22 :: () -> Closure42
wrapped_range_22 () =
  Variant42_0 ()

next_23 :: () -> Closure43
next_23 () =
  Variant43_0 ()

foldl_24 :: () -> Closure44
foldl_24 () =
  Variant44_0 ()

wrapped_foldl_25 :: () -> Closure45
wrapped_foldl_25 () =
  Variant45_0 ()

for_26 :: () -> Closure46
for_26 () =
  Variant46_0 ()

partition_27 :: () -> Closure47
partition_27 () =
  Variant47_0 ()

quicksort_help_28 :: () -> Closure48
quicksort_help_28 () =
  Variant48_0 ()

wrapped_quicksort_help_29 :: () -> Closure49
wrapped_quicksort_help_29 () =
  Variant49_0 ()

quicksort_30 :: () -> Closure50
quicksort_30 () =
  Variant50_0 ()

copy_contents_from_start_31 :: () -> Closure51
copy_contents_from_start_31 () =
  Variant51_0 ()

wrapped_copy_contents_from_start_32 :: () -> Closure52
wrapped_copy_contents_from_start_32 () =
  Variant52_0 ()

copy_33 :: () -> Closure53
copy_33 () =
  Variant53_0 ()

feedforward_repeat_34 :: () -> Closure54
feedforward_repeat_34 () =
  Variant54_0 ()

wrapped_feedforward_repeat_35 :: () -> Closure55
wrapped_feedforward_repeat_35 () =
  Variant55_0 ()

fill_with_rec_36 :: () -> Closure56
fill_with_rec_36 () =
  Variant56_0 ()

push__38 :: () -> Closure57
push__38 () =
  Variant57_0

push_37 :: () -> Closure57
push_37 () =
  push__38 ()

wrapped_fill_with_rec_39 :: () -> Closure58
wrapped_fill_with_rec_39 () =
  Variant58_0 ()

fill_with_40 :: () -> Closure59
fill_with_40 () =
  Variant59_0 ()

clone_41 :: () -> Closure60
clone_41 () =
  Variant60_0 ()

len__43 :: () -> Closure61
len__43 () =
  Variant61_0

len_42 :: () -> Closure61
len_42 () =
  len__43 ()

concat_from_44 :: () -> Closure62
concat_from_44 () =
  Variant62_0 ()

push__46 :: () -> Closure63
push__46 () =
  Variant63_0

push_45 :: () -> Closure63
push_45 () =
  push__46 ()

get__48 :: () -> Closure64
get__48 () =
  Variant64_0

get_47 :: () -> Closure64
get_47 () =
  get__48 ()

wrapped_concat_from_49 :: () -> Closure65
wrapped_concat_from_49 () =
  Variant65_0 ()

concat_50 :: () -> Closure66
concat_50 () =
  Variant66_0 ()

nat_to_string_51 :: () -> Closure67
nat_to_string_51 () =
  Variant67_0 ()

wrapped_nat_to_string_52 :: () -> Closure68
wrapped_nat_to_string_52 () =
  Variant68_0 ()

len__54 :: () -> Closure69
len__54 () =
  Variant69_0

len_53 :: () -> Closure69
len_53 () =
  len__54 ()

range_55 :: () -> Closure70
range_55 () =
  Variant70_0 ()

next_56 :: () -> Closure71
next_56 () =
  Variant71_0 ()

map_57 :: () -> Closure72
map_57 () =
  Variant72_0 ()

next_58 :: () -> Closure73
next_58 () =
  Variant73_0 ()

take_59 :: () -> Closure74
take_59 () =
  Variant74_0 ()

empty_60 :: () -> Iter11
empty_60 () =
  Iter11_0 (Variant20_1 ())

wrapped_map_61 :: () -> Closure75
wrapped_map_61 () =
  Variant75_0 ()

wrapped_range_62 :: () -> Closure76
wrapped_range_62 () =
  Variant76_0 ()

items_63 :: () -> Closure77
items_63 () =
  Variant77_0 ()

wrapped_take_64 :: () -> Closure78
wrapped_take_64 () =
  Variant78_0 ()

next_65 :: () -> Closure79
next_65 () =
  Variant79_0 ()

foldl_66 :: () -> Closure80
foldl_66 () =
  Variant80_0 ()

wrapped_foldl_67 :: () -> Closure81
wrapped_foldl_67 () =
  Variant81_0 ()

str_68 :: () -> Closure82
str_68 () =
  Variant82_0 ()

int_to_string_69 :: () -> Closure83
int_to_string_69 () =
  Variant83_0 ()

writeln_70 :: () -> Closure84
writeln_70 () =
  Variant84_0 ()

main_71 :: () -> Closure85
main_71 () =
  Variant85_0 ()

lam_range_74 :: (Int64, Int64) -> Iter1
lam_range_74 (l0, l1) =
  Iter1_0 (Variant13_0 (l0, l1))

lam_range_75 :: ((Int64, Int64), ()) -> Option2
lam_range_75 ((l0, l1), ()) =
  case uncurry (<) (l0, l1) of True -> Some2_0 (l0, lam_range_74 (uncurry (+) (l0, 1), l1)); False -> None2_1

lam_items_76 :: ((Vector Word8) , Int64) -> Word8
lam_items_76 (l0, l1) =
  intrinsicGet l0 l1

lam_map_77 :: (Iter1, Closure15) -> Iter3
lam_map_77 (l0, l1) =
  Iter3_0 (Variant14_0 (l0, l1))

lam_chars_to_nat_84 :: (Int64, Int64) -> Int64
lam_chars_to_nat_84 (l0, l1) =
  uncurry (+) (uncurry (*) (l0, 10), l1)

lam_digit_to_nat_86 :: Word8 -> Option0
lam_digit_to_nat_86 l0 =
  case l0 of 48 -> Some0_0 0; 49 -> Some0_0 1; 50 -> Some0_0 2; 51 -> Some0_0 3; 52 -> Some0_0 4; 53 -> Some0_0 5; 54 -> Some0_0 6; 55 -> Some0_0 7; 56 -> Some0_0 8; 57 -> Some0_0 9; l_0 -> None0_1

lam_wrapped_map_91 :: (Iter1, Closure15) -> Iter3
lam_wrapped_map_91 l0 =
  lam_map_77 l0

lam_wrapped_range_92 :: (Int64, Int64) -> Iter1
lam_wrapped_range_92 l0 =
  lam_range_74 l0

lam_items_90 :: (Vector Word8)  -> Iter3
lam_items_90 l0 =
  lam_wrapped_map_91 (lam_wrapped_range_92 (0, intrinsicLen l0), Variant15_0 l0)

lam_range_105 :: (Int64, Int64) -> Iter5
lam_range_105 (l0, l1) =
  Iter5_0 (Variant16_0 (l0, l1))

lam_range_106 :: ((Int64, Int64), ()) -> Option6
lam_range_106 ((l0, l1), ()) =
  case uncurry (<) (l0, l1) of True -> Some6_0 (l0, lam_range_105 (uncurry (+) (l0, 1), l1)); False -> None6_1

lam_wrapped_range_107 :: (Int64, Int64) -> Iter5
lam_wrapped_range_107 l0 =
  lam_range_105 l0

lam_clone_117 :: () -> Int64
lam_clone_117 () =
  0

lam_fill_with_rec_120 :: ((Vector Int64) , Int64, Closure93) -> (Vector Int64) 
lam_fill_with_rec_120 (l0, l1, l2) =
  case uncurry (>) (l1, 0) of True -> lam_fill_with_rec_120 (intrinsicPush l0 (lam_clone_117 ()), uncurry (-) (l1, 1), l2); False -> l0

lam_wrapped_fill_with_rec_119 :: ((Vector Int64) , Int64, Closure93) -> (Vector Int64) 
lam_wrapped_fill_with_rec_119 l0 =
  lam_fill_with_rec_120 l0

lam_fill_with_118 :: (Int64, Closure93) -> (Vector Int64) 
lam_fill_with_118 (l0, l1) =
  lam_wrapped_fill_with_rec_119 (intrinsicReserve ((V.fromList [])) l0, l0, l1)

lam_concat_from_124 :: ((Vector Word8) , (Vector Word8) , Int64) -> (Vector Word8) 
lam_concat_from_124 (l0, l1, l2) =
  case uncurry (==) (l2, intrinsicLen l1) of True -> l0; False -> lam_concat_from_124 (intrinsicPush l0 (intrinsicGet l1 l2), l1, uncurry (+) (l2, 1))

lam_wrapped_concat_from_123 :: ((Vector Word8) , (Vector Word8) , Int64) -> (Vector Word8) 
lam_wrapped_concat_from_123 l0 =
  lam_concat_from_124 l0

lam_concat_122 :: ((Vector Word8) , (Vector Word8) ) -> (Vector Word8) 
lam_concat_122 (l0, l1) =
  lam_wrapped_concat_from_123 (l0, l1, 0)

lam_nat_to_string_127 :: Int64 -> (Vector Word8) 
lam_nat_to_string_127 l0 =
  case l0 of 0 -> (V.fromList [48]); 1 -> (V.fromList [49]); 2 -> (V.fromList [50]); 3 -> (V.fromList [51]); 4 -> (V.fromList [52]); 5 -> (V.fromList [53]); 6 -> (V.fromList [54]); 7 -> (V.fromList [55]); 8 -> (V.fromList [56]); 9 -> (V.fromList [57]); l_1 -> (V.fromList [])

lam_nat_to_string_126 :: Int64 -> (Vector Word8) 
lam_nat_to_string_126 l0 =
  let l1 = Variant94_0 () in (case uncurry (==) (l0, 0) of True -> (V.fromList []); False -> lam_concat_122 (lam_nat_to_string_126 (uncurry divv (l0, 10)), lam_nat_to_string_127 (uncurry (-) (l0, uncurry (*) (uncurry divv (l0, 10), 10)))))

lam_wrapped_nat_to_string_125 :: Int64 -> (Vector Word8) 
lam_wrapped_nat_to_string_125 l0 =
  lam_nat_to_string_126 l0

lam_int_to_string_121 :: Int64 -> (Vector Word8) 
lam_int_to_string_121 l0 =
  case uncurry (==) (l0, 0) of True -> (V.fromList [48]); False -> (case uncurry (<) (l0, 0) of True -> lam_concat_122 ((V.fromList [45]), lam_wrapped_nat_to_string_125 (uncurry (-) (0, l0))); False -> lam_wrapped_nat_to_string_125 l0)

lam_range_129 :: (Int64, Int64) -> Iter7
lam_range_129 (l0, l1) =
  Iter7_0 (Variant17_0 (l0, l1))

lam_range_130 :: ((Int64, Int64), ()) -> Option8
lam_range_130 ((l0, l1), ()) =
  case uncurry (<) (l0, l1) of True -> Some8_0 (l0, lam_range_129 (uncurry (+) (l0, 1), l1)); False -> None8_1

lam_items_131 :: ((Vector Int64) , Int64) -> Int64
lam_items_131 (l0, l1) =
  intrinsicGet l0 l1

lam_map_132 :: (Iter7, Closure19) -> Iter9
lam_map_132 (l0, l1) =
  Iter9_0 (Variant18_0 (l0, l1))

lam_take_135 :: (Iter9, Int64) -> Iter11
lam_take_135 (l0, l1) =
  case uncurry (>) (l1, 0) of True -> Iter11_0 (Variant20_0 (l0, l1)); False -> empty_60 ()

lam_empty_138 :: () -> Option12
lam_empty_138 () =
  None12_1

lam_wrapped_map_140 :: (Iter7, Closure19) -> Iter9
lam_wrapped_map_140 l0 =
  lam_map_132 l0

lam_wrapped_range_141 :: (Int64, Int64) -> Iter7
lam_wrapped_range_141 l0 =
  lam_range_129 l0

lam_items_139 :: (Vector Int64)  -> Iter9
lam_items_139 l0 =
  lam_wrapped_map_140 (lam_wrapped_range_141 (0, intrinsicLen l0), Variant19_0 l0)

lam_wrapped_take_142 :: (Iter9, Int64) -> Iter11
lam_wrapped_take_142 l0 =
  lam_take_135 l0

lam_str_143 :: (Closure83, ((Vector Word8) , Int64)) -> (Vector Word8) 
lam_str_143 (l0, (l1, l2)) =
  let l3 = (let l3 = l1 in lam_concat_122 (l3, lam_int_to_string_121 l2)) in lam_concat_122 (l3, (V.fromList [44, 32]))

lam_writeln_147 :: (Vector Word8)  -> ()
lam_writeln_147 l0 =
  let l_0 = output l0 in l_0 `seq` output ((V.fromList [10]))

dispatch_149 :: (Closure15, Int64) -> Word8
dispatch_149 (l0, l1) =
  case l0 of (Variant15_0 l2) -> lam_items_76 (l2, l1)

dispatch_150 :: (Closure13, ()) -> Option2
dispatch_150 (l0, l1) =
  case l0 of (Variant13_0 l2) -> lam_range_75 (l2, l1)

lam_next_79 :: Iter1 -> Option2
lam_next_79 l0 =
  let (Iter1_0 l1) = l0 in dispatch_150 (l1, ())

lam_map_78 :: ((Iter1, Closure15), ()) -> Option4
lam_map_78 ((l0, l1), ()) =
  case lam_next_79 l0 of (Some2_0 (l2, l3)) -> Some4_0 (dispatch_149 (l1, l2), lam_map_77 (l3, l1)); (None2_1) -> None4_1

dispatch_151 :: (Closure14, ()) -> Option4
dispatch_151 (l0, l1) =
  case l0 of (Variant14_0 l2) -> lam_map_78 (l2, l1)

lam_next_81 :: Iter3 -> Option4
lam_next_81 l0 =
  let (Iter3_0 l1) = l0 in dispatch_151 (l1, ())

dispatch_152 :: (Closure89, Int64) -> Int64
dispatch_152 (l0, l1) =
  case l0 of (Variant89_0 l2) -> lam_chars_to_nat_84 (l2, l1)

lam_map_85 :: (Option0, Closure89) -> Option0
lam_map_85 (l0, l1) =
  case l0 of (Some0_0 l2) -> Some0_0 (dispatch_152 (l1, l2)); (None0_1) -> None0_1

lam_chars_to_nat_83 :: (Word8, Int64) -> Option0
lam_chars_to_nat_83 (l0, l1) =
  lam_map_85 (lam_digit_to_nat_86 l0, Variant89_0 l1)

dispatch_153 :: (Closure88, Int64) -> Option0
dispatch_153 (l0, l1) =
  case l0 of (Variant88_0 l2) -> lam_chars_to_nat_83 (l2, l1)

lam_and_then_87 :: (Option0, Closure88) -> Option0
lam_and_then_87 (l0, l1) =
  case l0 of (Some0_0 l2) -> dispatch_153 (l1, l2); (None0_1) -> None0_1

lam_chars_to_nat_82 :: (Option0, Word8) -> Option0
lam_chars_to_nat_82 (l0, l1) =
  lam_and_then_87 (l0, Variant88_0 l1)

lam_foldl_89 :: (Iter3, Option0, Closure87) -> Option0
lam_foldl_89 (l0, l1, l2) =
  case lam_next_81 l0 of (Some4_0 (l3, l4)) -> lam_foldl_89 (l4, lam_chars_to_nat_82 (l1, l3), l2); (None4_1) -> l1

lam_wrapped_foldl_88 :: (Iter3, Option0, Closure87) -> Option0
lam_wrapped_foldl_88 l0 =
  lam_foldl_89 l0

lam_chars_to_nat_80 :: Iter3 -> Option0
lam_chars_to_nat_80 l0 =
  case lam_next_81 l0 of (None4_1) -> None0_1; (Some4_0 l_0) -> lam_wrapped_foldl_88 (l0, Some0_0 0, Variant87_0 ())

lam_string_to_nat_73 :: (Vector Word8)  -> Option0
lam_string_to_nat_73 l0 =
  lam_chars_to_nat_80 (lam_items_90 l0)

lam_input_ints_help_95 :: (Vector Int64)  -> (Vector Int64) 
lam_input_ints_help_95 l0 =
  case lam_string_to_nat_73 (input ()) of (Some0_0 l1) -> (let l2 = intrinsicPush l0 l1 in lam_input_ints_help_95 l2); (None0_1) -> l0

lam_wrapped_input_ints_help_94 :: (Vector Int64)  -> (Vector Int64) 
lam_wrapped_input_ints_help_94 l0 =
  lam_input_ints_help_95 l0

lam_input_ints_93 :: () -> (Vector Int64) 
lam_input_ints_93 () =
  lam_wrapped_input_ints_help_94 ((V.fromList []))

dispatch_154 :: (Closure92, ((Vector Int64) , Int64)) -> (Int64, Closure91)
dispatch_154 (l0, l1) =
  case l0 of (Variant92_0) -> (let (l2, l3) = l1 in (let (l4, l5) = intrinsicExtract l2 l3 in (l4, Variant91_0 l5)))

dispatch_155 :: (Closure91, Int64) -> (Vector Int64) 
dispatch_155 (l0, l1) =
  case l0 of (Variant91_0 l2) -> intrinsicReplace l2 l1

lam_set_103 :: ((Vector Int64) , Int64, Int64) -> (Vector Int64) 
lam_set_103 (l0, l1, l2) =
  let (l_0, l3) = dispatch_154 (Variant92_0, (l0, l1)) in l_0 `seq` dispatch_155 (l3, l2)

lam_swap_102 :: ((Vector Int64) , Int64, Int64) -> (Vector Int64) 
lam_swap_102 (l0, l1, l2) =
  let l3 = intrinsicGet l0 l1 in (let l4 = (let l4 = l0 in lam_set_103 (l4, l1, intrinsicGet l0 l2)) in lam_set_103 (l4, l2, l3))

lam_partition_101 :: ((Int64, Int64), ((Int64, (Vector Int64) ), Int64)) -> (Int64, (Vector Int64) )
lam_partition_101 ((l0, l1), ((l2, l3), l4)) =
  case uncurry (==) (l4, l0) of True -> (l2, lam_swap_102 (l3, l2, l0)); False -> (case uncurry (<) (intrinsicGet l3 l4, l1) of True -> (uncurry (+) (l2, 1), lam_swap_102 (l3, l2, l4)); False -> (l2, l3))

lam_copy_contents_from_start_113 :: ((Vector Int64) , (Vector Int64) , Int64) -> (Vector Int64) 
lam_copy_contents_from_start_113 (l0, l1, l2) =
  case uncurry (>=) (l2, intrinsicLen l1) of True -> l0; False -> (let l3 = lam_set_103 (l0, l2, intrinsicGet l1 l2) in lam_copy_contents_from_start_113 (l3, l1, uncurry (+) (l2, 1)))

lam_wrapped_copy_contents_from_start_112 :: ((Vector Int64) , (Vector Int64) , Int64) -> (Vector Int64) 
lam_wrapped_copy_contents_from_start_112 l0 =
  lam_copy_contents_from_start_113 l0

lam_copy_111 :: ((Vector Int64) , (Vector Int64) ) -> (Vector Int64) 
lam_copy_111 (l0, l1) =
  lam_wrapped_copy_contents_from_start_112 (l0, l1, 0)

lam_clone_116 :: (Vector Int64)  -> (Vector Int64) 
lam_clone_116 l0 =
  let l1 = lam_fill_with_118 (intrinsicLen l0, Variant93_0 ()) in lam_copy_111 (l1, l0)

dispatch_156 :: (Closure90, ((Int64, (Vector Int64) ), Int64)) -> (Int64, (Vector Int64) )
dispatch_156 (l0, l1) =
  case l0 of (Variant90_0 l2) -> lam_partition_101 (l2, l1)

dispatch_157 :: (Closure16, ()) -> Option6
dispatch_157 (l0, l1) =
  case l0 of (Variant16_0 l2) -> lam_range_106 (l2, l1)

lam_next_110 :: Iter5 -> Option6
lam_next_110 l0 =
  let (Iter5_0 l1) = l0 in dispatch_157 (l1, ())

lam_foldl_109 :: (Iter5, (Int64, (Vector Int64) ), Closure90) -> (Int64, (Vector Int64) )
lam_foldl_109 (l0, l1, l2) =
  case lam_next_110 l0 of (Some6_0 (l3, l4)) -> lam_foldl_109 (l4, dispatch_156 (l2, (l1, l3)), l2); (None6_1) -> l1

lam_wrapped_foldl_108 :: (Iter5, (Int64, (Vector Int64) ), Closure90) -> (Int64, (Vector Int64) )
lam_wrapped_foldl_108 l0 =
  lam_foldl_109 l0

lam_for_104 :: (Int64, Int64, (Int64, (Vector Int64) ), Closure90) -> (Int64, (Vector Int64) )
lam_for_104 (l0, l1, l2, l3) =
  let l4 = lam_wrapped_range_107 (l0, l1) in lam_wrapped_foldl_108 (l4, l2, l3)

lam_partition_100 :: ((Vector Int64) , Int64, Int64) -> (Int64, (Vector Int64) )
lam_partition_100 (l0, l1, l2) =
  let l3 = intrinsicGet l0 l2 in lam_for_104 (l1, uncurry (+) (l2, 1), (l1, l0), Variant90_0 (l2, l3))

lam_quicksort_help_99 :: ((Vector Int64) , Int64, Int64) -> (Vector Int64) 
lam_quicksort_help_99 (l0, l1, l2) =
  case uncurry (>=) (l1, l2) of True -> l0; False -> (let (l3, l4) = lam_partition_100 (l0, l1, l2) in (let l5 = (let l5 = l4 in lam_quicksort_help_99 (l5, l1, uncurry (-) (l3, 1))) in lam_quicksort_help_99 (l5, uncurry (+) (l3, 1), l2)))

lam_wrapped_quicksort_help_98 :: ((Vector Int64) , Int64, Int64) -> (Vector Int64) 
lam_wrapped_quicksort_help_98 l0 =
  lam_quicksort_help_99 l0

lam_quicksort_97 :: (Vector Int64)  -> (Vector Int64) 
lam_quicksort_97 l0 =
  lam_wrapped_quicksort_help_98 (l0, 0, uncurry (-) (intrinsicLen l0, 1))

lam_main_96 :: ((Vector Int64) , (Vector Int64) ) -> (Vector Int64) 
lam_main_96 (l0, l1) =
  lam_quicksort_97 (lam_copy_111 (l1, l0))

dispatch_158 :: (Closure86, (Vector Int64) ) -> (Vector Int64) 
dispatch_158 (l0, l1) =
  case l0 of (Variant86_0 l2) -> lam_main_96 (l2, l1)

lam_feedforward_repeat_115 :: (Int64, (Vector Int64) , Closure86) -> (Vector Int64) 
lam_feedforward_repeat_115 (l0, l1, l2) =
  case uncurry (<=) (l0, 0) of True -> l1; False -> lam_feedforward_repeat_115 (uncurry (-) (l0, 1), dispatch_158 (l2, l1), l2)

lam_wrapped_feedforward_repeat_114 :: (Int64, (Vector Int64) , Closure86) -> (Vector Int64) 
lam_wrapped_feedforward_repeat_114 l0 =
  lam_feedforward_repeat_115 l0

dispatch_159 :: (Closure19, Int64) -> Int64
dispatch_159 (l0, l1) =
  case l0 of (Variant19_0 l2) -> lam_items_131 (l2, l1)

dispatch_160 :: (Closure17, ()) -> Option8
dispatch_160 (l0, l1) =
  case l0 of (Variant17_0 l2) -> lam_range_130 (l2, l1)

lam_next_134 :: Iter7 -> Option8
lam_next_134 l0 =
  let (Iter7_0 l1) = l0 in dispatch_160 (l1, ())

lam_map_133 :: ((Iter7, Closure19), ()) -> Option10
lam_map_133 ((l0, l1), ()) =
  case lam_next_134 l0 of (Some8_0 (l2, l3)) -> Some10_0 (dispatch_159 (l1, l2), lam_map_132 (l3, l1)); (None8_1) -> None10_1

dispatch_161 :: (Closure18, ()) -> Option10
dispatch_161 (l0, l1) =
  case l0 of (Variant18_0 l2) -> lam_map_133 (l2, l1)

lam_next_137 :: Iter9 -> Option10
lam_next_137 l0 =
  let (Iter9_0 l1) = l0 in dispatch_161 (l1, ())

lam_take_136 :: ((Iter9, Int64), ()) -> Option12
lam_take_136 ((l0, l1), ()) =
  case lam_next_137 l0 of (Some10_0 (l2, l3)) -> Some12_0 (l2, lam_take_135 (l3, uncurry (-) (l1, 1))); (None10_1) -> None12_1

dispatch_162 :: (Closure95, ((Vector Word8) , Int64)) -> (Vector Word8) 
dispatch_162 (l0, l1) =
  case l0 of (Variant95_0 l2) -> lam_str_143 (l2, l1)

dispatch_163 :: (Closure20, ()) -> Option12
dispatch_163 (l0, l1) =
  case l0 of (Variant20_0 l2) -> lam_take_136 (l2, l1); (Variant20_1 l2) -> lam_empty_138 l1

lam_next_146 :: Iter11 -> Option12
lam_next_146 l0 =
  let (Iter11_0 l1) = l0 in dispatch_163 (l1, ())

lam_foldl_145 :: (Iter11, (Vector Word8) , Closure95) -> (Vector Word8) 
lam_foldl_145 (l0, l1, l2) =
  case lam_next_146 l0 of (Some12_0 (l3, l4)) -> lam_foldl_145 (l4, dispatch_162 (l2, (l1, l3)), l2); (None12_1) -> l1

lam_wrapped_foldl_144 :: (Iter11, (Vector Word8) , Closure95) -> (Vector Word8) 
lam_wrapped_foldl_144 l0 =
  lam_foldl_145 l0

lam_str_128 :: ((Vector Int64) , Closure83) -> (Vector Word8) 
lam_str_128 (l0, l1) =
  let l2 = uncurry (-) (intrinsicLen l0, 1) in (let l3 = (let l3 = (let l3 = (let l3 = lam_items_139 l0 in lam_wrapped_take_142 (l3, l2)) in lam_wrapped_foldl_144 (l3, (V.fromList [91]), Variant95_0 l1)) in lam_concat_122 (l3, lam_int_to_string_121 (intrinsicGet l0 l2))) in lam_concat_122 (l3, (V.fromList [93])))

lam_main_72 :: () -> ()
lam_main_72 () =
  case lam_string_to_nat_73 (input ()) of (Some0_0 l0) -> (let l1 = input (); l2 = lam_input_ints_93 (); l3 = lam_wrapped_feedforward_repeat_114 (l0, lam_clone_116 l2, Variant86_0 l2); l_0 = output ((V.fromList [84, 104, 101, 32, 115, 111, 114, 116, 101, 100, 32, 118, 101, 114, 115, 105, 111, 110, 32, 111, 102, 10, 32, 32])); l_1 = output (lam_str_128 (l2, int_to_string_69 ())); l_2 = output ((V.fromList [10, 105, 115, 10, 32, 32])); l_3 = output (lam_str_128 (l3, int_to_string_69 ())) in l_0 `seq` l_1 `seq` l_2 `seq` l_3 `seq` output ((V.fromList [10]))); (None0_1) -> lam_writeln_147 ((V.fromList [80, 108, 101, 97, 115, 101, 32, 101, 110, 116, 101, 114, 32, 97, 110, 32, 105, 110, 116, 101, 103, 101, 114, 32, 40, 101, 120, 112, 114, 101, 115, 115, 105, 110, 103, 32, 105, 116, 101, 114, 97, 116, 105, 111, 110, 32, 99, 111, 117, 110, 116, 41, 32, 102, 111, 108, 108, 111, 119, 101, 100, 32, 98, 121, 32, 97, 32, 115, 101, 113, 117, 101, 110, 99, 101, 32, 111, 102, 32, 108, 105, 110, 101, 45, 115, 101, 112, 97, 114, 97, 116, 101, 100, 32, 105, 110, 116, 101, 103, 101, 114, 115, 32, 40, 97, 110, 32, 97, 114, 114, 97, 121, 41, 44, 32, 102, 111, 108, 108, 111, 119, 101, 100, 32, 98, 121, 32, 97, 110, 32, 101, 109, 112, 116, 121, 32, 108, 105, 110, 101]))

dispatch_164 :: (Closure85, ()) -> ()
dispatch_164 (l0, l1) =
  case l0 of (Variant85_0 l2) -> lam_main_72 l1

main_wrapper_148 :: () -> ()
main_wrapper_148 () =
  dispatch_164 (main_71 (), ())


main :: IO ()
main = main_wrapper_148 () `seq` return ()
