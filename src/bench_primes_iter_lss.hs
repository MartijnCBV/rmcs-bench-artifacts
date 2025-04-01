
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

data Closure15
  = Variant15_0 (Int64, Int64)

data Iter1
  = Iter1_0 Closure15

data Option2
  = Some2_0 (Int64, Iter1)
  | None2_1

data Closure17
  = Variant17_0 (Vector Word8) 

data Closure16
  = Variant16_0 (Iter1, Closure17)

data Iter3
  = Iter3_0 Closure16

data Option4
  = Some4_0 (Word8, Iter3)
  | None4_1

data Closure18
  = Variant18_0 (Int64, Int64)

data Iter5
  = Iter5_0 Closure18

data Option6
  = Some6_0 (Int64, Iter5)
  | None6_1

data Closure19
  = Variant19_0 Int64

data Iter7
  = Iter7_0 Closure19

data Option8
  = Some8_0 (Int64, Iter7)
  | None8_1

data Closure21
  = Variant21_0 Int64

data Closure20
  = Variant20_0 (Iter7, Closure21)

data Iter9
  = Iter9_0 Closure20

data Option10
  = Some10_0 (Int64, Iter9)
  | None10_1

data Closure23
  = Variant23_0 ()

data Closure22
  = Variant22_0 (Iter5, Closure23)

data Iter11
  = Iter11_0 Closure22

data Option12
  = Some12_0 (Int64, Iter11)
  | None12_1

data Closure25
  = Variant25_0 ()

data Closure24
  = Variant24_0 (Iter11, Closure25)

data Iter13
  = Iter13_0 Closure24

data Option14
  = Some14_0 (Int64, Iter13)
  | None14_1

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
  = Variant57_0 ()

data Closure58
  = Variant58_0 ()

data Closure59
  = Variant59_0 ()

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
  = Variant65_0

data Closure66
  = Variant66_0 ()

data Closure67
  = Variant67_0

data Closure68
  = Variant68_0

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
  = Variant76_0 Int64

data Closure77
  = Variant77_0 ()

data Closure78
  = Variant78_0 Word8

data Closure79
  = Variant79_0 Int64

data Closure80
  = Variant80_0 Int64

data Closure81
  = Variant81_0 ()

data Closure82
  = Variant82_0 ()

range_0 :: () -> Closure26
range_0 () =
  Variant26_0 ()

next_1 :: () -> Closure27
next_1 () =
  Variant27_0 ()

map_2 :: () -> Closure28
map_2 () =
  Variant28_0 ()

next_3 :: () -> Closure29
next_3 () =
  Variant29_0 ()

map_4 :: () -> Closure30
map_4 () =
  Variant30_0 ()

digit_to_nat_5 :: () -> Closure31
digit_to_nat_5 () =
  Variant31_0 ()

and_then_6 :: () -> Closure32
and_then_6 () =
  Variant32_0 ()

foldl_7 :: () -> Closure33
foldl_7 () =
  Variant33_0 ()

wrapped_foldl_8 :: () -> Closure34
wrapped_foldl_8 () =
  Variant34_0 ()

chars_to_nat_9 :: () -> Closure35
chars_to_nat_9 () =
  Variant35_0 ()

wrapped_map_10 :: () -> Closure36
wrapped_map_10 () =
  Variant36_0 ()

wrapped_range_11 :: () -> Closure37
wrapped_range_11 () =
  Variant37_0 ()

items_12 :: () -> Closure38
items_12 () =
  Variant38_0 ()

string_to_nat_13 :: () -> Closure39
string_to_nat_13 () =
  Variant39_0 ()

range_14 :: () -> Closure40
range_14 () =
  Variant40_0 ()

ints_15 :: () -> Closure41
ints_15 () =
  Variant41_0 ()

next_16 :: () -> Closure42
next_16 () =
  Variant42_0 ()

take_while_17 :: () -> Closure43
take_while_17 () =
  Variant43_0 ()

next_18 :: () -> Closure44
next_18 () =
  Variant44_0 ()

any_19 :: () -> Closure45
any_19 () =
  Variant45_0 ()

wrapped_any_20 :: () -> Closure46
wrapped_any_20 () =
  Variant46_0 ()

wrapped_take_while_21 :: () -> Closure47
wrapped_take_while_21 () =
  Variant47_0 ()

wrapped_ints_22 :: () -> Closure48
wrapped_ints_22 () =
  Variant48_0 ()

next_23 :: () -> Closure49
next_23 () =
  Variant49_0 ()

filter_24 :: () -> Closure50
filter_24 () =
  Variant50_0 ()

next_25 :: () -> Closure51
next_25 () =
  Variant51_0 ()

map_26 :: () -> Closure52
map_26 () =
  Variant52_0 ()

next_27 :: () -> Closure53
next_27 () =
  Variant53_0 ()

foldl_28 :: () -> Closure54
foldl_28 () =
  Variant54_0 ()

wrapped_foldl_29 :: () -> Closure55
wrapped_foldl_29 () =
  Variant55_0 ()

sum_30 :: () -> Closure56
sum_30 () =
  Variant56_0 ()

wrapped_map_31 :: () -> Closure57
wrapped_map_31 () =
  Variant57_0 ()

count_32 :: () -> Closure58
count_32 () =
  Variant58_0 ()

wrapped_filter_33 :: () -> Closure59
wrapped_filter_33 () =
  Variant59_0 ()

wrapped_range_34 :: () -> Closure60
wrapped_range_34 () =
  Variant60_0 ()

is_prime_35 :: () -> Closure23
is_prime_35 () =
  Variant23_0 ()

primes_to_36 :: () -> Closure61
primes_to_36 () =
  Variant61_0 ()

count_primes_37 :: () -> Closure62
count_primes_37 () =
  Variant62_0 ()

repeat_38 :: () -> Closure63
repeat_38 () =
  Variant63_0 ()

wrapped_repeat_39 :: () -> Closure64
wrapped_repeat_39 () =
  Variant64_0 ()

len__41 :: () -> Closure65
len__41 () =
  Variant65_0

len_40 :: () -> Closure65
len_40 () =
  len__41 ()

concat_from_42 :: () -> Closure66
concat_from_42 () =
  Variant66_0 ()

push__44 :: () -> Closure67
push__44 () =
  Variant67_0

push_43 :: () -> Closure67
push_43 () =
  push__44 ()

get__46 :: () -> Closure68
get__46 () =
  Variant68_0

get_45 :: () -> Closure68
get_45 () =
  get__46 ()

wrapped_concat_from_47 :: () -> Closure69
wrapped_concat_from_47 () =
  Variant69_0 ()

concat_48 :: () -> Closure70
concat_48 () =
  Variant70_0 ()

nat_to_string_49 :: () -> Closure71
nat_to_string_49 () =
  Variant71_0 ()

wrapped_nat_to_string_50 :: () -> Closure72
wrapped_nat_to_string_50 () =
  Variant72_0 ()

int_to_string_51 :: () -> Closure73
int_to_string_51 () =
  Variant73_0 ()

writeln_52 :: () -> Closure74
writeln_52 () =
  Variant74_0 ()

main_53 :: () -> Closure75
main_53 () =
  Variant75_0 ()

lam_range_56 :: (Int64, Int64) -> Iter1
lam_range_56 (l0, l1) =
  Iter1_0 (Variant15_0 (l0, l1))

lam_range_57 :: ((Int64, Int64), ()) -> Option2
lam_range_57 ((l0, l1), ()) =
  case uncurry (<) (l0, l1) of True -> Some2_0 (l0, lam_range_56 (uncurry (+) (l0, 1), l1)); False -> None2_1

lam_items_58 :: ((Vector Word8) , Int64) -> Word8
lam_items_58 (l0, l1) =
  intrinsicGet l0 l1

lam_map_59 :: (Iter1, Closure17) -> Iter3
lam_map_59 (l0, l1) =
  Iter3_0 (Variant16_0 (l0, l1))

lam_chars_to_nat_66 :: (Int64, Int64) -> Int64
lam_chars_to_nat_66 (l0, l1) =
  uncurry (+) (uncurry (*) (l0, 10), l1)

lam_digit_to_nat_68 :: Word8 -> Option0
lam_digit_to_nat_68 l0 =
  case l0 of 48 -> Some0_0 0; 49 -> Some0_0 1; 50 -> Some0_0 2; 51 -> Some0_0 3; 52 -> Some0_0 4; 53 -> Some0_0 5; 54 -> Some0_0 6; 55 -> Some0_0 7; 56 -> Some0_0 8; 57 -> Some0_0 9; l_0 -> None0_1

lam_wrapped_map_73 :: (Iter1, Closure17) -> Iter3
lam_wrapped_map_73 l0 =
  lam_map_59 l0

lam_wrapped_range_74 :: (Int64, Int64) -> Iter1
lam_wrapped_range_74 l0 =
  lam_range_56 l0

lam_items_72 :: (Vector Word8)  -> Iter3
lam_items_72 l0 =
  lam_wrapped_map_73 (lam_wrapped_range_74 (0, intrinsicLen l0), Variant17_0 l0)

lam_range_77 :: (Int64, Int64) -> Iter5
lam_range_77 (l0, l1) =
  Iter5_0 (Variant18_0 (l0, l1))

lam_range_78 :: ((Int64, Int64), ()) -> Option6
lam_range_78 ((l0, l1), ()) =
  case uncurry (<) (l0, l1) of True -> Some6_0 (l0, lam_range_77 (uncurry (+) (l0, 1), l1)); False -> None6_1

lam_ints_80 :: Int64 -> Iter7
lam_ints_80 l0 =
  Iter7_0 (Variant19_0 l0)

lam_ints_81 :: (Int64, ()) -> Option8
lam_ints_81 (l0, ()) =
  Some8_0 (l0, lam_ints_80 (uncurry (+) (l0, 1)))

lam_is_prime_82 :: (Int64, Int64) -> Bool
lam_is_prime_82 (l0, l1) =
  uncurry (<=) (uncurry (*) (l1, l1), l0)

lam_take_while_83 :: (Iter7, Closure21) -> Iter9
lam_take_while_83 (l0, l1) =
  Iter9_0 (Variant20_0 (l0, l1))

lam_is_prime_86 :: (Int64, Int64) -> Bool
lam_is_prime_86 (l0, l1) =
  uncurry (==) (uncurry (*) (uncurry divv (l0, l1), l1), l0)

lam_wrapped_take_while_90 :: (Iter7, Closure21) -> Iter9
lam_wrapped_take_while_90 l0 =
  lam_take_while_83 l0

lam_wrapped_ints_91 :: Int64 -> Iter7
lam_wrapped_ints_91 l0 =
  lam_ints_80 l0

lam_filter_92 :: (Iter5, Closure23) -> Iter11
lam_filter_92 (l0, l1) =
  Iter11_0 (Variant22_0 (l0, l1))

lam_count_97 :: Int64 -> Int64
lam_count_97 l_1 =
  1

lam_map_98 :: (Iter11, Closure25) -> Iter13
lam_map_98 (l0, l1) =
  Iter13_0 (Variant24_0 (l0, l1))

lam_sum_101 :: (Int64, Int64) -> Int64
lam_sum_101 (l0, l1) =
  uncurry (+) (l0, l1)

lam_wrapped_map_105 :: (Iter11, Closure25) -> Iter13
lam_wrapped_map_105 l0 =
  lam_map_98 l0

lam_wrapped_filter_107 :: (Iter5, Closure23) -> Iter11
lam_wrapped_filter_107 l0 =
  lam_filter_92 l0

lam_wrapped_range_108 :: (Int64, Int64) -> Iter5
lam_wrapped_range_108 l0 =
  lam_range_77 l0

lam_primes_to_106 :: Int64 -> Iter11
lam_primes_to_106 l0 =
  lam_wrapped_filter_107 (lam_wrapped_range_108 (2, uncurry (+) (l0, 1)), is_prime_35 ())

lam_concat_from_114 :: ((Vector Word8) , (Vector Word8) , Int64) -> (Vector Word8) 
lam_concat_from_114 (l0, l1, l2) =
  case uncurry (==) (l2, intrinsicLen l1) of True -> l0; False -> lam_concat_from_114 (intrinsicPush l0 (intrinsicGet l1 l2), l1, uncurry (+) (l2, 1))

lam_wrapped_concat_from_113 :: ((Vector Word8) , (Vector Word8) , Int64) -> (Vector Word8) 
lam_wrapped_concat_from_113 l0 =
  lam_concat_from_114 l0

lam_concat_112 :: ((Vector Word8) , (Vector Word8) ) -> (Vector Word8) 
lam_concat_112 (l0, l1) =
  lam_wrapped_concat_from_113 (l0, l1, 0)

lam_nat_to_string_117 :: Int64 -> (Vector Word8) 
lam_nat_to_string_117 l0 =
  case l0 of 0 -> (V.fromList [48]); 1 -> (V.fromList [49]); 2 -> (V.fromList [50]); 3 -> (V.fromList [51]); 4 -> (V.fromList [52]); 5 -> (V.fromList [53]); 6 -> (V.fromList [54]); 7 -> (V.fromList [55]); 8 -> (V.fromList [56]); 9 -> (V.fromList [57]); l_2 -> (V.fromList [])

lam_nat_to_string_116 :: Int64 -> (Vector Word8) 
lam_nat_to_string_116 l0 =
  let l1 = Variant82_0 () in (case uncurry (==) (l0, 0) of True -> (V.fromList []); False -> lam_concat_112 (lam_nat_to_string_116 (uncurry divv (l0, 10)), lam_nat_to_string_117 (uncurry (-) (l0, uncurry (*) (uncurry divv (l0, 10), 10)))))

lam_wrapped_nat_to_string_115 :: Int64 -> (Vector Word8) 
lam_wrapped_nat_to_string_115 l0 =
  lam_nat_to_string_116 l0

lam_int_to_string_111 :: Int64 -> (Vector Word8) 
lam_int_to_string_111 l0 =
  case uncurry (==) (l0, 0) of True -> (V.fromList [48]); False -> (case uncurry (<) (l0, 0) of True -> lam_concat_112 ((V.fromList [45]), lam_wrapped_nat_to_string_115 (uncurry (-) (0, l0))); False -> lam_wrapped_nat_to_string_115 l0)

lam_writeln_118 :: (Vector Word8)  -> ()
lam_writeln_118 l0 =
  let l_0 = output l0 in l_0 `seq` output ((V.fromList [10]))

dispatch_120 :: (Closure17, Int64) -> Word8
dispatch_120 (l0, l1) =
  case l0 of (Variant17_0 l2) -> lam_items_58 (l2, l1)

dispatch_121 :: (Closure15, ()) -> Option2
dispatch_121 (l0, l1) =
  case l0 of (Variant15_0 l2) -> lam_range_57 (l2, l1)

lam_next_61 :: Iter1 -> Option2
lam_next_61 l0 =
  let (Iter1_0 l1) = l0 in dispatch_121 (l1, ())

lam_map_60 :: ((Iter1, Closure17), ()) -> Option4
lam_map_60 ((l0, l1), ()) =
  case lam_next_61 l0 of (Some2_0 (l2, l3)) -> Some4_0 (dispatch_120 (l1, l2), lam_map_59 (l3, l1)); (None2_1) -> None4_1

dispatch_122 :: (Closure16, ()) -> Option4
dispatch_122 (l0, l1) =
  case l0 of (Variant16_0 l2) -> lam_map_60 (l2, l1)

lam_next_63 :: Iter3 -> Option4
lam_next_63 l0 =
  let (Iter3_0 l1) = l0 in dispatch_122 (l1, ())

dispatch_123 :: (Closure79, Int64) -> Int64
dispatch_123 (l0, l1) =
  case l0 of (Variant79_0 l2) -> lam_chars_to_nat_66 (l2, l1)

lam_map_67 :: (Option0, Closure79) -> Option0
lam_map_67 (l0, l1) =
  case l0 of (Some0_0 l2) -> Some0_0 (dispatch_123 (l1, l2)); (None0_1) -> None0_1

lam_chars_to_nat_65 :: (Word8, Int64) -> Option0
lam_chars_to_nat_65 (l0, l1) =
  lam_map_67 (lam_digit_to_nat_68 l0, Variant79_0 l1)

dispatch_124 :: (Closure78, Int64) -> Option0
dispatch_124 (l0, l1) =
  case l0 of (Variant78_0 l2) -> lam_chars_to_nat_65 (l2, l1)

lam_and_then_69 :: (Option0, Closure78) -> Option0
lam_and_then_69 (l0, l1) =
  case l0 of (Some0_0 l2) -> dispatch_124 (l1, l2); (None0_1) -> None0_1

lam_chars_to_nat_64 :: (Option0, Word8) -> Option0
lam_chars_to_nat_64 (l0, l1) =
  lam_and_then_69 (l0, Variant78_0 l1)

lam_foldl_71 :: (Iter3, Option0, Closure77) -> Option0
lam_foldl_71 (l0, l1, l2) =
  case lam_next_63 l0 of (Some4_0 (l3, l4)) -> lam_foldl_71 (l4, lam_chars_to_nat_64 (l1, l3), l2); (None4_1) -> l1

lam_wrapped_foldl_70 :: (Iter3, Option0, Closure77) -> Option0
lam_wrapped_foldl_70 l0 =
  lam_foldl_71 l0

lam_chars_to_nat_62 :: Iter3 -> Option0
lam_chars_to_nat_62 l0 =
  case lam_next_63 l0 of (None4_1) -> None0_1; (Some4_0 l_0) -> lam_wrapped_foldl_70 (l0, Some0_0 0, Variant77_0 ())

lam_string_to_nat_55 :: (Vector Word8)  -> Option0
lam_string_to_nat_55 l0 =
  lam_chars_to_nat_62 (lam_items_72 l0)

dispatch_125 :: (Closure21, Int64) -> Bool
dispatch_125 (l0, l1) =
  case l0 of (Variant21_0 l2) -> lam_is_prime_82 (l2, l1)

dispatch_126 :: (Closure19, ()) -> Option8
dispatch_126 (l0, l1) =
  case l0 of (Variant19_0 l2) -> lam_ints_81 (l2, l1)

lam_next_85 :: Iter7 -> Option8
lam_next_85 l0 =
  let (Iter7_0 l1) = l0 in dispatch_126 (l1, ())

lam_take_while_84 :: ((Iter7, Closure21), ()) -> Option10
lam_take_while_84 ((l0, l1), ()) =
  case lam_next_85 l0 of (Some8_0 (l2, l3)) -> (case dispatch_125 (l1, l2) of True -> Some10_0 (l2, lam_take_while_83 (l3, l1)); False -> None10_1); (None8_1) -> None10_1

dispatch_127 :: (Closure80, Int64) -> Bool
dispatch_127 (l0, l1) =
  case l0 of (Variant80_0 l2) -> lam_is_prime_86 (l2, l1)

dispatch_128 :: (Closure20, ()) -> Option10
dispatch_128 (l0, l1) =
  case l0 of (Variant20_0 l2) -> lam_take_while_84 (l2, l1)

lam_next_89 :: Iter9 -> Option10
lam_next_89 l0 =
  let (Iter9_0 l1) = l0 in dispatch_128 (l1, ())

lam_any_88 :: (Iter9, Closure80) -> Bool
lam_any_88 (l0, l1) =
  case lam_next_89 l0 of (Some10_0 (l2, l3)) -> (case dispatch_127 (l1, l2) of True -> True; False -> lam_any_88 (l3, l1)); (None10_1) -> False

lam_wrapped_any_87 :: (Iter9, Closure80) -> Bool
lam_wrapped_any_87 l0 =
  lam_any_88 l0

lam_is_prime_79 :: Int64 -> Bool
lam_is_prime_79 l0 =
  not (lam_wrapped_any_87 (lam_wrapped_take_while_90 (lam_wrapped_ints_91 2, Variant21_0 l0), Variant80_0 l0))

dispatch_129 :: (Closure18, ()) -> Option6
dispatch_129 (l0, l1) =
  case l0 of (Variant18_0 l2) -> lam_range_78 (l2, l1)

lam_next_94 :: Iter5 -> Option6
lam_next_94 l0 =
  let (Iter5_0 l1) = l0 in dispatch_129 (l1, ())

dispatch_130 :: (Closure22, ()) -> Option12
dispatch_130 (l0, l1) =
  case l0 of (Variant22_0 l2) -> lam_filter_93 (l2, l1)

lam_filter_93 ((l0, l1), ()) =
  case lam_next_94 l0 of (Some6_0 (l2, l3)) -> (case lam_is_prime_79 l2 of True -> Some12_0 (l2, lam_filter_92 (l3, l1)); False -> lam_next_95 (lam_filter_92 (l3, l1))); (None6_1) -> None12_1

lam_next_95 l0 =
  let (Iter11_0 l1) = l0 in dispatch_130 (l1, ())

lam_map_99 :: ((Iter11, Closure25), ()) -> Option14
lam_map_99 ((l0, l1), ()) =
  case lam_next_95 l0 of (Some12_0 (l2, l3)) -> Some14_0 (lam_count_97 l2, lam_map_98 (l3, l1)); (None12_1) -> None14_1

dispatch_131 :: (Closure24, ()) -> Option14
dispatch_131 (l0, l1) =
  case l0 of (Variant24_0 l2) -> lam_map_99 (l2, l1)

lam_next_104 :: Iter13 -> Option14
lam_next_104 l0 =
  let (Iter13_0 l1) = l0 in dispatch_131 (l1, ())

lam_foldl_103 :: (Iter13, Int64, Closure81) -> Int64
lam_foldl_103 (l0, l1, l2) =
  case lam_next_104 l0 of (Some14_0 (l3, l4)) -> lam_foldl_103 (l4, lam_sum_101 (l1, l3), l2); (None14_1) -> l1

lam_wrapped_foldl_102 :: (Iter13, Int64, Closure81) -> Int64
lam_wrapped_foldl_102 l0 =
  lam_foldl_103 l0

lam_sum_100 :: Iter13 -> Int64
lam_sum_100 l0 =
  lam_wrapped_foldl_102 (l0, 0, Variant81_0 ())

lam_count_96 :: Iter11 -> Int64
lam_count_96 l0 =
  lam_sum_100 (lam_wrapped_map_105 (l0, Variant25_0 ()))

lam_count_primes_76 :: Int64 -> Int64
lam_count_primes_76 l0 =
  lam_count_96 (lam_primes_to_106 l0)

lam_main_75 :: (Int64, ()) -> Int64
lam_main_75 (l0, ()) =
  lam_count_primes_76 l0

dispatch_132 :: (Closure76, ()) -> Int64
dispatch_132 (l0, l1) =
  case l0 of (Variant76_0 l2) -> lam_main_75 (l2, l1)

lam_repeat_110 :: (Int64, Closure76) -> Option0
lam_repeat_110 (l0, l1) =
  case uncurry (<) (l0, 1) of True -> None0_1; False -> (let l2 = dispatch_132 (l1, ()) in (case uncurry (==) (l0, 1) of True -> Some0_0 l2; False -> lam_repeat_110 (uncurry (-) (l0, 1), l1)))

lam_wrapped_repeat_109 :: (Int64, Closure76) -> Option0
lam_wrapped_repeat_109 l0 =
  lam_repeat_110 l0

lam_main_54 :: () -> ()
lam_main_54 () =
  case (lam_string_to_nat_55 (input ()), lam_string_to_nat_55 (input ())) of ((Some0_0 l0), (Some0_0 l1)) -> (case lam_wrapped_repeat_109 (l0, Variant76_0 l1) of (Some0_0 l2) -> (let l_0 = output ((V.fromList [84, 104, 101, 114, 101, 32, 97, 114, 101, 32])); l_1 = output (lam_int_to_string_111 l2); l_2 = output ((V.fromList [32, 112, 114, 105, 109, 101, 115, 32, 60, 61, 32])); l_3 = output (lam_int_to_string_111 l1) in l_0 `seq` l_1 `seq` l_2 `seq` l_3 `seq` output ((V.fromList [10]))); (None0_1) -> ()); (l_4, l_5) -> lam_writeln_118 ((V.fromList [80, 108, 101, 97, 115, 101, 32, 101, 110, 116, 101, 114, 32, 116, 119, 111, 32, 112, 111, 115, 105, 116, 105, 118, 101, 32, 105, 110, 116, 101, 103, 101, 114, 115, 32, 40, 97, 110, 32, 105, 116, 101, 114, 97, 116, 105, 111, 110, 32, 99, 111, 117, 110, 116, 32, 97, 110, 100, 32, 97, 32, 108, 105, 109, 105, 116, 41]))

dispatch_133 :: (Closure75, ()) -> ()
dispatch_133 (l0, l1) =
  case l0 of (Variant75_0 l2) -> lam_main_54 l1

main_wrapper_119 :: () -> ()
main_wrapper_119 () =
  dispatch_133 (main_53 (), ())


main :: IO ()
main = main_wrapper_119 () `seq` return ()
