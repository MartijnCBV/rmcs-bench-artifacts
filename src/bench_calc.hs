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

data Option a
  = Some a
  | None

data Iter a
  = Iter (((()) -> (Option ((a, (Iter a))))))

data Result a b
  = Ok a
  | Err b

data Mt19937_64
  = Mt19937_64 ((Int64, (Vector Int64) ))

data State' a b
  = State' ((a -> (a, b)))

data Parse a
  = Parse ((((Int64, (Vector Word8) )) -> (Option ((Int64, a)))))

data Expr
  = Const Int64
  | Add ((Expr, Expr))
  | Sub ((Expr, Expr))
  | Mul ((Expr, Expr))
  | Div ((Expr, Expr))

is_some_0 :: (Option a) -> Bool
is_some_0 l0 =
  case l0 of Some l1 -> True; None -> False


is_none_1 :: (Option a) -> Bool
is_none_1 l0 =
  case l0 of Some l1 -> False; None -> True


or_default_2 :: ((Option a), a) -> a
or_default_2 (l0, l1) =
  case l0 of Some l2 -> l2; None -> l1


map_3 :: ((Option a), (a -> b)) -> (Option b)
map_3 (l0, l1) =
  case l0 of Some l2 -> Some (l1 l2); None -> None


and_then_4 :: ((Option a), (a -> (Option b))) -> (Option b)
and_then_4 (l0, l1) =
  case l0 of Some l2 -> l1 l2; None -> None


or_else_5 :: ((Option a), ((()) -> (Option a))) -> (Option a)
or_else_5 (l0, l1) =
  case l0 of Some l2 -> Some l2; None -> l1 ()


identity_6 :: a -> a
identity_6 l0 =
  l0


flatten_7 :: (Option (Option a)) -> (Option a)
flatten_7 l0 =
  and_then_4 (l0, identity_6)


both_8 :: ((Option a), (Option b)) -> (Option ((a, b)))
both_8 (l0, l1) =
  case (l0, l1) of (Some l2, Some l3) -> Some (l2, l3); l_0 -> None


either_9 :: ((Option a), (Option a)) -> (Option a)
either_9 (l0, l1) =
  case (l0, l1) of (Some l2, l_1) -> Some l2; (l_2, Some l2) -> Some l2; (None, None) -> None


unwrap_10 :: (Option a) -> a
unwrap_10 l0 =
  case l0 of Some l1 -> l1; None -> panic ((V.fromList [67, 97, 110, 110, 111, 116, 32, 99, 97, 108, 108, 32, 39, 79, 112, 116, 105, 111, 110, 46, 117, 110, 119, 114, 97, 112, 39, 32, 111, 110, 32, 97, 32, 39, 78, 111, 110, 101, 39, 32, 118, 97, 108, 117, 101, 10]))


concat_from_11 :: ((Vector a) , (Vector a) , Int64) -> (Vector a) 
concat_from_11 (l0, l1, l2) =
  case uncurry (==) (l2, intrinsicLen l1) of True -> l0; False -> concat_from_11 (uncurry intrinsicPush (l0, uncurry intrinsicGet (l1, l2)), l1, uncurry (+) (l2, 1))


concat_12 :: ((Vector a) , (Vector a) ) -> (Vector a) 
concat_12 (l0, l1) =
  concat_from_11 (l0, l1, 0)


str_13 :: ((Option a), (a -> (Vector Word8) )) -> (Vector Word8) 
str_13 (l0, l1) =
  case l0 of Some l2 -> concat_12 (concat_12 ((V.fromList [83, 111, 109, 101, 40]), l1 l2), (V.fromList [41])); None -> (V.fromList [78, 111, 110, 101])


empty_14 :: () -> (Iter a)
empty_14 _ =
  Iter (\ () -> None)


once_15 :: a -> (Iter a)
once_15 l0 =
  Iter (\ () -> Some (l0, empty_14 ()))


forever_16 :: a -> (Iter a)
forever_16 l0 =
  Iter (\ () -> Some (l0, forever_16 l0))


range_17 :: (Int64, Int64) -> (Iter Int64)
range_17 (l0, l1) =
  Iter (\ () -> case uncurry (<) (l0, l1) of True -> Some (l0, range_17 (uncurry (+) (l0, 1), l1)); False -> None)


ints_18 :: Int64 -> (Iter Int64)
ints_18 l0 =
  Iter (\ () -> Some (l0, ints_18 (uncurry (+) (l0, 1))))


cons_lazy_20 :: (((()) -> a), ((()) -> (Iter a))) -> (Iter a)
cons_lazy_20 (l0, l1) =
  Iter (\ () -> Some (l0 (), l1 ()))


next_31 :: (Iter a) -> (Option ((a, (Iter a))))
next_31 l0 =
  let Iter l1 = l0 in l1 ()


chain_lazy_21 :: ((Iter a), ((()) -> (Iter a))) -> (Iter a)
chain_lazy_21 (l0, l1) =
  Iter (\ () -> case next_31 l0 of Some (l2, l3) -> Some (l2, chain_lazy_21 (l3, l1)); None -> next_31 (l1 ()))


chain_22 :: ((Iter a), (Iter a)) -> (Iter a)
chain_22 (l0, l1) =
  chain_lazy_21 (l0, (\ () -> l1))


map_23 :: ((Iter a), (a -> b)) -> (Iter b)
map_23 (l0, l1) =
  Iter (\ () -> case next_31 l0 of Some (l2, l3) -> Some (l1 l2, map_23 (l3, l1)); None -> None)


items_19 :: (Vector a)  -> (Iter a)
items_19 l0 =
  map_23 (range_17 (0, intrinsicLen l0), (\ l1 -> uncurry intrinsicGet (l0, l1)))


flatten_24 :: (Iter (Iter a)) -> (Iter a)
flatten_24 l0 =
  Iter (\ () -> case next_31 l0 of Some (l1, l2) -> next_31 (chain_lazy_21 (l1, (\ () -> flatten_24 l2))); None -> None)


flat_map_25 :: ((Iter a), (a -> (Iter b))) -> (Iter b)
flat_map_25 (l0, l1) =
  flatten_24 (map_23 (l0, l1))


filter_26 :: ((Iter a), (a -> Bool)) -> (Iter a)
filter_26 (l0, l1) =
  Iter (\ () -> case next_31 l0 of Some (l2, l3) -> (case l1 l2 of True -> Some (l2, filter_26 (l3, l1)); False -> next_31 (filter_26 (l3, l1))); None -> None)


take_27 :: ((Iter a), Int64) -> (Iter a)
take_27 (l0, l1) =
  case uncurry (>) (l1, 0) of True -> Iter (\ () -> case next_31 l0 of Some (l2, l3) -> Some (l2, take_27 (l3, uncurry (-) (l1, 1))); None -> None); False -> empty_14 ()


take_while_28 :: ((Iter a), (a -> Bool)) -> (Iter a)
take_while_28 (l0, l1) =
  Iter (\ () -> case next_31 l0 of Some (l2, l3) -> (case l1 l2 of True -> Some (l2, take_while_28 (l3, l1)); False -> None); None -> None)


zip_29 :: ((Iter a), (Iter b)) -> (Iter ((a, b)))
zip_29 (l0, l1) =
  Iter (\ () -> case (next_31 l0, next_31 l1) of (Some (l2, l3), Some (l4, l5)) -> Some ((l2, l4), zip_29 (l3, l5)); l_0 -> None)


enumerate_30 :: (Iter a) -> (Iter ((Int64, a)))
enumerate_30 l0 =
  zip_29 (ints_18 0, l0)


foldl_32 :: ((Iter a), b, (((b, a)) -> b)) -> b
foldl_32 (l0, l1, l2) =
  case next_31 l0 of Some (l3, l4) -> foldl_32 (l4, l2 (l1, l3), l2); None -> l1


foldr_33 :: ((Iter a), b, (((a, b)) -> b)) -> b
foldr_33 (l0, l1, l2) =
  case next_31 l0 of Some (l3, l4) -> l2 (l3, foldr_33 (l4, l1, l2)); None -> l1


sum_34 :: (Iter Int64) -> Int64
sum_34 l0 =
  foldl_32 (l0, 0, (\ (l1, l2) -> uncurry (+) (l1, l2)))


count_35 :: (Iter a) -> Int64
count_35 l0 =
  sum_34 (map_23 (l0, (\ l_1 -> 1)))


any_36 :: ((Iter a), (a -> Bool)) -> Bool
any_36 (l0, l1) =
  case next_31 l0 of Some (l2, l3) -> (case l1 l2 of True -> True; False -> any_36 (l3, l1)); None -> False


all_37 :: ((Iter a), (a -> Bool)) -> Bool
all_37 (l0, l1) =
  case next_31 l0 of Some (l2, l3) -> (case l1 l2 of True -> all_37 (l3, l1); False -> False); None -> True


equal_38 :: ((Iter a), (Iter a), (((a, a)) -> Bool)) -> Bool
equal_38 (l0, l1, l2) =
  case (next_31 l0, next_31 l1) of (None, None) -> True; (Some (l3, l4), Some (l5, l6)) -> (case l2 (l3, l5) of True -> equal_38 (l4, l6, l2); False -> False); (l_2, l_3) -> False


find_39 :: ((Iter a), (a -> Bool)) -> (Option a)
find_39 (l0, l1) =
  case next_31 l0 of Some (l2, l3) -> (case l1 l2 of True -> Some l2; False -> find_39 (l3, l1)); None -> None


find_map_40 :: ((Iter a), (a -> (Option b))) -> (Option b)
find_map_40 (l0, l1) =
  case next_31 l0 of Some (l2, l3) -> (case l1 l2 of Some l4 -> Some l4; None -> find_map_40 (l3, l1)); None -> None


join_41 :: ((Iter (Iter a)), (Iter a)) -> (Iter a)
join_41 (l0, l1) =
  Iter (\ () -> case next_31 l0 of Some (l2, l3) -> next_31 (chain_lazy_21 (l2, (\ () -> chain_lazy_21 (l1, (\ () -> join_41 (l3, l1)))))); None -> None)


find_index_42 :: ((Iter a), (a -> Bool)) -> (Option Int64)
find_index_42 (l0, l1) =
  find_map_40 (enumerate_30 l0, (\ (l2, l3) -> case l1 l3 of True -> Some l2; False -> None))


for_each_43 :: ((Iter a), (a -> ())) -> ()
for_each_43 (l0, l1) =
  case next_31 l0 of Some (l2, l3) -> (let l_0 = l1 l2 in l_0 `seq` for_each_43 (l3, l1)); None -> ()


get__44 :: () -> ((((Vector a) , Int64)) -> a)
get__44 _ =
  uncurry intrinsicGet


len__45 :: () -> ((Vector a)  -> Int64)
len__45 _ =
  intrinsicLen


push__46 :: () -> ((((Vector a) , a)) -> (Vector a) )
push__46 _ =
  uncurry intrinsicPush


get_47 :: () -> ((((Vector a) , Int64)) -> a)
get_47 _ =
  get__44 ()


len_48 :: () -> ((Vector a)  -> Int64)
len_48 _ =
  len__45 ()


push_49 :: () -> ((((Vector a) , a)) -> (Vector a) )
push_49 _ =
  push__46 ()


set_50 :: ((Vector a) , Int64, a) -> (Vector a) 
set_50 (l0, l1, l2) =
  let (l_0, l3) = uncurry intrinsicExtract (l0, l1) in l_0 `seq` l3 l2


update_51 :: ((Vector a) , Int64, (a -> a)) -> (Vector a) 
update_51 (l0, l1, l2) =
  let (l3, l4) = uncurry intrinsicExtract (l0, l1) in l4 (l2 l3)


try_get_52 :: ((Vector a) , Int64) -> (Option a)
try_get_52 (l0, l1) =
  case (case uncurry (>=) (l1, 0) of False -> False; True -> uncurry (<) (l1, (len_48 ()) l0)) of True -> Some ((get_47 ()) (l0, l1)); False -> None


concat_from_53 :: ((Vector a) , (Vector a) , Int64) -> (Vector a) 
concat_from_53 (l0, l1, l2) =
  case uncurry (==) (l2, (len_48 ()) l1) of True -> l0; False -> concat_from_53 ((push_49 ()) (l0, (get_47 ()) (l1, l2)), l1, uncurry (+) (l2, 1))


concat_54 :: ((Vector a) , (Vector a) ) -> (Vector a) 
concat_54 (l0, l1) =
  concat_from_53 (l0, l1, 0)


find_index_rec_55 :: ((Vector a) , (a -> Bool), Int64) -> (Option Int64)
find_index_rec_55 (l0, l1, l2) =
  case uncurry (>=) (l2, (len_48 ()) l0) of True -> None; False -> (case l1 ((get_47 ()) (l0, l2)) of True -> Some l2; False -> find_index_rec_55 (l0, l1, uncurry (+) (l2, 1)))


find_index_56 :: ((Vector a) , (a -> Bool)) -> (Option Int64)
find_index_56 (l0, l1) =
  find_index_rec_55 (l0, l1, 0)


all_rec_57 :: ((Vector a) , (a -> Bool), Int64) -> Bool
all_rec_57 (l0, l1, l2) =
  case uncurry (>=) (l2, (len_48 ()) l0) of True -> True; False -> (case not (l1 ((get_47 ()) (l0, l2))) of True -> False; False -> all_rec_57 (l0, l1, uncurry (+) (l2, 1)))


all_58 :: ((Vector a) , (a -> Bool)) -> Bool
all_58 (l0, l1) =
  all_rec_57 (l0, l1, 0)


foldl_rec_59 :: ((Vector a) , b, (((b, a)) -> b), Int64) -> b
foldl_rec_59 (l0, l1, l2, l3) =
  case uncurry (>=) (l3, (len_48 ()) l0) of True -> l1; False -> foldl_rec_59 (l0, l2 (l1, (get_47 ()) (l0, l3)), l2, uncurry (+) (l3, 1))


foldl_60 :: ((Vector a) , b, (((b, a)) -> b)) -> b
foldl_60 (l0, l1, l2) =
  foldl_rec_59 (l0, l1, l2, 0)


map_rec_61 :: ((Vector a) , (a -> b), Int64) -> (Vector b) 
map_rec_61 (l0, l1, l2) =
  case uncurry (<) (l2, 0) of True -> (V.fromList []); False -> (push_49 ()) (map_rec_61 (l0, l1, uncurry (-) (l2, 1)), l1 ((get_47 ()) (l0, l2)))


is_empty_62 :: (Vector a)  -> Bool
is_empty_62 l0 =
  uncurry (==) ((len_48 ()) l0, 0)


map_63 :: ((Vector a) , (a -> b)) -> (Vector b) 
map_63 (l0, l1) =
  map_rec_61 (l0, l1, uncurry (-) ((len_48 ()) l0, 1))


slice_rec_64 :: ((Vector a) , Int64, Int64, Int64) -> (Vector a) 
slice_rec_64 (l0, l1, l2, l3) =
  case uncurry (<) (l3, l1) of True -> (V.fromList []); False -> (push_49 ()) (slice_rec_64 (l0, l1, l2, uncurry (-) (l3, 1)), (get_47 ()) (l0, l3))


slice_65 :: ((Vector a) , Int64, Int64) -> (Vector a) 
slice_65 (l0, l1, l2) =
  case (case (case (case (case uncurry (<) (l1, 0) of True -> True; False -> uncurry (<) (l2, 0)) of True -> True; False -> uncurry (>=) (l1, (len_48 ()) l0)) of True -> True; False -> uncurry (>) (l2, (len_48 ()) l0)) of True -> True; False -> uncurry (<=) (l2, l1)) of True -> (V.fromList []); False -> slice_rec_64 (l0, l1, l2, uncurry (-) (l2, 1))


zip_rec_66 :: ((Vector a) , (Vector b) , Int64) -> (Vector ((a, b))) 
zip_rec_66 (l0, l1, l2) =
  case uncurry (<) (l2, 0) of True -> (V.fromList []); False -> (let (l3, l4) = ((get_47 ()) (l0, l2), (get_47 ()) (l1, l2)) in (push_49 ()) (zip_rec_66 (l0, l1, uncurry (-) (l2, 1)), (l3, l4)))


zip_67 :: ((Vector a) , (Vector b) ) -> (Vector ((a, b))) 
zip_67 (l0, l1) =
  let l2 = uncurry (-) ((case uncurry (<) ((len_48 ()) l0, (len_48 ()) l1) of True -> (len_48 ()) l0; False -> (len_48 ()) l1), 1) in (case uncurry (<) (l2, 0) of True -> (V.fromList []); False -> zip_rec_66 (l0, l1, l2))


push_front_68 :: (a, (Vector a) ) -> (Vector a) 
push_front_68 (l0, l1) =
  concat_54 ((V.fromList [l0]), l1)


equal_rec_69 :: ((Vector a) , (Vector a) , (((a, a)) -> Bool), Int64) -> Bool
equal_rec_69 (l0, l1, l2, l3) =
  case uncurry (>=) (l3, (len_48 ()) l0) of True -> True; False -> (case uncurry (>=) (l3, (len_48 ()) l1) of True -> False; False -> (case not (l2 ((get_47 ()) (l0, l3), (get_47 ()) (l1, l3))) of True -> False; False -> equal_rec_69 (l0, l1, l2, uncurry (+) (l3, 1))))


equal_70 :: ((Vector a) , (Vector a) , (((a, a)) -> Bool)) -> Bool
equal_70 (l0, l1, l2) =
  case not (uncurry (==) ((len_48 ()) l0, (len_48 ()) l1)) of True -> False; False -> equal_rec_69 (l0, l1, l2, 0)


fill_with_rec_71 :: ((Vector a) , Int64, ((()) -> a)) -> (Vector a) 
fill_with_rec_71 (l0, l1, l2) =
  case uncurry (>) (l1, 0) of True -> fill_with_rec_71 ((push_49 ()) (l0, l2 ()), uncurry (-) (l1, 1), l2); False -> l0


fill_with_72 :: (Int64, ((()) -> a)) -> (Vector a) 
fill_with_72 (l0, l1) =
  fill_with_rec_71 (uncurry intrinsicReserve ((V.fromList []), l0), l0, l1)


fill_73 :: (Int64, a) -> (Vector a) 
fill_73 (l0, l1) =
  fill_with_72 (l0, (\ () -> l1))


from_iter_with_capacity_74 :: ((Iter a), Int64) -> (Vector a) 
from_iter_with_capacity_74 (l0, l1) =
  foldl_32 (l0, uncurry intrinsicReserve ((V.fromList []), l1), push_49 ())


from_iter_75 :: (Iter a) -> (Vector a) 
from_iter_75 l0 =
  from_iter_with_capacity_74 (l0, 0)


reverse_rec_76 :: ((Vector a) , Int64, (Vector a) ) -> (Vector a) 
reverse_rec_76 (l0, l1, l2) =
  case uncurry (<) (l1, 0) of True -> l2; False -> reverse_rec_76 (l0, uncurry (-) (l1, 1), (push_49 ()) (l2, (get_47 ()) (l0, l1)))


reverse_77 :: (Vector a)  -> (Vector a) 
reverse_77 l0 =
  reverse_rec_76 (l0, uncurry (-) ((len_48 ()) l0, 1), (V.fromList []))


str_78 :: ((Vector a) , (a -> (Vector Word8) )) -> (Vector Word8) 
str_78 (l0, l1) =
  let l2 = uncurry (-) ((len_48 ()) l0, 1) in (let l3 = (let l3 = (let l3 = (let l3 = items_19 l0 in take_27 (l3, l2)) in foldl_32 (l3, (V.fromList [91]), (\ (l4, l5) -> let l6 = (let l6 = l4 in concat_54 (l6, l1 l5)) in concat_54 (l6, (V.fromList [44, 32]))))) in concat_54 (l3, l1 ((get_47 ()) (l0, l2)))) in concat_54 (l3, (V.fromList [93])))


is_ok_79 :: (Result a b) -> Bool
is_ok_79 l0 =
  case l0 of Ok l1 -> True; Err l_0 -> False


is_error_80 :: (Result a b) -> Bool
is_error_80 l0 =
  case l0 of Ok l_1 -> False; Err l_2 -> True


or_default_81 :: ((Result a b), a) -> a
or_default_81 (l0, l1) =
  case l0 of Ok l2 -> l2; Err l_3 -> l1


map_82 :: ((Result a b), (a -> c)) -> (Result c b)
map_82 (l0, l1) =
  case l0 of Ok l2 -> Ok (l1 l2); Err l2 -> Err l2


and_then_83 :: ((Result a b), (a -> (Result c b))) -> (Result c b)
and_then_83 (l0, l1) =
  case l0 of Ok l2 -> l1 l2; Err l2 -> Err l2


or_else_84 :: ((Result a b), ((()) -> (Result a b))) -> (Result a b)
or_else_84 (l0, l1) =
  case l0 of Ok l2 -> Ok l2; Err l_4 -> l1 ()


identity_85 :: a -> a
identity_85 l0 =
  l0


flatten_86 :: (Result (Result a b) b) -> (Result a b)
flatten_86 l0 =
  and_then_83 (l0, identity_85)


both_87 :: ((Result a b), (Result c b)) -> (Result ((a, c)) b)
both_87 (l0, l1) =
  case (l0, l1) of (Ok l2, Ok l3) -> Ok (l2, l3); (Err l2, l_5) -> Err l2; (l_6, Err l2) -> Err l2


either_88 :: ((Result a b), (Result a b)) -> (Result a b)
either_88 (l0, l1) =
  case (l0, l1) of (Ok l2, l_7) -> Ok l2; (l_8, Ok l2) -> Ok l2; (Err l2, Err l_9) -> Err l2


unwrap_89 :: (Result a b) -> a
unwrap_89 l0 =
  case l0 of Ok l1 -> l1; Err l_10 -> panic ((V.fromList [67, 97, 110, 110, 111, 116, 32, 99, 97, 108, 108, 32, 39, 82, 101, 115, 117, 108, 116, 46, 117, 110, 119, 114, 97, 112, 39, 32, 111, 110, 32, 97, 110, 32, 39, 69, 114, 114, 39, 32, 118, 97, 108, 117, 101, 10]))


str_90 :: ((Result a b), (a -> (Vector Word8) ), (b -> (Vector Word8) )) -> (Vector Word8) 
str_90 (l0, l1, l2) =
  case l0 of Ok l3 -> concat_54 (concat_54 ((V.fromList [79, 107, 40]), l1 l3), (V.fromList [41])); Err l3 -> concat_54 (concat_54 ((V.fromList [69, 114, 114, 40]), l2 l3), (V.fromList [41]))


ok_or_91 :: ((Option a), b) -> (Result a b)
ok_or_91 (l0, l1) =
  case l0 of Some l2 -> Ok l2; None -> Err l1


all_ok_92 :: (Vector ((Result a b)))  -> (Result (Vector a)  b)
all_ok_92 l0 =
  foldl_60 (l0, Ok ((V.fromList [])), (\ (l1, l2) -> case (l1, l2) of (Ok l3, Ok l4) -> Ok (concat_54 (l3, (V.fromList [l4]))); (Err l3, l_11) -> Err l3; (l_12, Err l3) -> Err l3))


bool_to_string_93 :: Bool -> (Vector Word8) 
bool_to_string_93 l0 =
  case l0 of True -> (V.fromList [84, 114, 117, 101]); False -> (V.fromList [70, 97, 108, 115, 101])


nat_to_string_94 :: Int64 -> (Vector Word8) 
nat_to_string_94 l0 =
  let l1 = (\ l1 -> case l1 of 0 -> (V.fromList [48]); 1 -> (V.fromList [49]); 2 -> (V.fromList [50]); 3 -> (V.fromList [51]); 4 -> (V.fromList [52]); 5 -> (V.fromList [53]); 6 -> (V.fromList [54]); 7 -> (V.fromList [55]); 8 -> (V.fromList [56]); 9 -> (V.fromList [57]); l_0 -> (V.fromList [])) in (case uncurry (==) (l0, 0) of True -> (V.fromList []); False -> concat_54 (nat_to_string_94 (uncurry divv (l0, 10)), l1 (uncurry (-) (l0, uncurry (*) (uncurry divv (l0, 10), 10)))))


int_to_string_95 :: Int64 -> (Vector Word8) 
int_to_string_95 l0 =
  case uncurry (==) (l0, 0) of True -> (V.fromList [48]); False -> (case uncurry (<) (l0, 0) of True -> concat_54 ((V.fromList [45]), nat_to_string_94 (uncurry (-) (0, l0))); False -> nat_to_string_94 l0)


digit_ascii_base_96 :: () -> Word8
digit_ascii_base_96 _ =
  48


digit_to_nat_97 :: Word8 -> (Option Int64)
digit_to_nat_97 l0 =
  case l0 of 48 -> Some 0; 49 -> Some 1; 50 -> Some 2; 51 -> Some 3; 52 -> Some 4; 53 -> Some 5; 54 -> Some 6; 55 -> Some 7; 56 -> Some 8; 57 -> Some 9; l_1 -> None


chars_to_nat_98 :: (Iter Word8) -> (Option Int64)
chars_to_nat_98 l0 =
  case next_31 l0 of None -> None; Some l_2 -> foldl_32 (l0, Some 0, (\ (l1, l2) -> and_then_4 (l1, (\ l3 -> map_3 (digit_to_nat_97 l2, (\ l4 -> uncurry (+) (uncurry (*) (l3, 10), l4)))))))


string_to_nat_99 :: (Vector Word8)  -> (Option Int64)
string_to_nat_99 l0 =
  chars_to_nat_98 (items_19 l0)


string_to_int_100 :: (Vector Word8)  -> (Option Int64)
string_to_int_100 l0 =
  case (let l1 = items_19 l0 in next_31 l1) of Some (l1, l2) -> (case uncurry (==) (l1, uncurry intrinsicGet ((V.fromList [45]), 0)) of True -> chars_to_nat_98 l2; False -> string_to_nat_99 l0); None -> None


equal_101 :: ((Vector Word8) , (Vector Word8) ) -> Bool
equal_101 (l0, l1) =
  equal_70 (l0, l1, (\ (l2, l3) -> uncurry (==) (l2, l3)))


concat_102 :: ((Vector Word8) , (Vector Word8) ) -> (Vector Word8) 
concat_102 (l0, l1) =
  concat_54 (l0, l1)


join_103 :: ((Vector ((Vector Word8) )) , (Vector Word8) ) -> (Vector Word8) 
join_103 (l0, l1) =
  let l2 = join_41 ((let l2 = items_19 l0 in map_23 (l2, items_19)), items_19 l1) in from_iter_75 l2


id_104 :: a -> a
id_104 l0 =
  l0


compose_105 :: ((a -> b), (b -> c)) -> (a -> c)
compose_105 (l0, l1) =
  \ l2 -> l1 (l0 l2)


const_106 :: a -> (b -> a)
const_106 l0 =
  \ l_0 -> l0


iterate_107 :: ((a -> a), Int64) -> (a -> a)
iterate_107 (l0, l1) =
  case uncurry (<=) (l1, 0) of True -> id_104; False -> compose_105 (l0, iterate_107 (l0, uncurry (-) (l1, 1)))


writeln_108 :: (Vector Word8)  -> ()
writeln_108 l0 =
  let l_0 = output l0 in l_0 `seq` output ((V.fromList [10]))


max_int_109 :: () -> Int64
max_int_109 _ =
  2305843009213693951


cmp_max_110 :: (Int64, Int64) -> Int64
cmp_max_110 (l0, l1) =
  case uncurry (>) (l0, l1) of True -> l0; False -> l1


cmp_min_111 :: (Int64, Int64) -> Int64
cmp_min_111 (l0, l1) =
  case uncurry (<) (l0, l1) of True -> l0; False -> l1


int_bit_not_112 :: Int64 -> Int64
int_bit_not_112 l0 =
  uncurry (-) (negate l0, 1)


rem_113 :: (Int64, Int64) -> Int64
rem_113 (l0, l1) =
  uncurry (-) (l0, uncurry (*) (l1, uncurry divv (l0, l1)))


mod_114 :: (Int64, Int64) -> Int64
mod_114 (l0, l1) =
  case uncurry (<) (l0, 0) of True -> uncurry (+) (rem_113 (l0, l1), l1); False -> rem_113 (l0, l1)


pow_rec_115 :: (Int64, Int64, Int64) -> Int64
pow_rec_115 (l0, l1, l2) =
  case uncurry (==) (l2, 0) of True -> l0; False -> pow_rec_115 (uncurry (*) (l1, l0), l1, uncurry (-) (l2, 1))


pow_116 :: (Int64, Int64) -> Int64
pow_116 (l0, l1) =
  pow_rec_115 (1, l0, l1)


byte_pow_rec_117 :: (Word8, Word8, Word8) -> Word8
byte_pow_rec_117 (l0, l1, l2) =
  case uncurry (==) (l2, 0) of True -> l0; False -> byte_pow_rec_117 (uncurry (*) (l1, l0), l1, uncurry (-) (l2, 1))


byte_pow_118 :: (Word8, Word8) -> Word8
byte_pow_118 (l0, l1) =
  byte_pow_rec_117 (1, l0, l1)


float_pow_119 :: (Double, Int64) -> Double
float_pow_119 (l0, l1) =
  case uncurry (==) (l1, 0) of True -> 1.0; False -> (case uncurry (<) (l1, 0) of True -> uncurry divv (1.0, float_pow_119 (l0, negate l1)); False -> (case uncurry (==) (uncurry (.&.) (l1, 1), 0) of True -> float_pow_119 (uncurry (*) (l0, l0), uncurry shiftR_ (l1, 1)); False -> uncurry (*) (l0, float_pow_119 (uncurry (*) (l0, l0), uncurry shiftR_ (l1, 1)))))


int_to_float_rec_120 :: (Double, Int64) -> Double
int_to_float_rec_120 (l0, l1) =
  case uncurry (==) (l1, 0) of True -> l0; False -> (case uncurry (==) (uncurry (.&.) (l1, 1), 0) of True -> int_to_float_rec_120 (uncurry (*) (l0, 2.0), uncurry shiftR_ (l1, 1)); False -> int_to_float_rec_120 (uncurry (+) (uncurry (*) (l0, 2.0), 1.0), uncurry shiftR_ (l1, 1)))


int_to_float_121 :: Int64 -> Double
int_to_float_121 l0 =
  case uncurry (<) (l0, 0) of True -> negate (int_to_float_rec_120 (0.0, negate l0)); False -> int_to_float_rec_120 (0.0, l0)


do_while_122 :: ((a -> Bool), ((()) -> a)) -> ()
do_while_122 (l0, l1) =
  case l0 (l1 ()) of True -> do_while_122 (l0, l1); False -> ()


while_123 :: (a, (a -> Bool), ((()) -> a)) -> ()
while_123 (l0, l1, l2) =
  case l1 l0 of True -> do_while_122 (l1, l2); False -> ()


forever_124 :: ((()) -> ()) -> ()
forever_124 l0 =
  let l_0 = l0 () in l_0 `seq` forever_124 l0


coeff_w_125 :: () -> Int64
coeff_w_125 _ =
  64


coeff_n_126 :: () -> Int64
coeff_n_126 _ =
  312


coeff_m_127 :: () -> Int64
coeff_m_127 _ =
  156


coeff_r_128 :: () -> Int64
coeff_r_128 _ =
  31


coeff_a_129 :: () -> Int64
coeff_a_129 _ =
  negate 5403634167711393303


coeff_u_130 :: () -> Int64
coeff_u_130 _ =
  29


coeff_d_131 :: () -> Int64
coeff_d_131 _ =
  6148914691236517205


coeff_s_132 :: () -> Int64
coeff_s_132 _ =
  17


coeff_b_133 :: () -> Int64
coeff_b_133 _ =
  8202884508482404352


coeff_t_134 :: () -> Int64
coeff_t_134 _ =
  37


coeff_c_135 :: () -> Int64
coeff_c_135 _ =
  negate 2270628950310912


coeff_l_136 :: () -> Int64
coeff_l_136 _ =
  43


coeff_f_137 :: () -> Int64
coeff_f_137 _ =
  6364136223846793005


lower_mask_138 :: () -> Int64
lower_mask_138 _ =
  uncurry (-) (uncurry shiftL_ (1, coeff_r_128 ()), 1)


upper_mask_139 :: () -> Int64
upper_mask_139 _ =
  int_bit_not_112 (lower_mask_138 ())


seed_mt19937_64_rec_140 :: ((Vector Int64) , Int64, Int64, Int64) -> (Vector Int64) 
seed_mt19937_64_rec_140 (l0, l1, l2, l3) =
  case uncurry (>=) (l2, l3) of True -> l0; False -> (let l4 = uncurry (+) (uncurry (*) (coeff_f_137 (), uncurry xor (l1, uncurry shiftR_ (l1, uncurry (-) (coeff_w_125 (), 2)))), l2) in seed_mt19937_64_rec_140 (uncurry intrinsicPush (l0, l4), l4, uncurry (+) (l2, 1), l3))


seed_mt19937_64_141 :: Int64 -> Mt19937_64
seed_mt19937_64_141 l0 =
  Mt19937_64 (coeff_n_126 (), seed_mt19937_64_rec_140 ((V.fromList [l0]), l0, 1, coeff_n_126 ()))


mt19937_64_twist_rec_142 :: ((Vector Int64) , Int64, Int64) -> (Vector Int64) 
mt19937_64_twist_rec_142 (l0, l1, l2) =
  case uncurry (>=) (l1, l2) of True -> l0; False -> (let l3 = uncurry (+) (uncurry (.&.) ((get_47 ()) (l0, l1), upper_mask_139 ()), uncurry (.&.) ((get_47 ()) (l0, rem_113 (uncurry (+) (l1, 1), coeff_n_126 ())), lower_mask_138 ())); l4 = (case uncurry (==) (uncurry (.&.) (l3, 1), 0) of True -> uncurry shiftR_ (l3, 1); False -> uncurry xor (uncurry shiftR_ (l3, 1), coeff_a_129 ())); l5 = uncurry xor ((get_47 ()) (l0, rem_113 (uncurry (+) (l1, coeff_m_127 ()), coeff_n_126 ())), l4) in mt19937_64_twist_rec_142 (set_50 (l0, l1, l5), uncurry (+) (l1, 1), l2))


mt19937_64_twist_143 :: Mt19937_64 -> Mt19937_64
mt19937_64_twist_143 l0 =
  let Mt19937_64 (l_0, l1) = l0 in l_0 `seq` Mt19937_64 (0, mt19937_64_twist_rec_142 (l1, 0, coeff_n_126 ()))


mt19937_64_next_144 :: Mt19937_64 -> (Mt19937_64, Int64)
mt19937_64_next_144 l0 =
  let Mt19937_64 (l1, l_0) = l0; Mt19937_64 (l2, l3) = (case uncurry (>=) (l1, coeff_n_126 ()) of True -> mt19937_64_twist_143 l0; False -> l0); l4 = (get_47 ()) (l3, l2); l5 = uncurry xor (l4, uncurry (.&.) (uncurry shiftR_ (l4, coeff_u_130 ()), coeff_d_131 ())); l6 = uncurry xor (l5, uncurry (.&.) (uncurry shiftL_ (l5, coeff_s_132 ()), coeff_b_133 ())); l7 = uncurry xor (l6, uncurry (.&.) (uncurry shiftL_ (l6, coeff_t_134 ()), coeff_c_135 ())); l8 = uncurry xor (l7, uncurry shiftR_ (l7, coeff_l_136 ())) in l_0 `seq` (Mt19937_64 (uncurry (+) (l2, 1), l3), l8)


get_145 :: () -> (State' a a)
get_145 _ =
  State' (\ l0 -> (l0, l0))


set_146 :: a -> (State' a (()))
set_146 l0 =
  State' (\ l_1 -> (l0, ()))


modify_147 :: (a -> a) -> (State' a (()))
modify_147 l0 =
  State' (\ l1 -> (l0 l1, ()))


modify_ret_148 :: (a -> (a, b)) -> (State' a b)
modify_ret_148 l0 =
  State' l0


pure_149 :: a -> (State' b a)
pure_149 l0 =
  State' (\ l1 -> (l1, l0))


run_150 :: ((State' a b), a) -> (a, b)
run_150 (l0, l1) =
  let State' l2 = l0 in l2 l1


bind_151 :: ((State' a b), (b -> (State' a c))) -> (State' a c)
bind_151 (l0, l1) =
  State' (\ l2 -> let (l3, l4) = run_150 (l0, l2) in run_150 (l1 l4, l3))


seq_152 :: ((State' a b), (State' a c)) -> (State' a c)
seq_152 (l0, l1) =
  bind_151 (l0, (\ l_0 -> l1))


for_each_153 :: ((Iter a), (a -> (State' b (())))) -> (State' b (()))
for_each_153 (l0, l1) =
  State' (\ l2 -> let l3 = foldl_32 (l0, l2, (\ (l3, l4) -> let (l5, l_0) = run_150 (l1 l4, l3) in l_0 `seq` l5)) in (l3, ()))


parse_from_154 :: (Int64, (Vector Word8) , (Parse a)) -> (Option ((Int64, a)))
parse_from_154 (l0, l1, l2) =
  let Parse l3 = l2 in l3 (l0, l1)


parse_prefix_155 :: ((Vector Word8) , (Parse a)) -> (Option ((Int64, a)))
parse_prefix_155 (l0, l1) =
  parse_from_154 (0, l0, l1)


parse_all_156 :: ((Vector Word8) , (Parse a)) -> (Option a)
parse_all_156 (l0, l1) =
  let l2 = (let l2 = l0 in parse_prefix_155 (l2, l1)) in and_then_4 (l2, (\ (l3, l4) -> case uncurry (==) (l3, (len_48 ()) l0) of True -> Some l4; False -> None))


pure_157 :: a -> (Parse a)
pure_157 l0 =
  Parse (\ (l1, l2) -> Some (l1, l0))


fail_158 :: () -> (Parse a)
fail_158 _ =
  Parse (\ (l_0, l_1) -> None)


lazy_159 :: ((()) -> (Parse a)) -> (Parse a)
lazy_159 l0 =
  Parse (\ (l1, l2) -> parse_from_154 (l1, l2, l0 ()))


map_160 :: ((Parse a), (a -> b)) -> (Parse b)
map_160 (l0, l1) =
  Parse (\ (l2, l3) -> let l4 = parse_from_154 (l2, l3, l0) in map_3 (l4, (\ (l5, l6) -> (l5, l1 l6))))


and_then_161 :: ((Parse a), (a -> (Parse b))) -> (Parse b)
and_then_161 (l0, l1) =
  Parse (\ (l2, l3) -> let l4 = parse_from_154 (l2, l3, l0) in and_then_4 (l4, (\ (l5, l6) -> parse_from_154 (l5, l3, l1 l6))))


skip_pre_162 :: ((Parse a), (Parse b)) -> (Parse b)
skip_pre_162 (l0, l1) =
  Parse (\ (l2, l3) -> let l4 = parse_from_154 (l2, l3, l0) in and_then_4 (l4, (\ (l5, l_0) -> parse_from_154 (l5, l3, l1))))


skip_post_163 :: ((Parse a), (Parse b)) -> (Parse a)
skip_post_163 (l0, l1) =
  Parse (\ (l2, l3) -> let l4 = parse_from_154 (l2, l3, l0) in and_then_4 (l4, (\ (l5, l6) -> let l7 = parse_from_154 (l5, l3, l1) in map_3 (l7, (\ (l8, l_0) -> (l8, l6))))))


or_164 :: ((Parse a), (Parse a)) -> (Parse a)
or_164 (l0, l1) =
  Parse (\ (l2, l3) -> let l4 = parse_from_154 (l2, l3, l0) in or_else_5 (l4, (\ () -> parse_from_154 (l2, l3, l1))))


optional_175 :: (Parse a) -> (Parse (Option a))
optional_175 l0 =
  or_164 (map_160 (l0, (\ l1 -> Some l1)), lazy_159 (\ () -> pure_157 None))


many0_fold_165 :: ((Parse a), b, (((b, a)) -> b)) -> (Parse b)
many0_fold_165 (l0, l1, l2) =
  and_then_161 (optional_175 l0, (\ l3 -> case l3 of None -> pure_157 l1; Some l4 -> many0_fold_165 (l0, l2 (l1, l4), l2)))


many0_166 :: (Parse a) -> (Parse (Vector a) )
many0_166 l0 =
  lazy_159 (\ () -> many0_fold_165 (l0, (V.fromList []), push_49 ()))


skip_many0_167 :: (Parse a) -> (Parse (()))
skip_many0_167 l0 =
  lazy_159 (\ () -> many0_fold_165 (l0, (), (\ (l_0, l_1) -> ())))


many1_fold_168 :: ((Parse a), b, (((b, a)) -> b)) -> (Parse b)
many1_fold_168 (l0, l1, l2) =
  and_then_161 (l0, (\ l3 -> many0_fold_165 (l0, l2 (l1, l3), l2)))


many1_169 :: (Parse a) -> (Parse (Vector a) )
many1_169 l0 =
  lazy_159 (\ () -> many1_fold_168 (l0, (V.fromList []), push_49 ()))


skip_many1_170 :: (Parse a) -> (Parse (()))
skip_many1_170 l0 =
  lazy_159 (\ () -> many1_fold_168 (l0, (), (\ (l_2, l_3) -> ())))


sep1_fold_173 :: ((Parse a), (Parse b), c, (((c, a)) -> c)) -> (Parse c)
sep1_fold_173 (l0, l1, l2, l3) =
  and_then_161 (l0, (\ l4 -> many0_fold_165 (skip_pre_162 (l1, l0), l3 (l2, l4), l3)))


sep1_174 :: ((Parse a), (Parse b)) -> (Parse (Vector a) )
sep1_174 (l0, l1) =
  lazy_159 (\ () -> sep1_fold_173 (l0, l1, (V.fromList []), push_49 ()))


sep0_fold_171 :: ((Parse a), (Parse b), c, (((c, a)) -> c)) -> (Parse c)
sep0_fold_171 (l0, l1, l2, l3) =
  and_then_161 (optional_175 l0, (\ l4 -> case l4 of None -> pure_157 l2; Some l5 -> many0_fold_165 (skip_pre_162 (l1, l0), l3 (l2, l5), l3)))


sep0_172 :: ((Parse a), (Parse b)) -> (Parse (Vector a) )
sep0_172 (l0, l1) =
  lazy_159 (\ () -> sep0_fold_171 (l0, l1, (V.fromList []), push_49 ()))


lookahead_176 :: (Parse a) -> (Parse a)
lookahead_176 l0 =
  Parse (\ (l1, l2) -> let l3 = parse_from_154 (l1, l2, l0) in map_3 (l3, (\ (l4, l5) -> (l1, l5))))


not_followed_by_177 :: (Parse a) -> (Parse (()))
not_followed_by_177 l0 =
  Parse (\ (l1, l2) -> case parse_from_154 (l1, l2, l0) of Some l_0 -> None; None -> Some (l1, ()))


between_178 :: ((Parse a), (Parse b), (Parse c)) -> (Parse c)
between_178 (l0, l1, l2) =
  skip_pre_162 (l0, skip_post_163 (l2, l1))


guard_179 :: ((Parse a), (a -> Bool)) -> (Parse a)
guard_179 (l0, l1) =
  and_then_161 (l0, (\ l2 -> case l1 l2 of True -> pure_157 l2; False -> fail_158 ()))


byte_180 :: () -> (Parse Word8)
byte_180 _ =
  Parse (\ (l0, l1) -> case uncurry (<) (l0, (len_48 ()) l1) of True -> Some (uncurry (+) (l0, 1), (get_47 ()) (l1, l0)); False -> None)


byte_eq_181 :: Word8 -> (Parse Word8)
byte_eq_181 l0 =
  guard_179 (byte_180 (), (\ l1 -> uncurry (==) (l1, l0)))


byte_ne_182 :: Word8 -> (Parse Word8)
byte_ne_182 l0 =
  guard_179 (byte_180 (), (\ l1 -> not (uncurry (==) (l1, l0))))


byte_range_183 :: (Word8, Word8) -> (Parse Word8)
byte_range_183 (l0, l1) =
  guard_179 (byte_180 (), (\ l2 -> case uncurry (<=) (l0, l2) of False -> False; True -> uncurry (<=) (l2, l1)))


lit_from_184 :: ((Vector Word8) , Int64) -> (Parse (Vector Word8) )
lit_from_184 (l0, l1) =
  lazy_159 (\ () -> case uncurry (==) (l1, (len_48 ()) l0) of True -> pure_157 l0; False -> skip_pre_162 (byte_eq_181 ((get_47 ()) (l0, l1)), lit_from_184 (l0, uncurry (+) (l1, 1))))


lit_185 :: (Vector Word8)  -> (Parse (Vector Word8) )
lit_185 l0 =
  lit_from_184 (l0, 0)


repeat_186 :: (Int64, ((()) -> a)) -> (Option a)
repeat_186 (l0, l1) =
  case uncurry (<) (l0, 1) of True -> None; False -> (let l2 = l1 () in (case uncurry (==) (l0, 1) of True -> Some l2; False -> repeat_186 (uncurry (-) (l0, 1), l1)))


eval_187 :: Expr -> Int64
eval_187 l0 =
  case l0 of Const l1 -> l1; Add (l1, l2) -> uncurry (+) (eval_187 l1, eval_187 l2); Sub (l1, l2) -> uncurry (-) (eval_187 l1, eval_187 l2); Mul (l1, l2) -> uncurry (*) (eval_187 l1, eval_187 l2); Div (l1, l2) -> uncurry divv (eval_187 l1, eval_187 l2)


ascii_zero_188 :: () -> Word8
ascii_zero_188 _ =
  48


ascii_nine_189 :: () -> Word8
ascii_nine_189 _ =
  57


ascii_space_190 :: () -> Word8
ascii_space_190 _ =
  32


ascii_newline_191 :: () -> Word8
ascii_newline_191 _ =
  10


ascii_tab_192 :: () -> Word8
ascii_tab_192 _ =
  9


ascii_asterisk_193 :: () -> Word8
ascii_asterisk_193 _ =
  42


ascii_slash_194 :: () -> Word8
ascii_slash_194 _ =
  47


ascii_plus_195 :: () -> Word8
ascii_plus_195 _ =
  43


ascii_minus_196 :: () -> Word8
ascii_minus_196 _ =
  45


ascii_open_paren_197 :: () -> Word8
ascii_open_paren_197 _ =
  40


ascii_close_paren_198 :: () -> Word8
ascii_close_paren_198 _ =
  41


digit_199 :: () -> (Parse Int64)
digit_199 _ =
  let l0 = byte_range_183 (ascii_zero_188 (), ascii_nine_189 ()) in map_160 (l0, (\ l1 -> fromIntegral (uncurry (-) (l1, ascii_zero_188 ()))))


int_200 :: () -> (Parse Int64)
int_200 _ =
  many1_fold_168 (digit_199 (), 0, (\ (l0, l1) -> uncurry (+) (uncurry (*) (l0, 10), l1)))


space_201 :: () -> (Parse (()))
space_201 _ =
  let l0 = or_164 (byte_eq_181 (ascii_space_190 ()), or_164 (byte_eq_181 (ascii_newline_191 ()), byte_eq_181 (ascii_tab_192 ()))) in map_160 (l0, (\ l_0 -> ()))


spaces_202 :: () -> (Parse (()))
spaces_202 _ =
  skip_many0_167 (space_201 ())


spaced_203 :: (Parse a) -> (Parse a)
spaced_203 l0 =
  skip_pre_162 (spaces_202 (), skip_post_163 (l0, spaces_202 ()))


operator_chain_205 :: ((Parse Expr), (Parse ((((Expr, Expr)) -> Expr)))) -> (Parse Expr)
operator_chain_205 (l0, l1) =
  and_then_161 (l0, (\ l2 -> many0_fold_165 (and_then_161 (spaced_203 l1, (\ l3 -> and_then_161 (l0, (\ l4 -> pure_157 (l3, l4))))), l2, (\ (l3, (l4, l5)) -> l4 (l3, l5)))))


mul_level_operator_206 :: () -> (Parse ((((Expr, Expr)) -> Expr)))
mul_level_operator_206 _ =
  or_164 ((let l0 = byte_eq_181 (ascii_asterisk_193 ()) in map_160 (l0, (\ l_0 -> \ l1 -> Mul l1))), (let l0 = byte_eq_181 (ascii_slash_194 ()) in map_160 (l0, (\ l_0 -> \ l1 -> Div l1))))


add_level_operator_208 :: () -> (Parse ((((Expr, Expr)) -> Expr)))
add_level_operator_208 _ =
  or_164 ((let l0 = byte_eq_181 (ascii_plus_195 ()) in map_160 (l0, (\ l_0 -> \ l1 -> Add l1))), (let l0 = byte_eq_181 (ascii_minus_196 ()) in map_160 (l0, (\ l_0 -> \ l1 -> Sub l1))))


expr_210 :: () -> (Parse Expr)
expr_210 _ =
  spaced_203 (add_level_expr_209 ())
add_level_expr_209 _ =
  operator_chain_205 (mul_level_expr_207 (), add_level_operator_208 ())
mul_level_expr_207 _ =
  operator_chain_205 (atomic_expr_204 (), mul_level_operator_206 ())
atomic_expr_204 _ =
  or_164 (map_160 (int_200 (), (\ l0 -> Const l0)), lazy_159 (\ () -> skip_pre_162 (byte_eq_181 (ascii_open_paren_197 ()), skip_post_163 (spaced_203 (expr_210 ()), byte_eq_181 (ascii_close_paren_198 ())))))


eval_exprs_211 :: (Iter (Vector Word8) ) -> (Vector Int64) 
eval_exprs_211 l0 =
  let l1 = (let l1 = l0 in map_23 (l1, (\ l2 -> let l3 = (let l3 = (let l3 = l2 in parse_all_156 (l3, expr_210 ())) in unwrap_10 l3) in eval_187 l3))) in from_iter_75 l1


read_input_rec_212 :: (Vector ((Vector Word8) ))  -> (Vector ((Vector Word8) )) 
read_input_rec_212 l0 =
  let l1 = input () in (case uncurry (==) ((len_48 ()) l1, 0) of True -> l0; False -> read_input_rec_212 ((push_49 ()) (l0, l1)))


read_input_213 :: () -> (Vector ((Vector Word8) )) 
read_input_213 () =
  read_input_rec_212 ((V.fromList []))


main_214 :: () -> ()
main_214 () =
  case string_to_nat_99 (input ()) of Some l0 -> (let l1 = read_input_213 () in (case repeat_186 (l0, (\ () -> eval_exprs_211 (items_19 l1))) of Some l2 -> (let l3 = items_19 l2 in for_each_43 (l3, (\ l4 -> writeln_108 (int_to_string_95 l4)))); None -> ())); None -> writeln_108 ((V.fromList [80, 108, 101, 97, 115, 101, 32, 101, 110, 116, 101, 114, 32, 97, 110, 32, 105, 116, 101, 114, 97, 116, 105, 111, 110, 32, 99, 111, 117, 110, 116]))



main :: IO ()
main = main_214 () `seq` return ()
