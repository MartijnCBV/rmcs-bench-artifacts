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

data Order
  = Equal
  | GThan
  | LThan

data ORD a
  = ORD ((((a, a)) -> Order))

data Result a b
  = Ok a
  | Err b

data SHOW a
  = SHOW ((a -> (Vector Word8) ))

data Mt19937_64
  = Mt19937_64 ((Int64, (Vector Int64) ))

data State a b
  = State ((a -> (a, b)))

data BadGood a b
  = Bad a
  | Good b

data SourcePos
  = SourcePos (((Vector Word8) , Int64, Int64))

data Message
  = SysUnExpect (Vector Word8) 
  | UnExpect (Vector Word8) 
  | Expect (Vector Word8) 
  | Message (Vector Word8) 

data ParseError
  = ParseError ((SourcePos, (Vector Message) ))

data Consumed a
  = Consumed a
  | Empty a

data ParseState
  = ParseState (((Iter Word8), SourcePos, Int64))

data ParsecT a
  = ParsecT ((ParseState -> (Consumed (Result ((a, ParseState, ParseError)) ParseError))))

data Entry a b
  = Entry ((a, b))

data Bucket a b
  = Bucket (Vector ((Entry a b))) 

data HashMap a b
  = HashMap ((Int64, (a -> Int64), (((a, a)) -> Bool), (Vector ((Bucket a b))) , Int64))

data Type
  = Dummy
  | Integer
  | Boolean
  | Func ((Type, Type))

data MinHS
  = HNum Int64
  | HBool Bool
  | HApply ((MinHS, MinHS))
  | HIf ((MinHS, MinHS, MinHS))
  | HLet ((MinHS, (MinHS -> MinHS)))
  | HLam ((MinHS -> MinHS))
  | HFun ((MinHS -> (MinHS -> MinHS)))

data Operator
  = Neg
  | Not
  | Add
  | Mul
  | And
  | Or
  | Eq
  | Lt

data NamedExpression
  = NVar (Vector Word8) 
  | NNum Int64
  | NBool Bool
  | NOp Operator
  | NApply ((NamedExpression, NamedExpression))
  | NLet (((Vector Word8) , Type, NamedExpression, NamedExpression))
  | NFun (((Vector Word8) , (Vector Word8) , Type, NamedExpression))
  | NIf ((NamedExpression, NamedExpression, NamedExpression))

data Expression
  = EVar Int64
  | ENum Int64
  | EBool Bool
  | EOp Operator
  | EApply ((Expression, Expression))
  | ELet ((Int64, Type, Expression, Expression))
  | EFun ((Int64, Int64, Type, Expression))
  | EIf ((Expression, Expression, Expression))

is_some_0 :: (Option a) -> Bool
is_some_0 l0 =
  case l0 of (Some l1) -> True; (None) -> False


is_none_1 :: (Option a) -> Bool
is_none_1 l0 =
  case l0 of (Some l1) -> False; (None) -> True


or_default_2 :: ((Option a), a) -> a
or_default_2 (l0, l1) =
  case l0 of (Some l2) -> l2; (None) -> l1


map_3 :: ((Option a), (a -> b)) -> (Option b)
map_3 (l0, l1) =
  case l0 of (Some l2) -> Some (l1 l2); (None) -> None


and_then_4 :: ((Option a), (a -> (Option b))) -> (Option b)
and_then_4 (l0, l1) =
  case l0 of (Some l2) -> l1 l2; (None) -> None


or_else_5 :: ((Option a), ((()) -> (Option a))) -> (Option a)
or_else_5 (l0, l1) =
  case l0 of (Some l2) -> Some l2; (None) -> l1 ()


identity_6 :: a -> a
identity_6 l0 =
  l0


flatten_7 :: (Option (Option a)) -> (Option a)
flatten_7 l0 =
  and_then_4 (l0, identity_6)


both_8 :: ((Option a), (Option b)) -> (Option ((a, b)))
both_8 (l0, l1) =
  case (l0, l1) of ((Some l2), (Some l3)) -> Some (l2, l3); l_0 -> None


either_9 :: ((Option a), (Option a)) -> (Option a)
either_9 (l0, l1) =
  case (l0, l1) of ((Some l2), l_1) -> Some l2; (l_2, (Some l2)) -> Some l2; ((None), (None)) -> None


unwrap_10 :: (Option a) -> a
unwrap_10 l0 =
  case l0 of (Some l1) -> l1; (None) -> panic ((V.fromList [67, 97, 110, 110, 111, 116, 32, 99, 97, 108, 108, 32, 39, 79, 112, 116, 105, 111, 110, 46, 117, 110, 119, 114, 97, 112, 39, 32, 111, 110, 32, 97, 32, 39, 78, 111, 110, 101, 39, 32, 118, 97, 108, 117, 101, 10]))


concat_from_11 :: ((Vector a) , (Vector a) , Int64) -> (Vector a) 
concat_from_11 (l0, l1, l2) =
  case uncurry (==) (l2, intrinsicLen l1) of True -> l0; False -> concat_from_11 (uncurry intrinsicPush (l0, uncurry intrinsicGet (l1, l2)), l1, uncurry (+) (l2, 1))


concat_12 :: ((Vector a) , (Vector a) ) -> (Vector a) 
concat_12 (l0, l1) =
  concat_from_11 (l0, l1, 0)


str_13 :: ((Option a), (a -> (Vector Word8) )) -> (Vector Word8) 
str_13 (l0, l1) =
  case l0 of (Some l2) -> concat_12 (concat_12 ((V.fromList [83, 111, 109, 101, 40]), l1 l2), (V.fromList [41])); (None) -> (V.fromList [78, 111, 110, 101])


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


next_34 :: (Iter a) -> (Option ((a, (Iter a))))
next_34 l0 =
  let (Iter l1) = l0 in l1 ()


chain_lazy_21 :: ((Iter a), ((()) -> (Iter a))) -> (Iter a)
chain_lazy_21 (l0, l1) =
  Iter (\ () -> case next_34 l0 of (Some (l2, l3)) -> Some (l2, chain_lazy_21 (l3, l1)); (None) -> next_34 (l1 ()))


chain_22 :: ((Iter a), (Iter a)) -> (Iter a)
chain_22 (l0, l1) =
  chain_lazy_21 (l0, (\ () -> l1))


map_23 :: ((Iter a), (a -> b)) -> (Iter b)
map_23 (l0, l1) =
  Iter (\ () -> case next_34 l0 of (Some (l2, l3)) -> Some (l1 l2, map_23 (l3, l1)); (None) -> None)


items_19 :: (Vector a)  -> (Iter a)
items_19 l0 =
  map_23 (range_17 (0, intrinsicLen l0), (\ l1 -> uncurry intrinsicGet (l0, l1)))


flatten_24 :: (Iter (Iter a)) -> (Iter a)
flatten_24 l0 =
  Iter (\ () -> case next_34 l0 of (Some (l1, l2)) -> next_34 (chain_lazy_21 (l1, (\ () -> flatten_24 l2))); (None) -> None)


flat_map_25 :: ((Iter a), (a -> (Iter b))) -> (Iter b)
flat_map_25 (l0, l1) =
  flatten_24 (map_23 (l0, l1))


filter_26 :: ((Iter a), (a -> Bool)) -> (Iter a)
filter_26 (l0, l1) =
  Iter (\ () -> case next_34 l0 of (Some (l2, l3)) -> (case l1 l2 of True -> Some (l2, filter_26 (l3, l1)); False -> next_34 (filter_26 (l3, l1))); (None) -> None)


take_27 :: ((Iter a), Int64) -> (Iter a)
take_27 (l0, l1) =
  case uncurry (>) (l1, 0) of True -> Iter (\ () -> case next_34 l0 of (Some (l2, l3)) -> Some (l2, take_27 (l3, uncurry (-) (l1, 1))); (None) -> None); False -> empty_14 ()


take_while_28 :: ((Iter a), (a -> Bool)) -> (Iter a)
take_while_28 (l0, l1) =
  Iter (\ () -> case next_34 l0 of (Some (l2, l3)) -> (case l1 l2 of True -> Some (l2, take_while_28 (l3, l1)); False -> None); (None) -> None)


skip_29 :: ((Iter a), Int64) -> (Iter a)
skip_29 (l0, l1) =
  case uncurry (==) (l1, 0) of True -> l0; False -> Iter (\ () -> case next_34 l0 of (Some (l2, l3)) -> next_34 (skip_29 (l3, uncurry (-) (l1, 1))); (None) -> None)


skip_while_30 :: ((Iter a), (a -> Bool)) -> (Iter a)
skip_while_30 (l0, l1) =
  Iter (\ () -> case next_34 l0 of (None) -> None; (Some (l2, l3)) -> (case l1 l2 of True -> next_34 (skip_while_30 (l3, l1)); False -> Some (l2, l3)))


split_32 :: ((Iter a), (a -> Bool)) -> ((Iter a), (Iter a))
split_32 (l0, l1) =
  let l2 = take_while_28 (l0, (\ l2 -> l1 l2)); l3 = skip_while_30 (l0, (\ l3 -> l1 l3)) in (l2, l3)


zip_31 :: ((Iter a), (Iter b)) -> (Iter ((a, b)))
zip_31 (l0, l1) =
  Iter (\ () -> case (next_34 l0, next_34 l1) of ((Some (l2, l3)), (Some (l4, l5))) -> Some ((l2, l4), zip_31 (l3, l5)); l_0 -> None)


enumerate_33 :: (Iter a) -> (Iter ((Int64, a)))
enumerate_33 l0 =
  zip_31 (ints_18 0, l0)


foldl_35 :: ((Iter a), b, (((b, a)) -> b)) -> b
foldl_35 (l0, l1, l2) =
  case next_34 l0 of (Some (l3, l4)) -> foldl_35 (l4, l2 (l1, l3), l2); (None) -> l1


foldr_36 :: ((Iter a), b, (((a, b)) -> b)) -> b
foldr_36 (l0, l1, l2) =
  case next_34 l0 of (Some (l3, l4)) -> l2 (l3, foldr_36 (l4, l1, l2)); (None) -> l1


sum_37 :: (Iter Int64) -> Int64
sum_37 l0 =
  foldl_35 (l0, 0, (\ (l1, l2) -> uncurry (+) (l1, l2)))


count_38 :: (Iter a) -> Int64
count_38 l0 =
  sum_37 (map_23 (l0, (\ l_1 -> 1)))


any_39 :: ((Iter a), (a -> Bool)) -> Bool
any_39 (l0, l1) =
  case next_34 l0 of (Some (l2, l3)) -> (case l1 l2 of True -> True; False -> any_39 (l3, l1)); (None) -> False


all_40 :: ((Iter a), (a -> Bool)) -> Bool
all_40 (l0, l1) =
  case next_34 l0 of (Some (l2, l3)) -> (case l1 l2 of True -> all_40 (l3, l1); False -> False); (None) -> True


equal_41 :: ((Iter a), (Iter a), (((a, a)) -> Bool)) -> Bool
equal_41 (l0, l1, l2) =
  case (next_34 l0, next_34 l1) of ((None), (None)) -> True; ((Some (l3, l4)), (Some (l5, l6))) -> (case l2 (l3, l5) of True -> equal_41 (l4, l6, l2); False -> False); (l_2, l_3) -> False


find_42 :: ((Iter a), (a -> Bool)) -> (Option a)
find_42 (l0, l1) =
  case next_34 l0 of (Some (l2, l3)) -> (case l1 l2 of True -> Some l2; False -> find_42 (l3, l1)); (None) -> None


find_map_43 :: ((Iter a), (a -> (Option b))) -> (Option b)
find_map_43 (l0, l1) =
  case next_34 l0 of (Some (l2, l3)) -> (case l1 l2 of (Some l4) -> Some l4; (None) -> find_map_43 (l3, l1)); (None) -> None


join_44 :: ((Iter (Iter a)), (Iter a)) -> (Iter a)
join_44 (l0, l1) =
  Iter (\ () -> case next_34 l0 of (Some (l2, l3)) -> next_34 (chain_lazy_21 (l2, (\ () -> chain_lazy_21 (l1, (\ () -> join_44 (l3, l1)))))); (None) -> None)


find_index_45 :: ((Iter a), (a -> Bool)) -> (Option Int64)
find_index_45 (l0, l1) =
  find_map_43 (enumerate_33 l0, (\ (l2, l3) -> case l1 l3 of True -> Some l2; False -> None))


for_each_46 :: ((Iter a), (a -> ())) -> ()
for_each_46 (l0, l1) =
  case next_34 l0 of (Some (l2, l3)) -> (let l_0 = l1 l2 in l_0 `seq` for_each_46 (l3, l1)); (None) -> ()


compare_47 :: ((ORD a), a, a) -> Order
compare_47 (l0, l1, l2) =
  let (ORD l3) = l0 in l3 (l1, l2)


ordInts_48 :: () -> (ORD Int64)
ordInts_48 _ =
  ORD (\ (l0, l1) -> case uncurry (==) (l0, l1) of True -> Equal; False -> (case uncurry (>) (l0, l1) of True -> GThan; False -> LThan))


ordByte_49 :: () -> (ORD Word8)
ordByte_49 _ =
  ORD (\ (l0, l1) -> case uncurry (==) (l0, l1) of True -> Equal; False -> (case uncurry (>) (l0, l1) of True -> GThan; False -> LThan))


get__50 :: () -> ((((Vector a) , Int64)) -> a)
get__50 _ =
  uncurry intrinsicGet


len__51 :: () -> ((Vector a)  -> Int64)
len__51 _ =
  intrinsicLen


push__52 :: () -> ((((Vector a) , a)) -> (Vector a) )
push__52 _ =
  uncurry intrinsicPush


get_53 :: () -> ((((Vector a) , Int64)) -> a)
get_53 _ =
  get__50 ()


len_54 :: () -> ((Vector a)  -> Int64)
len_54 _ =
  len__51 ()


push_55 :: () -> ((((Vector a) , a)) -> (Vector a) )
push_55 _ =
  push__52 ()


set_56 :: ((Vector a) , Int64, a) -> (Vector a) 
set_56 (l0, l1, l2) =
  let (l_0, l3) = uncurry intrinsicExtract (l0, l1) in l_0 `seq` l3 l2


update_57 :: ((Vector a) , Int64, (a -> a)) -> (Vector a) 
update_57 (l0, l1, l2) =
  let (l3, l4) = uncurry intrinsicExtract (l0, l1) in l4 (l2 l3)


try_get_58 :: ((Vector a) , Int64) -> (Option a)
try_get_58 (l0, l1) =
  case (case uncurry (>=) (l1, 0) of False -> False; True -> uncurry (<) (l1, (len_54 ()) l0)) of True -> Some ((get_53 ()) (l0, l1)); False -> None


concat_from_59 :: ((Vector a) , (Vector a) , Int64) -> (Vector a) 
concat_from_59 (l0, l1, l2) =
  case uncurry (==) (l2, (len_54 ()) l1) of True -> l0; False -> concat_from_59 ((push_55 ()) (l0, (get_53 ()) (l1, l2)), l1, uncurry (+) (l2, 1))


concat_60 :: ((Vector a) , (Vector a) ) -> (Vector a) 
concat_60 (l0, l1) =
  concat_from_59 (l0, l1, 0)


find_index_rec_61 :: ((Vector a) , (a -> Bool), Int64) -> (Option Int64)
find_index_rec_61 (l0, l1, l2) =
  case uncurry (>=) (l2, (len_54 ()) l0) of True -> None; False -> (case l1 ((get_53 ()) (l0, l2)) of True -> Some l2; False -> find_index_rec_61 (l0, l1, uncurry (+) (l2, 1)))


find_index_62 :: ((Vector a) , (a -> Bool)) -> (Option Int64)
find_index_62 (l0, l1) =
  find_index_rec_61 (l0, l1, 0)


all_rec_63 :: ((Vector a) , (a -> Bool), Int64) -> Bool
all_rec_63 (l0, l1, l2) =
  case uncurry (>=) (l2, (len_54 ()) l0) of True -> True; False -> (case not (l1 ((get_53 ()) (l0, l2))) of True -> False; False -> all_rec_63 (l0, l1, uncurry (+) (l2, 1)))


all_64 :: ((Vector a) , (a -> Bool)) -> Bool
all_64 (l0, l1) =
  all_rec_63 (l0, l1, 0)


foldl_rec_65 :: ((Vector a) , b, (((b, a)) -> b), Int64) -> b
foldl_rec_65 (l0, l1, l2, l3) =
  case uncurry (>=) (l3, (len_54 ()) l0) of True -> l1; False -> foldl_rec_65 (l0, l2 (l1, (get_53 ()) (l0, l3)), l2, uncurry (+) (l3, 1))


foldl_66 :: ((Vector a) , b, (((b, a)) -> b)) -> b
foldl_66 (l0, l1, l2) =
  foldl_rec_65 (l0, l1, l2, 0)


map_rec_68 :: ((Vector a) , (a -> b), Int64) -> (Vector b) 
map_rec_68 (l0, l1, l2) =
  case uncurry (<) (l2, 0) of True -> (V.fromList []); False -> (push_55 ()) (map_rec_68 (l0, l1, uncurry (-) (l2, 1)), l1 ((get_53 ()) (l0, l2)))


is_empty_69 :: (Vector a)  -> Bool
is_empty_69 l0 =
  uncurry (==) ((len_54 ()) l0, 0)


reduce_67 :: ((Vector a) , (((a, a)) -> a)) -> a
reduce_67 (l0, l1) =
  case is_empty_69 l0 of True -> panic ((V.fromList [99, 97, 110, 110, 111, 116, 32, 114, 101, 100, 117, 99, 101, 32, 101, 109, 112, 116, 121, 32, 97, 114, 114, 97, 121])); False -> foldl_rec_65 (l0, (get_53 ()) (l0, 0), l1, 1)


map_70 :: ((Vector a) , (a -> b)) -> (Vector b) 
map_70 (l0, l1) =
  map_rec_68 (l0, l1, uncurry (-) ((len_54 ()) l0, 1))


slice_rec_71 :: ((Vector a) , Int64, Int64, Int64) -> (Vector a) 
slice_rec_71 (l0, l1, l2, l3) =
  case uncurry (<) (l3, l1) of True -> (V.fromList []); False -> (push_55 ()) (slice_rec_71 (l0, l1, l2, uncurry (-) (l3, 1)), (get_53 ()) (l0, l3))


slice_72 :: ((Vector a) , Int64, Int64) -> (Vector a) 
slice_72 (l0, l1, l2) =
  case (case (case (case (case uncurry (<) (l1, 0) of True -> True; False -> uncurry (<) (l2, 0)) of True -> True; False -> uncurry (>=) (l1, (len_54 ()) l0)) of True -> True; False -> uncurry (>) (l2, (len_54 ()) l0)) of True -> True; False -> uncurry (<=) (l2, l1)) of True -> (V.fromList []); False -> slice_rec_71 (l0, l1, l2, uncurry (-) (l2, 1))


zip_rec_77 :: ((Vector a) , (Vector b) , Int64) -> (Vector ((a, b))) 
zip_rec_77 (l0, l1, l2) =
  case uncurry (<) (l2, 0) of True -> (V.fromList []); False -> (let (l3, l4) = ((get_53 ()) (l0, l2), (get_53 ()) (l1, l2)) in (push_55 ()) (zip_rec_77 (l0, l1, uncurry (-) (l2, 1)), (l3, l4)))


zip_78 :: ((Vector a) , (Vector b) ) -> (Vector ((a, b))) 
zip_78 (l0, l1) =
  let l2 = uncurry (-) ((case uncurry (<) ((len_54 ()) l0, (len_54 ()) l1) of True -> (len_54 ()) l0; False -> (len_54 ()) l1), 1) in (case uncurry (<) (l2, 0) of True -> (V.fromList []); False -> zip_rec_77 (l0, l1, l2))


push_front_79 :: (a, (Vector a) ) -> (Vector a) 
push_front_79 (l0, l1) =
  concat_60 ((V.fromList [l0]), l1)


equal_rec_80 :: ((Vector a) , (Vector a) , (((a, a)) -> Bool), Int64) -> Bool
equal_rec_80 (l0, l1, l2, l3) =
  case uncurry (>=) (l3, (len_54 ()) l0) of True -> True; False -> (case uncurry (>=) (l3, (len_54 ()) l1) of True -> False; False -> (case not (l2 ((get_53 ()) (l0, l3), (get_53 ()) (l1, l3))) of True -> False; False -> equal_rec_80 (l0, l1, l2, uncurry (+) (l3, 1))))


equal_81 :: ((Vector a) , (Vector a) , (((a, a)) -> Bool)) -> Bool
equal_81 (l0, l1, l2) =
  case not (uncurry (==) ((len_54 ()) l0, (len_54 ()) l1)) of True -> False; False -> equal_rec_80 (l0, l1, l2, 0)


fill_with_rec_82 :: ((Vector a) , Int64, ((()) -> a)) -> (Vector a) 
fill_with_rec_82 (l0, l1, l2) =
  case uncurry (>) (l1, 0) of True -> fill_with_rec_82 ((push_55 ()) (l0, l2 ()), uncurry (-) (l1, 1), l2); False -> l0


fill_with_83 :: (Int64, ((()) -> a)) -> (Vector a) 
fill_with_83 (l0, l1) =
  fill_with_rec_82 (uncurry intrinsicReserve ((V.fromList []), l0), l0, l1)


fill_84 :: (Int64, a) -> (Vector a) 
fill_84 (l0, l1) =
  fill_with_83 (l0, (\ () -> l1))


from_iter_with_capacity_85 :: ((Iter a), Int64) -> (Vector a) 
from_iter_with_capacity_85 (l0, l1) =
  foldl_35 (l0, uncurry intrinsicReserve ((V.fromList []), l1), push_55 ())


from_iter_86 :: (Iter a) -> (Vector a) 
from_iter_86 l0 =
  from_iter_with_capacity_85 (l0, 0)


span_rec_73 :: ((Iter a), (a -> Bool), (Vector a) ) -> ((Vector a) , (Vector a) )
span_rec_73 (l0, l1, l2) =
  case next_34 l0 of (None) -> (l2, (V.fromList [])); (Some (l3, l4)) -> (case l1 l3 of True -> span_rec_73 (l4, l1, (push_55 ()) (l2, l3)); False -> (l2, from_iter_86 l0))


span_74 :: ((Vector a) , (a -> Bool)) -> ((Vector a) , (Vector a) )
span_74 (l0, l1) =
  span_rec_73 (items_19 l0, l1, (V.fromList []))


reverse_rec_87 :: ((Vector a) , Int64, (Vector a) ) -> (Vector a) 
reverse_rec_87 (l0, l1, l2) =
  case uncurry (<) (l1, 0) of True -> l2; False -> reverse_rec_87 (l0, uncurry (-) (l1, 1), (push_55 ()) (l2, (get_53 ()) (l0, l1)))


reverse_88 :: (Vector a)  -> (Vector a) 
reverse_88 l0 =
  reverse_rec_87 (l0, uncurry (-) ((len_54 ()) l0, 1), (V.fromList []))


last_75 :: (Vector a)  -> (Option a)
last_75 l0 =
  case next_34 (items_19 (reverse_88 l0)) of (None) -> None; (Some (l1, l_0)) -> Some l1


init_76 :: (Vector a)  -> (Option (Vector a) )
init_76 l0 =
  case next_34 (items_19 (reverse_88 l0)) of (None) -> None; (Some (l_1, l1)) -> Some (reverse_88 (from_iter_86 l1))


str_89 :: ((Vector a) , (a -> (Vector Word8) )) -> (Vector Word8) 
str_89 (l0, l1) =
  let l2 = uncurry (-) ((len_54 ()) l0, 1) in (let l3 = (let l3 = (let l3 = (let l3 = items_19 l0 in take_27 (l3, l2)) in foldl_35 (l3, (V.fromList [91]), (\ (l4, l5) -> let l6 = (let l6 = l4 in concat_60 (l6, l1 l5)) in concat_60 (l6, (V.fromList [44, 32]))))) in concat_60 (l3, l1 ((get_53 ()) (l0, l2)))) in concat_60 (l3, (V.fromList [93])))


elem_90 :: (a, (Vector a) , (((a, a)) -> Bool)) -> Bool
elem_90 (l0, l1, l2) =
  foldl_66 (l1, False, (\ (l3, l4) -> case l3 of True -> True; False -> l2 (l0, l4)))


insertBy_92 :: ((ORD a), a, (Vector a) ) -> (Vector a) 
insertBy_92 (l0, l1, l2) =
  case is_empty_69 l2 of True -> (push_55 ()) (l2, l1); False -> (let l3 = slice_72 (l2, 1, (len_54 ()) l2); l4 = (get_53 ()) (l2, 0); l5 = compare_47 (l0, l1, l4) in (case l5 of (GThan) -> push_front_79 (l4, insertBy_92 (l0, l1, l3)); l_0 -> push_front_79 (l1, l2)))


sortBy_91 :: ((ORD a), (Vector a) ) -> (Vector a) 
sortBy_91 (l0, l1) =
  foldl_66 (l1, (V.fromList []), (\ (l2, l3) -> insertBy_92 (l0, l3, l2)))


nubIter_94 :: ((Iter a), (((a, a)) -> Bool)) -> (Vector a) 
nubIter_94 (l0, l1) =
  case next_34 l0 of (Some (l2, l3)) -> push_front_79 (l2, nubIter_94 (filter_26 (l3, (\ l4 -> not (l1 (l2, l4)))), l1)); (None) -> from_iter_86 (empty_14 ())


nub_93 :: ((Vector a) , (((a, a)) -> Bool)) -> (Vector a) 
nub_93 (l0, l1) =
  nubIter_94 (items_19 l0, l1)


is_ok_95 :: (Result a b) -> Bool
is_ok_95 l0 =
  case l0 of (Ok l1) -> True; (Err l_1) -> False


is_error_96 :: (Result a b) -> Bool
is_error_96 l0 =
  case l0 of (Ok l_2) -> False; (Err l_3) -> True


or_default_97 :: ((Result a b), a) -> a
or_default_97 (l0, l1) =
  case l0 of (Ok l2) -> l2; (Err l_4) -> l1


map_98 :: ((Result a b), (a -> c)) -> (Result c b)
map_98 (l0, l1) =
  case l0 of (Ok l2) -> Ok (l1 l2); (Err l2) -> Err l2


and_then_99 :: ((Result a b), (a -> (Result c b))) -> (Result c b)
and_then_99 (l0, l1) =
  case l0 of (Ok l2) -> l1 l2; (Err l2) -> Err l2


or_else_100 :: ((Result a b), ((()) -> (Result a b))) -> (Result a b)
or_else_100 (l0, l1) =
  case l0 of (Ok l2) -> Ok l2; (Err l_5) -> l1 ()


identity_101 :: a -> a
identity_101 l0 =
  l0


flatten_102 :: (Result (Result a b) b) -> (Result a b)
flatten_102 l0 =
  and_then_99 (l0, identity_101)


both_103 :: ((Result a b), (Result c b)) -> (Result ((a, c)) b)
both_103 (l0, l1) =
  case (l0, l1) of ((Ok l2), (Ok l3)) -> Ok (l2, l3); ((Err l2), l_6) -> Err l2; (l_7, (Err l2)) -> Err l2


either_104 :: ((Result a b), (Result a b)) -> (Result a b)
either_104 (l0, l1) =
  case (l0, l1) of ((Ok l2), l_8) -> Ok l2; (l_9, (Ok l2)) -> Ok l2; ((Err l2), (Err l_10)) -> Err l2


unwrap_105 :: (Result a b) -> a
unwrap_105 l0 =
  case l0 of (Ok l1) -> l1; (Err l_11) -> panic ((V.fromList [67, 97, 110, 110, 111, 116, 32, 99, 97, 108, 108, 32, 39, 82, 101, 115, 117, 108, 116, 46, 117, 110, 119, 114, 97, 112, 39, 32, 111, 110, 32, 97, 110, 32, 39, 69, 114, 114, 39, 32, 118, 97, 108, 117, 101, 10]))


str_106 :: ((Result a b), (a -> (Vector Word8) ), (b -> (Vector Word8) )) -> (Vector Word8) 
str_106 (l0, l1, l2) =
  case l0 of (Ok l3) -> concat_60 (concat_60 ((V.fromList [79, 107, 40]), l1 l3), (V.fromList [41])); (Err l3) -> concat_60 (concat_60 ((V.fromList [69, 114, 114, 40]), l2 l3), (V.fromList [41]))


ok_or_107 :: ((Option a), b) -> (Result a b)
ok_or_107 (l0, l1) =
  case l0 of (Some l2) -> Ok l2; (None) -> Err l1


all_ok_108 :: (Vector ((Result a b)))  -> (Result (Vector a)  b)
all_ok_108 l0 =
  foldl_66 (l0, Ok ((V.fromList [])), (\ (l1, l2) -> case (l1, l2) of ((Ok l3), (Ok l4)) -> Ok (concat_60 (l3, (V.fromList [l4]))); ((Err l3), l_12) -> Err l3; (l_13, (Err l3)) -> Err l3))


show_109 :: ((SHOW a), a) -> (Vector Word8) 
show_109 (l0, l1) =
  let (SHOW l2) = l0 in l2 l1


showChar_110 :: () -> (SHOW Word8)
showChar_110 _ =
  SHOW (\ l0 -> (V.fromList [l0]))


compareStrings_112 :: ((Iter Word8), (Iter Word8)) -> Order
compareStrings_112 (l0, l1) =
  case (next_34 l0, next_34 l1) of ((None), (None)) -> Equal; ((None), l_0) -> LThan; (l_1, (None)) -> GThan; ((Some (l2, l3)), (Some (l4, l5))) -> (case compare_47 (ordByte_49 (), l2, l4) of (GThan) -> GThan; (LThan) -> LThan; (Equal) -> compareStrings_112 (l3, l5))


ordString_111 :: () -> (ORD (Vector Word8) )
ordString_111 _ =
  ORD (\ (l0, l1) -> compareStrings_112 (items_19 l0, items_19 l1))


bool_to_string_113 :: Bool -> (Vector Word8) 
bool_to_string_113 l0 =
  case l0 of True -> (V.fromList [84, 114, 117, 101]); False -> (V.fromList [70, 97, 108, 115, 101])


nat_to_string_114 :: Int64 -> (Vector Word8) 
nat_to_string_114 l0 =
  let l1 = (\ l1 -> case l1 of 0 -> (V.fromList [48]); 1 -> (V.fromList [49]); 2 -> (V.fromList [50]); 3 -> (V.fromList [51]); 4 -> (V.fromList [52]); 5 -> (V.fromList [53]); 6 -> (V.fromList [54]); 7 -> (V.fromList [55]); 8 -> (V.fromList [56]); 9 -> (V.fromList [57]); l_0 -> (V.fromList [])) in (case uncurry (==) (l0, 0) of True -> (V.fromList []); False -> concat_60 (nat_to_string_114 (uncurry divv (l0, 10)), l1 (uncurry (-) (l0, uncurry (*) (uncurry divv (l0, 10), 10)))))


int_to_string_115 :: Int64 -> (Vector Word8) 
int_to_string_115 l0 =
  case uncurry (==) (l0, 0) of True -> (V.fromList [48]); False -> (case uncurry (<) (l0, 0) of True -> concat_60 ((V.fromList [45]), nat_to_string_114 (uncurry (-) (0, l0))); False -> nat_to_string_114 l0)


digit_ascii_base_116 :: () -> Word8
digit_ascii_base_116 _ =
  48


digit_to_nat_117 :: Word8 -> (Option Int64)
digit_to_nat_117 l0 =
  case l0 of 48 -> Some 0; 49 -> Some 1; 50 -> Some 2; 51 -> Some 3; 52 -> Some 4; 53 -> Some 5; 54 -> Some 6; 55 -> Some 7; 56 -> Some 8; 57 -> Some 9; l_1 -> None


chars_to_nat_118 :: (Iter Word8) -> (Option Int64)
chars_to_nat_118 l0 =
  case next_34 l0 of (None) -> None; (Some l_2) -> foldl_35 (l0, Some 0, (\ (l1, l2) -> and_then_4 (l1, (\ l3 -> map_3 (digit_to_nat_117 l2, (\ l4 -> uncurry (+) (uncurry (*) (l3, 10), l4)))))))


string_to_nat_119 :: (Vector Word8)  -> (Option Int64)
string_to_nat_119 l0 =
  chars_to_nat_118 (items_19 l0)


string_to_int_120 :: (Vector Word8)  -> (Option Int64)
string_to_int_120 l0 =
  case (let l1 = items_19 l0 in next_34 l1) of (Some (l1, l2)) -> (case uncurry (==) (l1, uncurry intrinsicGet ((V.fromList [45]), 0)) of True -> chars_to_nat_118 l2; False -> string_to_nat_119 l0); (None) -> None


equal_121 :: ((Vector Word8) , (Vector Word8) ) -> Bool
equal_121 (l0, l1) =
  equal_81 (l0, l1, (\ (l2, l3) -> uncurry (==) (l2, l3)))


concat_122 :: ((Vector Word8) , (Vector Word8) ) -> (Vector Word8) 
concat_122 (l0, l1) =
  concat_60 (l0, l1)


join_123 :: ((Vector ((Vector Word8) )) , (Vector Word8) ) -> (Vector Word8) 
join_123 (l0, l1) =
  let l2 = join_44 ((let l2 = items_19 l0 in map_23 (l2, items_19)), items_19 l1) in from_iter_86 l2


isSpace_124 :: Word8 -> Bool
isSpace_124 l0 =
  case l0 of 32 -> True; l_0 -> False


isUpper_125 :: Word8 -> Bool
isUpper_125 l0 =
  case l0 of 65 -> True; 66 -> True; 67 -> True; 68 -> True; 69 -> True; 70 -> True; 71 -> True; 72 -> True; 73 -> True; 74 -> True; 75 -> True; 76 -> True; 77 -> True; 78 -> True; 79 -> True; 80 -> True; 81 -> True; 82 -> True; 83 -> True; 84 -> True; 85 -> True; 86 -> True; 87 -> True; 88 -> True; 89 -> True; 90 -> True; l_1 -> False


isLower_126 :: Word8 -> Bool
isLower_126 l0 =
  case l0 of 97 -> True; 98 -> True; 99 -> True; 100 -> True; 101 -> True; 102 -> True; 103 -> True; 104 -> True; 105 -> True; 106 -> True; 107 -> True; 108 -> True; 109 -> True; 110 -> True; 111 -> True; 112 -> True; 113 -> True; 114 -> True; 115 -> True; 116 -> True; 117 -> True; 118 -> True; 119 -> True; 120 -> True; 121 -> True; 122 -> True; l_2 -> False


isDigit_127 :: Word8 -> Bool
isDigit_127 l0 =
  case l0 of 48 -> True; 49 -> True; 50 -> True; 51 -> True; 52 -> True; 53 -> True; 54 -> True; 55 -> True; 56 -> True; 57 -> True; l_3 -> False


isAlpha_128 :: Word8 -> Bool
isAlpha_128 l0 =
  case isLower_126 l0 of True -> True; False -> isUpper_125 l0


isAlphaNum_129 :: Word8 -> Bool
isAlphaNum_129 l0 =
  case isDigit_127 l0 of True -> True; False -> (case isLower_126 l0 of True -> True; False -> (case isUpper_125 l0 of True -> True; False -> False))


isHexDigit_130 :: Word8 -> Bool
isHexDigit_130 l0 =
  case isDigit_127 l0 of True -> True; False -> (case l0 of 65 -> True; 66 -> True; 67 -> True; 68 -> True; 69 -> True; 70 -> True; 97 -> True; 98 -> True; 99 -> True; 100 -> True; 101 -> True; 102 -> True; l_4 -> False)


isOctDigit_131 :: Word8 -> Bool
isOctDigit_131 l0 =
  case l0 of 48 -> True; 49 -> True; 50 -> True; 51 -> True; 52 -> True; 53 -> True; 54 -> True; 55 -> True; l_5 -> False


toLower_132 :: Word8 -> Word8
toLower_132 l0 =
  case l0 of 65 -> 97; 66 -> 98; 67 -> 99; 68 -> 100; 69 -> 101; 70 -> 102; 71 -> 103; 72 -> 104; 73 -> 105; 74 -> 106; 75 -> 107; 76 -> 108; 77 -> 109; 78 -> 110; 79 -> 111; 80 -> 112; 81 -> 113; 82 -> 114; 83 -> 115; 84 -> 116; 85 -> 117; 86 -> 118; 87 -> 119; 88 -> 120; 89 -> 121; 90 -> 122; l_6 -> l0


toUpper_133 :: Word8 -> Word8
toUpper_133 l0 =
  case l0 of 97 -> 65; 98 -> 66; 99 -> 67; 100 -> 68; 101 -> 69; 102 -> 70; 103 -> 71; 104 -> 72; 105 -> 73; 106 -> 74; 107 -> 75; 108 -> 76; 109 -> 77; 110 -> 78; 111 -> 79; 112 -> 80; 113 -> 81; 114 -> 82; 115 -> 83; 116 -> 84; 117 -> 85; 118 -> 86; 119 -> 87; 120 -> 88; 121 -> 89; 122 -> 90; l_7 -> l0


id_134 :: a -> a
id_134 l0 =
  l0


compose_135 :: ((a -> b), (b -> c)) -> (a -> c)
compose_135 (l0, l1) =
  \ l2 -> l1 (l0 l2)


const_136 :: a -> (b -> a)
const_136 l0 =
  \ l_8 -> l0


iterate_137 :: ((a -> a), Int64) -> (a -> a)
iterate_137 (l0, l1) =
  case uncurry (<=) (l1, 0) of True -> id_134; False -> compose_135 (l0, iterate_137 (l0, uncurry (-) (l1, 1)))


writeln_138 :: (Vector Word8)  -> ()
writeln_138 l0 =
  let l_0 = output l0 in l_0 `seq` output ((V.fromList [10]))


max_int_139 :: () -> Int64
max_int_139 _ =
  2305843009213693951


cmp_max_140 :: (Int64, Int64) -> Int64
cmp_max_140 (l0, l1) =
  case uncurry (>) (l0, l1) of True -> l0; False -> l1


cmp_min_141 :: (Int64, Int64) -> Int64
cmp_min_141 (l0, l1) =
  case uncurry (<) (l0, l1) of True -> l0; False -> l1


int_bit_not_142 :: Int64 -> Int64
int_bit_not_142 l0 =
  uncurry (-) (negate l0, 1)


rem_143 :: (Int64, Int64) -> Int64
rem_143 (l0, l1) =
  uncurry (-) (l0, uncurry (*) (l1, uncurry divv (l0, l1)))


mod_144 :: (Int64, Int64) -> Int64
mod_144 (l0, l1) =
  case uncurry (<) (l0, 0) of True -> uncurry (+) (rem_143 (l0, l1), l1); False -> rem_143 (l0, l1)


pow_rec_145 :: (Int64, Int64, Int64) -> Int64
pow_rec_145 (l0, l1, l2) =
  case uncurry (==) (l2, 0) of True -> l0; False -> pow_rec_145 (uncurry (*) (l1, l0), l1, uncurry (-) (l2, 1))


pow_146 :: (Int64, Int64) -> Int64
pow_146 (l0, l1) =
  pow_rec_145 (1, l0, l1)


byte_pow_rec_147 :: (Word8, Word8, Word8) -> Word8
byte_pow_rec_147 (l0, l1, l2) =
  case uncurry (==) (l2, 0) of True -> l0; False -> byte_pow_rec_147 (uncurry (*) (l1, l0), l1, uncurry (-) (l2, 1))


byte_pow_148 :: (Word8, Word8) -> Word8
byte_pow_148 (l0, l1) =
  byte_pow_rec_147 (1, l0, l1)


float_pow_149 :: (Double, Int64) -> Double
float_pow_149 (l0, l1) =
  case uncurry (==) (l1, 0) of True -> 1.0; False -> (case uncurry (<) (l1, 0) of True -> uncurry divv (1.0, float_pow_149 (l0, negate l1)); False -> (case uncurry (==) (uncurry (.&.) (l1, 1), 0) of True -> float_pow_149 (uncurry (*) (l0, l0), uncurry shiftR_ (l1, 1)); False -> uncurry (*) (l0, float_pow_149 (uncurry (*) (l0, l0), uncurry shiftR_ (l1, 1)))))


int_to_float_rec_150 :: (Double, Int64) -> Double
int_to_float_rec_150 (l0, l1) =
  case uncurry (==) (l1, 0) of True -> l0; False -> (case uncurry (==) (uncurry (.&.) (l1, 1), 0) of True -> int_to_float_rec_150 (uncurry (*) (l0, 2.0), uncurry shiftR_ (l1, 1)); False -> int_to_float_rec_150 (uncurry (+) (uncurry (*) (l0, 2.0), 1.0), uncurry shiftR_ (l1, 1)))


int_to_float_151 :: Int64 -> Double
int_to_float_151 l0 =
  case uncurry (<) (l0, 0) of True -> negate (int_to_float_rec_150 (0.0, negate l0)); False -> int_to_float_rec_150 (0.0, l0)


do_while_152 :: ((a -> Bool), ((()) -> a)) -> ()
do_while_152 (l0, l1) =
  case l0 (l1 ()) of True -> do_while_152 (l0, l1); False -> ()


while_153 :: (a, (a -> Bool), ((()) -> a)) -> ()
while_153 (l0, l1, l2) =
  case l1 l0 of True -> do_while_152 (l1, l2); False -> ()


forever_154 :: ((()) -> ()) -> ()
forever_154 l0 =
  let l_0 = l0 () in l_0 `seq` forever_154 l0


coeff_w_155 :: () -> Int64
coeff_w_155 _ =
  64


coeff_n_156 :: () -> Int64
coeff_n_156 _ =
  312


coeff_m_157 :: () -> Int64
coeff_m_157 _ =
  156


coeff_r_158 :: () -> Int64
coeff_r_158 _ =
  31


coeff_a_159 :: () -> Int64
coeff_a_159 _ =
  negate 5403634167711393303


coeff_u_160 :: () -> Int64
coeff_u_160 _ =
  29


coeff_d_161 :: () -> Int64
coeff_d_161 _ =
  6148914691236517205


coeff_s_162 :: () -> Int64
coeff_s_162 _ =
  17


coeff_b_163 :: () -> Int64
coeff_b_163 _ =
  8202884508482404352


coeff_t_164 :: () -> Int64
coeff_t_164 _ =
  37


coeff_c_165 :: () -> Int64
coeff_c_165 _ =
  negate 2270628950310912


coeff_l_166 :: () -> Int64
coeff_l_166 _ =
  43


coeff_f_167 :: () -> Int64
coeff_f_167 _ =
  6364136223846793005


lower_mask_168 :: () -> Int64
lower_mask_168 _ =
  uncurry (-) (uncurry shiftL_ (1, coeff_r_158 ()), 1)


upper_mask_169 :: () -> Int64
upper_mask_169 _ =
  int_bit_not_142 (lower_mask_168 ())


seed_mt19937_64_rec_170 :: ((Vector Int64) , Int64, Int64, Int64) -> (Vector Int64) 
seed_mt19937_64_rec_170 (l0, l1, l2, l3) =
  case uncurry (>=) (l2, l3) of True -> l0; False -> (let l4 = uncurry (+) (uncurry (*) (coeff_f_167 (), uncurry xor (l1, uncurry shiftR_ (l1, uncurry (-) (coeff_w_155 (), 2)))), l2) in seed_mt19937_64_rec_170 (uncurry intrinsicPush (l0, l4), l4, uncurry (+) (l2, 1), l3))


seed_mt19937_64_171 :: Int64 -> Mt19937_64
seed_mt19937_64_171 l0 =
  Mt19937_64 (coeff_n_156 (), seed_mt19937_64_rec_170 ((V.fromList [l0]), l0, 1, coeff_n_156 ()))


mt19937_64_twist_rec_172 :: ((Vector Int64) , Int64, Int64) -> (Vector Int64) 
mt19937_64_twist_rec_172 (l0, l1, l2) =
  case uncurry (>=) (l1, l2) of True -> l0; False -> (let l3 = uncurry (+) (uncurry (.&.) ((get_53 ()) (l0, l1), upper_mask_169 ()), uncurry (.&.) ((get_53 ()) (l0, rem_143 (uncurry (+) (l1, 1), coeff_n_156 ())), lower_mask_168 ())); l4 = (case uncurry (==) (uncurry (.&.) (l3, 1), 0) of True -> uncurry shiftR_ (l3, 1); False -> uncurry xor (uncurry shiftR_ (l3, 1), coeff_a_159 ())); l5 = uncurry xor ((get_53 ()) (l0, rem_143 (uncurry (+) (l1, coeff_m_157 ()), coeff_n_156 ())), l4) in mt19937_64_twist_rec_172 (set_56 (l0, l1, l5), uncurry (+) (l1, 1), l2))


mt19937_64_twist_173 :: Mt19937_64 -> Mt19937_64
mt19937_64_twist_173 l0 =
  let (Mt19937_64 (l_0, l1)) = l0 in l_0 `seq` Mt19937_64 (0, mt19937_64_twist_rec_172 (l1, 0, coeff_n_156 ()))


mt19937_64_next_174 :: Mt19937_64 -> (Mt19937_64, Int64)
mt19937_64_next_174 l0 =
  let (Mt19937_64 (l1, l_0)) = l0; (Mt19937_64 (l2, l3)) = (case uncurry (>=) (l1, coeff_n_156 ()) of True -> mt19937_64_twist_173 l0; False -> l0); l4 = (get_53 ()) (l3, l2); l5 = uncurry xor (l4, uncurry (.&.) (uncurry shiftR_ (l4, coeff_u_160 ()), coeff_d_161 ())); l6 = uncurry xor (l5, uncurry (.&.) (uncurry shiftL_ (l5, coeff_s_162 ()), coeff_b_163 ())); l7 = uncurry xor (l6, uncurry (.&.) (uncurry shiftL_ (l6, coeff_t_164 ()), coeff_c_165 ())); l8 = uncurry xor (l7, uncurry shiftR_ (l7, coeff_l_166 ())) in l_0 `seq` (Mt19937_64 (uncurry (+) (l2, 1), l3), l8)


get_175 :: () -> (State a a)
get_175 _ =
  State (\ l0 -> (l0, l0))


set_176 :: a -> (State a (()))
set_176 l0 =
  State (\ l_1 -> (l0, ()))


modify_177 :: (a -> a) -> (State a (()))
modify_177 l0 =
  State (\ l1 -> (l0 l1, ()))


modify_ret_178 :: (a -> (a, b)) -> (State a b)
modify_ret_178 l0 =
  State l0


pure_179 :: a -> (State b a)
pure_179 l0 =
  State (\ l1 -> (l1, l0))


run_180 :: ((State a b), a) -> (a, b)
run_180 (l0, l1) =
  let (State l2) = l0 in l2 l1


bind_181 :: ((State a b), (b -> (State a c))) -> (State a c)
bind_181 (l0, l1) =
  State (\ l2 -> let (l3, l4) = run_180 (l0, l2) in run_180 (l1 l4, l3))


seq_182 :: ((State a b), (State a c)) -> (State a c)
seq_182 (l0, l1) =
  bind_181 (l0, (\ l_0 -> l1))


for_each_183 :: ((Iter a), (a -> (State b (())))) -> (State b (()))
for_each_183 (l0, l1) =
  State (\ l2 -> let l3 = foldl_35 (l0, l2, (\ (l3, l4) -> let (l5, l_0) = run_180 (l1 l4, l3) in l_0 `seq` l5)) in (l3, ()))


either_184 :: ((a -> b), (c -> b), (BadGood a c)) -> b
either_184 (l0, l1, l2) =
  case l2 of (Bad l3) -> l0 l3; (Good l3) -> l1 l3


leftsI_186 :: (Iter (BadGood a b)) -> (Vector a) 
leftsI_186 l0 =
  case next_34 l0 of (None) -> (V.fromList []); (Some (l1, l2)) -> (case l1 of (Bad l3) -> push_front_79 (l3, leftsI_186 l2); (Good l_1) -> leftsI_186 l2)


lefts_185 :: (Vector ((BadGood a b)))  -> (Vector a) 
lefts_185 l0 =
  leftsI_186 (items_19 l0)


rightsI_188 :: (Iter (BadGood a b)) -> (Vector b) 
rightsI_188 l0 =
  case next_34 l0 of (None) -> (V.fromList []); (Some (l1, l2)) -> (case l1 of (Bad l_2) -> rightsI_188 l2; (Good l3) -> push_front_79 (l3, rightsI_188 l2))


rights_187 :: (Vector ((BadGood a b)))  -> (Vector b) 
rights_187 l0 =
  rightsI_188 (items_19 l0)


is_left_189 :: (BadGood a b) -> Bool
is_left_189 l0 =
  case l0 of (Bad l_3) -> True; (Good l_4) -> False


is_right_190 :: (BadGood a b) -> Bool
is_right_190 l0 =
  case l0 of (Bad l_5) -> False; (Good l_6) -> True


from_left_191 :: (a, (BadGood a b)) -> a
from_left_191 (l0, l1) =
  case l1 of (Bad l2) -> l2; (Good l_7) -> l0


from_right_192 :: (a, (BadGood b a)) -> a
from_right_192 (l0, l1) =
  case l1 of (Bad l_8) -> l0; (Good l2) -> l2


partition_eithersI_194 :: ((Iter (BadGood a b)), (Vector a) , (Vector b) ) -> ((Vector a) , (Vector b) )
partition_eithersI_194 (l0, l1, l2) =
  case next_34 l0 of (None) -> (l1, l2); (Some (l3, l4)) -> (case l3 of (Bad l5) -> partition_eithersI_194 (l4, push_front_79 (l5, l1), l2); (Good l5) -> partition_eithersI_194 (l4, l1, push_front_79 (l5, l2)))


partition_eithers_193 :: (Vector ((BadGood a b)))  -> ((Vector a) , (Vector b) )
partition_eithers_193 l0 =
  partition_eithersI_194 (items_19 l0, (V.fromList []), (V.fromList []))


newPos_195 :: ((Vector Word8) , Int64, Int64) -> SourcePos
newPos_195 (l0, l1, l2) =
  SourcePos (l0, l1, l2)


initialPos_196 :: (Vector Word8)  -> SourcePos
initialPos_196 l0 =
  newPos_195 (l0, 1, 1)


sourceName_197 :: SourcePos -> (Vector Word8) 
sourceName_197 l0 =
  let (SourcePos (l1, l_0, l_1)) = l0 in l_0 `seq` l_1 `seq` l1


sourceLine_198 :: SourcePos -> Int64
sourceLine_198 l0 =
  let (SourcePos (l_0, l1, l_1)) = l0 in l_0 `seq` l_1 `seq` l1


sourceColumn_199 :: SourcePos -> Int64
sourceColumn_199 l0 =
  let (SourcePos (l_0, l_1, l1)) = l0 in l_0 `seq` l_1 `seq` l1


incSourceLine_200 :: (SourcePos, Int64) -> SourcePos
incSourceLine_200 (l0, l1) =
  let (SourcePos (l2, l3, l4)) = l0 in SourcePos (l2, uncurry (+) (l3, l1), l4)


incSourceColumn_201 :: (SourcePos, Int64) -> SourcePos
incSourceColumn_201 (l0, l1) =
  let (SourcePos (l2, l3, l4)) = l0 in SourcePos (l2, l3, uncurry (+) (l4, l1))


setSourceName_202 :: (SourcePos, (Vector Word8) ) -> SourcePos
setSourceName_202 (l0, l1) =
  let (SourcePos (l_0, l2, l3)) = l0 in l_0 `seq` SourcePos (l1, l2, l3)


setSourceLine_203 :: (SourcePos, Int64) -> SourcePos
setSourceLine_203 (l0, l1) =
  let (SourcePos (l2, l_0, l3)) = l0 in l_0 `seq` SourcePos (l2, l1, l3)


setSourceColumn_204 :: (SourcePos, Int64) -> SourcePos
setSourceColumn_204 (l0, l1) =
  let (SourcePos (l2, l3, l_0)) = l0 in l_0 `seq` SourcePos (l2, l3, l1)


updatePosChar_206 :: (SourcePos, Word8) -> SourcePos
updatePosChar_206 (l0, l1) =
  let (SourcePos (l2, l3, l4)) = l0 in (case l1 of 10 -> SourcePos (l2, uncurry (+) (l3, 1), 1); 9 -> SourcePos (l2, l3, uncurry (-) (uncurry (+) (l4, 8), mod_144 (uncurry (-) (l4, 1), 8))); l_0 -> SourcePos (l2, l3, uncurry (+) (l4, 1)))


updatePosString_205 :: (SourcePos, (Iter Word8)) -> SourcePos
updatePosString_205 (l0, l1) =
  foldl_35 (l1, l0, updatePosChar_206)


showPos_207 :: SourcePos -> (Vector Word8) 
showPos_207 l0 =
  let (SourcePos (l1, l2, l3)) = l0; l4 = (let l4 = (let l4 = (let l4 = (let l4 = (V.fromList [40, 108, 105, 110, 101, 32]) in concat_60 (l4, int_to_string_115 l2)) in concat_60 (l4, (V.fromList [44, 32, 99, 111, 108, 117, 109, 110, 32]))) in concat_60 (l4, int_to_string_115 l3)) in concat_60 (l4, (V.fromList [41]))) in (case is_empty_69 l1 of True -> l4; False -> (let l5 = (let l5 = (let l5 = (V.fromList [34]) in concat_60 (l5, l1)) in concat_60 (l5, (V.fromList [34]))) in concat_60 (l5, l4)))


main_208 :: () -> ()
main_208 () =
  output (concat_60 ((V.fromList [72, 101, 108, 108, 111, 44, 32]), (V.fromList [119, 111, 114, 108, 100, 33])))


getEnumIndex_209 :: Message -> Int64
getEnumIndex_209 l0 =
  case l0 of (SysUnExpect l_0) -> 0; (UnExpect l_1) -> 1; (Expect l_2) -> 2; (Message l_3) -> 3


messageString_210 :: Message -> (Vector Word8) 
messageString_210 l0 =
  case l0 of (SysUnExpect l1) -> l1; (UnExpect l1) -> l1; (Expect l1) -> l1; (Message l1) -> l1


ordMessage_211 :: () -> (ORD Message)
ordMessage_211 _ =
  ORD (\ (l0, l1) -> compare_47 (ordInts_48 (), getEnumIndex_209 l0, getEnumIndex_209 l1))


compareMessages_212 :: (Message, Message) -> Order
compareMessages_212 (l0, l1) =
  compare_47 (ordInts_48 (), getEnumIndex_209 l0, getEnumIndex_209 l1)


eqMessage_213 :: (Message, Message) -> Bool
eqMessage_213 (l0, l1) =
  uncurry (==) (getEnumIndex_209 l0, getEnumIndex_209 l1)


errorPos_214 :: ParseError -> SourcePos
errorPos_214 l0 =
  let (ParseError (l1, l_0)) = l0 in l_0 `seq` l1


errorMessages_215 :: ParseError -> (Vector Message) 
errorMessages_215 l0 =
  let (ParseError (l_0, l1)) = l0 in l_0 `seq` sortBy_91 (ordMessage_211 (), l1)


errorIsUnknown_216 :: ParseError -> Bool
errorIsUnknown_216 l0 =
  let (ParseError (l_0, l1)) = l0 in l_0 `seq` is_empty_69 l1


newErrorUnknown_217 :: SourcePos -> ParseError
newErrorUnknown_217 l0 =
  ParseError (l0, from_iter_86 (empty_14 ()))


newErrorMessage_218 :: (Message, SourcePos) -> ParseError
newErrorMessage_218 (l0, l1) =
  ParseError (l1, (V.fromList [l0]))


addErrorMessage_219 :: (Message, ParseError) -> ParseError
addErrorMessage_219 (l0, l1) =
  let (ParseError (l2, l3)) = l1 in ParseError (l2, push_front_79 (l0, l3))


setErrorPos_220 :: (SourcePos, ParseError) -> ParseError
setErrorPos_220 (l0, l1) =
  let (ParseError (l_0, l2)) = l1 in l_0 `seq` ParseError (l0, l2)


setErrorMessage_221 :: (Message, ParseError) -> ParseError
setErrorMessage_221 (l0, l1) =
  let (ParseError (l2, l3)) = l1 in ParseError (l2, push_front_79 (l0, from_iter_86 (filter_26 (items_19 l3, (\ l4 -> eqMessage_213 (l0, l4))))))


compareErrorPos_223 :: (SourcePos, SourcePos) -> Order
compareErrorPos_223 (l0, l1) =
  let (SourcePos (l_0, l2, l3)) = l0; (SourcePos (l_1, l4, l5)) = l1 in l_0 `seq` l_1 `seq` (case compare_47 (ordInts_48 (), l2, l4) of (Equal) -> compare_47 (ordInts_48 (), l3, l5); (GThan) -> GThan; (LThan) -> LThan)


mergeError_222 :: (ParseError, ParseError) -> ParseError
mergeError_222 (l0, l1) =
  let (ParseError (l2, l3)) = l0; (ParseError (l4, l5)) = l1 in (case (case is_empty_69 l5 of False -> False; True -> not (is_empty_69 l3)) of True -> l0; False -> (case (case is_empty_69 l3 of False -> False; True -> not (is_empty_69 l5)) of True -> l1; False -> (case compareErrorPos_223 (l2, l4) of (Equal) -> ParseError (l2, concat_60 (l3, l5)); (GThan) -> l0; (LThan) -> l1)))


separate_230 :: ((Vector Word8) , (Vector ((Vector Word8) )) ) -> (Vector Word8) 
separate_230 (l0, l1) =
  case next_34 (items_19 l1) of (None) -> (V.fromList []); (Some (l2, l3)) -> (case next_34 l3 of (None) -> l2; (Some (l_0, l_1)) -> (let l4 = (let l4 = l2 in concat_60 (l4, l0)) in concat_60 (l4, separate_230 (l0, from_iter_86 l3))))


clean_231 :: (Vector ((Vector Word8) ))  -> (Vector ((Vector Word8) )) 
clean_231 l0 =
  nub_93 (from_iter_86 (filter_26 (items_19 l0, (\ l1 -> not (is_empty_69 l1)))), equal_121)


commaSep_229 :: (Vector ((Vector Word8) ))  -> (Vector Word8) 
commaSep_229 l0 =
  separate_230 ((V.fromList [44, 32]), clean_231 l0)


commasOr_228 :: ((Vector Word8) , (Vector ((Vector Word8) )) ) -> (Vector Word8) 
commasOr_228 (l0, l1) =
  case next_34 (items_19 l1) of (None) -> (V.fromList []); (Some (l2, l3)) -> (case next_34 l3 of (None) -> l2; (Some (l_0, l_1)) -> (let l4 = (let l4 = (let l4 = (let l4 = commaSep_229 (unwrap_10 (init_76 (from_iter_86 l3))) in concat_60 (l4, (V.fromList [32]))) in concat_60 (l4, l0)) in concat_60 (l4, (V.fromList [32]))) in concat_60 (l4, unwrap_10 (last_75 (from_iter_86 l3)))))


showMany_227 :: ((Vector Word8) , (Vector Word8) , (Vector Message) ) -> (Vector Word8) 
showMany_227 (l0, l1, l2) =
  let l3 = clean_231 (map_70 (l2, messageString_210)) in (case is_empty_69 l3 of True -> (V.fromList []); False -> (case is_empty_69 l1 of True -> commasOr_228 (l0, l3); False -> (let l4 = (let l4 = l1 in concat_60 (l4, (V.fromList [32]))) in concat_60 (l4, commasOr_228 (l0, l3)))))


showMessages_226 :: ((Vector Word8) , (Vector Message) ) -> (Vector Word8) 
showMessages_226 (l0, l1) =
  showMany_227 (l0, (V.fromList []), l1)


showErrorMessages_225 :: ((Vector Word8) , (Vector Word8) , (Vector Word8) , (Vector Word8) , (Vector Word8) , (Vector Message) ) -> (Vector Word8) 
showErrorMessages_225 (l0, l1, l2, l3, l4, l5) =
  case is_empty_69 l5 of True -> l1; False -> (let (l6, l7) = span_74 (l5, (\ l6 -> eqMessage_213 (l6, SysUnExpect ((V.fromList []))))); (l8, l9) = span_74 (l7, (\ l8 -> eqMessage_213 (l8, UnExpect ((V.fromList []))))); (l10, l11) = span_74 (l9, (\ l10 -> eqMessage_213 (l10, Expect ((V.fromList []))))); l12 = showMany_227 (l0, l2, l10); l13 = showMany_227 (l0, l3, l8); l14 = (case not (is_empty_69 l8) of True -> (V.fromList []); False -> (case next_34 (items_19 l6) of (None) -> (V.fromList []); (Some (l14, l15)) -> (case is_empty_69 (messageString_210 l14) of True -> (let l16 = (let l16 = l3 in concat_60 (l16, (V.fromList [32]))) in concat_60 (l16, l4)); False -> (let l16 = (let l16 = l3 in concat_60 (l16, (V.fromList [32]))) in concat_60 (l16, messageString_210 l14))))) in foldl_66 (map_70 (clean_231 ((V.fromList [l14, l13, l12, showMessages_226 (l0, l11)])), (\ l15 -> concat_60 ((V.fromList [10]), l15))), (V.fromList []), concat_60))


showParseError_224 :: ParseError -> (Vector Word8) 
showParseError_224 l0 =
  let l1 = (let l1 = (let l1 = showPos_207 (errorPos_214 l0) in concat_60 (l1, (V.fromList [58]))) in concat_60 (l1, showErrorMessages_225 ((V.fromList [111, 114]), (V.fromList [117, 110, 107, 110, 111, 119, 110, 32, 112, 97, 114, 115, 101, 32, 101, 114, 114, 111, 114]), (V.fromList [101, 120, 112, 101, 99, 116, 105, 110, 103]), (V.fromList [117, 110, 101, 120, 112, 101, 99, 116, 101, 100]), (V.fromList [101, 110, 100, 32, 111, 102, 32, 105, 110, 112, 117, 116]), errorMessages_215 l0))) in concat_60 (l1, (V.fromList [10]))


handleCOK_233 :: (a, ParseState, ParseError) -> (Consumed (Result ((a, ParseState, ParseError)) ParseError))
handleCOK_233 (l0, l1, l2) =
  Consumed (Ok (l0, l1, l2))


handleCERR_234 :: ParseError -> (Consumed (Result ((a, ParseState, ParseError)) ParseError))
handleCERR_234 l0 =
  Consumed (Err l0)


handleEOK_235 :: (a, ParseState, ParseError) -> (Consumed (Result ((a, ParseState, ParseError)) ParseError))
handleEOK_235 (l0, l1, l2) =
  Empty (Ok (l0, l1, l2))


handleEERR_236 :: ParseError -> (Consumed (Result ((a, ParseState, ParseError)) ParseError))
handleEERR_236 l0 =
  Empty (Err l0)


sysUnExpectError_238 :: ((Vector Word8) , SourcePos) -> (Result ((a, ParseState, ParseError)) ParseError)
sysUnExpectError_238 (l0, l1) =
  Err (newErrorMessage_218 (SysUnExpect l0, l1))


unParser_240 :: ((ParsecT a), ParseState) -> (Consumed (Result ((a, ParseState, ParseError)) ParseError))
unParser_240 (l0, l1) =
  let (ParsecT l2) = l0 in l2 l1


lazy_232 :: ((()) -> (ParsecT a)) -> (ParsecT a)
lazy_232 l0 =
  ParsecT (\ l1 -> unParser_240 (l0 (), l1))


runParsecT_241 :: ((ParsecT a), ParseState) -> (Consumed (Result ((a, ParseState, ParseError)) ParseError))
runParsecT_241 (l0, l1) =
  let l2 = (\ (l2, l3, l4) -> Consumed (Ok (l2, l3, l4))); l3 = (\ l3 -> Consumed (Err l3)); l4 = (\ (l4, l5, l6) -> Empty (Ok (l4, l5, l6))); l5 = (\ l5 -> Empty (Err l5)) in unParser_240 (l0, l1)


mkPT_242 :: (ParseState -> (Consumed (Result ((a, ParseState, ParseError)) ParseError))) -> (ParsecT a)
mkPT_242 l0 =
  ParsecT (\ l1 -> case l0 l1 of (Consumed l2) -> (case l2 of (Ok (l3, l4, l5)) -> handleCOK_233 (l3, l4, l5); (Err l3) -> handleCERR_234 l3); (Empty l2) -> (case l2 of (Ok (l3, l4, l5)) -> handleEOK_235 (l3, l4, l5); (Err l3) -> handleEERR_236 l3))


stateInput_243 :: ParseState -> (Iter Word8)
stateInput_243 l0 =
  let (ParseState (l1, l_0, l_1)) = l0 in l_0 `seq` l_1 `seq` l1


statePos_244 :: ParseState -> SourcePos
statePos_244 l0 =
  let (ParseState (l_0, l1, l_1)) = l0 in l_0 `seq` l_1 `seq` l1


unknownError_237 :: ParseState -> ParseError
unknownError_237 l0 =
  newErrorUnknown_217 (statePos_244 l0)


unexpected_239 :: (Vector Word8)  -> (ParsecT a)
unexpected_239 l0 =
  ParsecT (\ l1 -> handleEERR_236 (newErrorMessage_218 (UnExpect l0, statePos_244 l1)))


stateUser_245 :: ParseState -> Int64
stateUser_245 l0 =
  let (ParseState (l_0, l_1, l1)) = l0 in l_0 `seq` l_1 `seq` l1


parsecMap_246 :: ((a -> b), (ParsecT a)) -> (ParsecT b)
parsecMap_246 (l0, l1) =
  ParsecT (\ l2 -> case unParser_240 (l1, l2) of (Consumed (Ok (l3, l4, l5))) -> Consumed (Ok (l0 l3, l4, l5)); (Consumed (Err l3)) -> Consumed (Err l3); (Empty (Ok (l3, l4, l5))) -> Empty (Ok (l0 l3, l4, l5)); (Empty (Err l3)) -> Empty (Err l3))


parserReturn_247 :: a -> (ParsecT a)
parserReturn_247 l0 =
  ParsecT (\ l1 -> handleEOK_235 (l0, l1, unknownError_237 l1))


parserBind_248 :: ((ParsecT a), (a -> (ParsecT b))) -> (ParsecT b)
parserBind_248 (l0, l1) =
  ParsecT (\ l2 -> case unParser_240 (l0, l2) of (Consumed (Ok (l3, l4, l5))) -> (case errorIsUnknown_216 l5 of True -> (case unParser_240 (l1 l3, l4) of (Consumed (Ok (l6, l7, l8))) -> Consumed (Ok (l6, l7, l8)); (Consumed (Err l6)) -> Consumed (Err l6); (Empty (Ok (l6, l7, l8))) -> Consumed (Ok (l6, l7, l8)); (Empty (Err l6)) -> Consumed (Err l6)); False -> (case unParser_240 (l1 l3, l4) of (Consumed (Ok (l6, l7, l8))) -> Consumed (Ok (l6, l7, l8)); (Consumed (Err l6)) -> Consumed (Err l6); (Empty (Ok (l6, l7, l8))) -> Consumed (Ok (l6, l7, mergeError_222 (l5, l8))); (Empty (Err l6)) -> Consumed (Err (mergeError_222 (l5, l6))))); (Consumed (Err l3)) -> Consumed (Err l3); (Empty (Ok (l3, l4, l5))) -> (case errorIsUnknown_216 l5 of True -> unParser_240 (l1 l3, l4); False -> (case unParser_240 (l1 l3, l4) of (Consumed (Ok (l6, l7, l8))) -> Consumed (Ok (l6, l7, l8)); (Consumed (Err l6)) -> Consumed (Err l6); (Empty (Ok (l6, l7, l8))) -> Empty (Ok (l6, l7, mergeError_222 (l5, l8))); (Empty (Err l6)) -> Empty (Err (mergeError_222 (l5, l6))))); (Empty (Err l3)) -> Empty (Err l3))


parserSequenceI_250 :: (Iter (ParsecT a)) -> (ParsecT (Vector a) )
parserSequenceI_250 l0 =
  case next_34 l0 of (None) -> parserReturn_247 ((V.fromList [])); (Some (l1, l2)) -> parserBind_248 (l1, (\ l3 -> parsecMap_246 ((\ l4 -> (push_55 ()) (l4, l3)), parserSequenceI_250 l2)))


parserSequence_249 :: (Vector ((ParsecT a)))  -> (ParsecT (Vector a) )
parserSequence_249 l0 =
  parserSequenceI_250 (items_19 l0)


mergeErrorReply_251 :: (ParseError, (Result ((a, ParseState, ParseError)) ParseError)) -> (Result ((a, ParseState, ParseError)) ParseError)
mergeErrorReply_251 (l0, l1) =
  case l1 of (Ok (l2, l3, l4)) -> Ok (l2, l3, mergeError_222 (l0, l4)); (Err l2) -> Err (mergeError_222 (l0, l2))


parserFail_252 :: (Vector Word8)  -> (ParsecT a)
parserFail_252 l0 =
  ParsecT (\ l1 -> handleEERR_236 (newErrorMessage_218 (Message l0, statePos_244 l1)))


parserZero_253 :: () -> (ParsecT a)
parserZero_253 _ =
  ParsecT (\ l0 -> handleEERR_236 (unknownError_237 l0))


parserPlus_254 :: ((ParsecT a), (ParsecT a)) -> (ParsecT a)
parserPlus_254 (l0, l1) =
  ParsecT (\ l2 -> case unParser_240 (l0, l2) of (Consumed (Ok (l3, l4, l5))) -> Consumed (Ok (l3, l4, l5)); (Consumed (Err l3)) -> Consumed (Err l3); (Empty (Ok (l3, l4, l5))) -> Empty (Ok (l3, l4, l5)); (Empty (Err l3)) -> (case unParser_240 (l1, l2) of (Consumed (Ok (l4, l5, l6))) -> Consumed (Ok (l4, l5, l6)); (Consumed (Err l4)) -> Consumed (Err l4); (Empty (Ok (l4, l5, l6))) -> Empty (Ok (l4, l5, mergeError_222 (l3, l6))); (Empty (Err l4)) -> Empty (Err (mergeError_222 (l3, l4)))))


tryOr_256 :: ((ParsecT a), (ParsecT a)) -> (ParsecT a)
tryOr_256 (l0, l1) =
  parserPlus_254 (l0, l1)


labels_258 :: ((ParsecT a), (Vector ((Vector Word8) )) ) -> (ParsecT a)
labels_258 (l0, l1) =
  let l2 = (\ (l2, l3) -> let l4 = items_19 l3 in (case next_34 l4 of (None) -> setErrorMessage_221 (Expect ((V.fromList [])), l2); (Some (l5, l6)) -> (case uncurry (==) (count_38 l6, 0) of True -> setErrorMessage_221 (Expect l5, l2); False -> foldr_36 (l6, setErrorMessage_221 (Expect l5, l2), (\ (l7, l8) -> addErrorMessage_219 (Expect l7, l8)))))) in ParsecT (\ l3 -> case unParser_240 (l0, l3) of (Consumed (Ok (l4, l5, l6))) -> Consumed (Ok (l4, l5, l6)); (Consumed (Err l4)) -> Consumed (Err l4); (Empty (Ok (l4, l5, l6))) -> (case errorIsUnknown_216 l6 of True -> Empty (Ok (l4, l5, l6)); False -> Empty (Ok (l4, l5, l2 (l6, l1)))); (Empty (Err l4)) -> Empty (Err (l2 (l4, l1))))


label_257 :: ((ParsecT a), (Vector Word8) ) -> (ParsecT a)
label_257 (l0, l1) =
  labels_258 (l0, (V.fromList [l1]))


withErr_255 :: ((ParsecT a), (Vector Word8) ) -> (ParsecT a)
withErr_255 (l0, l1) =
  label_257 (l0, l1)


uncons_261 :: (Iter Word8) -> (Option ((Word8, (Iter Word8))))
uncons_261 l0 =
  next_34 l0


walk_260 :: ((Iter Word8), (Iter Word8), Int64, (Iter Word8), SourcePos, (((SourcePos, (Iter Word8))) -> SourcePos), ParseError, (Word8 -> ParseError)) -> (Consumed (Result (((Vector Word8) , ParseState, ParseError)) ParseError))
walk_260 (l0, l1, l2, l3, l4, l5, l6, l7) =
  let l8 = (\ l8 -> let l9 = l5 (l4, l3); l10 = ParseState (l1, l9, l2) in handleCOK_233 (from_iter_86 l3, l10, newErrorUnknown_217 l9)) in (case next_34 l0 of (None) -> l8 l1; (Some (l9, l10)) -> (case uncons_261 l1 of (None) -> handleCERR_234 l6; (Some (l11, l12)) -> (case uncurry (==) (l9, l11) of True -> walk_260 (l10, l12, l2, l3, l4, l5, l6, l7); False -> handleCERR_234 (l7 l11))))


tokens_259 :: ((((Word8, Word8)) -> Bool), ((Iter Word8) -> (Vector Word8) ), (((SourcePos, (Iter Word8))) -> SourcePos), (Iter Word8)) -> (ParsecT (Vector Word8) )
tokens_259 (l0, l1, l2, l3) =
  case next_34 l3 of (None) -> ParsecT (\ l4 -> handleEOK_235 ((V.fromList []), l4, unknownError_237 l4)); (Some (l4, l5)) -> ParsecT (\ l6 -> let (ParseState (l7, l8, l9)) = l6; l10 = setErrorMessage_221 (Expect (l1 l3), newErrorMessage_218 (SysUnExpect ((V.fromList [])), l8)); l11 = (\ l11 -> setErrorMessage_221 (Expect (l1 l3), newErrorMessage_218 (SysUnExpect ((V.fromList [l11])), l8))) in (case uncons_261 l7 of (None) -> handleEERR_236 l10; (Some (l12, l13)) -> (case l0 (l4, l12) of True -> walk_260 (l5, l13, l9, l3, l8, l2, l10, l11); False -> handleEERR_236 (l11 l12))))


tokens1_262 :: (((Iter Word8) -> (Vector Word8) ), (((SourcePos, (Iter Word8))) -> SourcePos), (Iter Word8)) -> (ParsecT (Vector Word8) )
tokens1_262 (l0, l1, l2) =
  case next_34 l2 of (None) -> ParsecT (\ l3 -> handleEOK_235 ((V.fromList []), l3, unknownError_237 l3)); (Some (l3, l4)) -> ParsecT (\ l5 -> let (ParseState (l6, l7, l8)) = l5; l9 = setErrorMessage_221 (Expect (l0 l2), newErrorMessage_218 (SysUnExpect ((V.fromList [])), l7)); l10 = (\ l10 -> setErrorMessage_221 (Expect (l0 l2), newErrorMessage_218 (SysUnExpect ((V.fromList [l10])), l7))) in (case uncons_261 l6 of (None) -> handleEERR_236 l9; (Some (l11, l12)) -> (case uncurry (==) (l3, l11) of True -> walk_260 (l4, l12, l8, l2, l7, l1, l9, l10); False -> handleEERR_236 (l10 l11))))


try_263 :: (ParsecT a) -> (ParsecT a)
try_263 l0 =
  ParsecT (\ l1 -> case unParser_240 (l0, l1) of (Consumed (Ok (l2, l3, l4))) -> Consumed (Ok (l2, l3, l4)); (Consumed (Err l2)) -> Empty (Err l2); (Empty (Ok (l2, l3, l4))) -> Empty (Ok (l2, l3, l4)); (Empty (Err l2)) -> Empty (Err l2))


lookAhead_264 :: (ParsecT a) -> (ParsecT a)
lookAhead_264 l0 =
  ParsecT (\ l1 -> case unParser_240 (l0, l1) of (Consumed (Ok (l2, l3, l4))) -> Empty (Ok (l2, l1, newErrorUnknown_217 (statePos_244 l1))); (Consumed (Err l2)) -> Consumed (Err l2); (Empty (Ok (l2, l3, l4))) -> Empty (Ok (l2, l1, newErrorUnknown_217 (statePos_244 l1))); (Empty (Err l2)) -> Empty (Err l2))


unexpectError_268 :: ((Vector Word8) , SourcePos) -> ParseError
unexpectError_268 (l0, l1) =
  newErrorMessage_218 (SysUnExpect l0, l1)


tokenPrimEx_267 :: ((Word8 -> (Vector Word8) ), (((SourcePos, Word8, (Iter Word8))) -> SourcePos), (Option ((((SourcePos, Word8, (Iter Word8), Int64)) -> Int64))), (Word8 -> (Option a))) -> (ParsecT a)
tokenPrimEx_267 (l0, l1, l2, l3) =
  case l2 of (None) -> ParsecT (\ l4 -> let (ParseState (l5, l6, l7)) = l4 in (case uncons_261 l5 of (None) -> handleEERR_236 (unexpectError_268 ((V.fromList []), l6)); (Some (l8, l9)) -> (case l3 l8 of (Some l10) -> (let l11 = l1 (l6, l8, l9); l12 = ParseState (l9, l11, l7) in handleCOK_233 (l10, l12, newErrorUnknown_217 l11)); (None) -> handleEERR_236 (unexpectError_268 (l0 l8, l6))))); (Some l4) -> ParsecT (\ l5 -> let (ParseState (l6, l7, l8)) = l5 in (case uncons_261 l6 of (None) -> handleEERR_236 (unexpectError_268 ((V.fromList []), l7)); (Some (l9, l10)) -> (case l3 l9 of (Some l11) -> (let l12 = l1 (l7, l9, l10); l13 = l4 (l7, l9, l10, l8); l14 = ParseState (l10, l12, l13) in handleCOK_233 (l11, l14, newErrorUnknown_217 l12)); (None) -> handleEERR_236 (unexpectError_268 (l0 l9, l7)))))


tokenPrim_266 :: ((Word8 -> (Vector Word8) ), (((SourcePos, Word8, (Iter Word8))) -> SourcePos), (Word8 -> (Option a))) -> (ParsecT a)
tokenPrim_266 (l0, l1, l2) =
  tokenPrimEx_267 (l0, l1, None, l2)


token_265 :: ((Word8 -> (Vector Word8) ), (Word8 -> SourcePos), (Word8 -> (Option a))) -> (ParsecT a)
token_265 (l0, l1, l2) =
  let l3 = (\ (l_0, l3, l4) -> case uncons_261 l4 of (None) -> l1 l3; (Some (l5, l_1)) -> l1 l5) in tokenPrim_266 (l0, l3, l2)


walk1_273 :: ((Vector a) , a, ParseState, (((a, (Vector a) )) -> (Vector a) ), (ParsecT a)) -> (Consumed (Result (((Vector a) , ParseState, ParseError)) ParseError))
walk1_273 (l0, l1, l2, l3, l4) =
  case unParser_240 (l4, l2) of (Consumed (Ok (l5, l6, l_2))) -> walk1_273 (l3 (l1, l0), l5, l6, l3, l4); (Consumed (Err l5)) -> handleCERR_234 l5; (Empty (Ok (l5, l6, l_3))) -> panic ((V.fromList [84, 101, 120, 116, 46, 80, 97, 114, 115, 101, 114, 67, 111, 109, 98, 105, 110, 97, 116, 111, 114, 115, 46, 80, 97, 114, 115, 101, 99, 46, 80, 114, 105, 109, 46, 109, 97, 110, 121, 58, 32, 99, 111, 109, 98, 105, 110, 97, 116, 111, 114, 32, 39, 109, 97, 110, 121, 39, 32, 105, 115, 32, 97, 112, 112, 108, 105, 101, 100, 32, 116, 111, 32, 97, 32, 112, 97, 114, 115, 101, 114, 32, 116, 104, 97, 116, 32, 97, 99, 99, 101, 112, 116, 115, 32, 97, 110, 32, 101, 109, 112, 116, 121, 32, 115, 116, 114, 105, 110, 103, 46])); (Empty (Err l5)) -> handleCOK_233 (l3 (l1, l0), l2, l5)


manyAccum_272 :: ((((a, (Vector a) )) -> (Vector a) ), (ParsecT a)) -> (ParsecT (Vector a) )
manyAccum_272 (l0, l1) =
  ParsecT (\ l2 -> case unParser_240 (l1, l2) of (Consumed (Ok (l3, l4, l5))) -> walk1_273 ((V.fromList []), l3, l4, l0, l1); (Consumed (Err l3)) -> Consumed (Err l3); (Empty (Ok (l3, l4, l5))) -> panic ((V.fromList [84, 101, 120, 116, 46, 80, 97, 114, 115, 101, 114, 67, 111, 109, 98, 105, 110, 97, 116, 111, 114, 115, 46, 80, 97, 114, 115, 101, 99, 46, 80, 114, 105, 109, 46, 109, 97, 110, 121, 58, 32, 99, 111, 109, 98, 105, 110, 97, 116, 111, 114, 32, 39, 109, 97, 110, 121, 39, 32, 105, 115, 32, 97, 112, 112, 108, 105, 101, 100, 32, 116, 111, 32, 97, 32, 112, 97, 114, 115, 101, 114, 32, 116, 104, 97, 116, 32, 97, 99, 99, 101, 112, 116, 115, 32, 97, 110, 32, 101, 109, 112, 116, 121, 32, 115, 116, 114, 105, 110, 103, 46])); (Empty (Err l3)) -> Empty (Ok ((V.fromList []), l2, l3)))


many_269 :: (ParsecT a) -> (ParsecT (Vector a) )
many_269 l0 =
  manyAccum_272 ((\ (l1, l2) -> (push_55 ()) (l2, l1)), l0)


many1_270 :: (ParsecT a) -> (ParsecT (Vector a) )
many1_270 l0 =
  parserBind_248 (l0, (\ l1 -> parserBind_248 (many_269 l0, (\ l2 -> parserReturn_247 (push_front_79 (l1, l2))))))


skipMany_271 :: (ParsecT a) -> (ParsecT (()))
skipMany_271 l0 =
  parsecMap_246 ((\ l_4 -> ()), manyAccum_272 ((\ (l_5, l_6) -> (V.fromList [])), l0))


parserReply_275 :: (Consumed a) -> a
parserReply_275 l0 =
  case l0 of (Consumed l1) -> l1; (Empty l1) -> l1


runPT_274 :: ((ParsecT a), Int64, (Vector Word8) , (Iter Word8)) -> (BadGood ParseError a)
runPT_274 (l0, l1, l2, l3) =
  case parserReply_275 (runParsecT_241 (l0, ParseState (l3, initialPos_196 l2, l1))) of (Ok (l4, l_7, l_8)) -> Good l4; (Err l4) -> Bad l4


runP_276 :: ((ParsecT a), Int64, (Vector Word8) , (Iter Word8)) -> (BadGood ParseError a)
runP_276 (l0, l1, l2, l3) =
  runPT_274 (l0, l1, l2, l3)


runParserT_277 :: ((ParsecT a), Int64, (Vector Word8) , (Iter Word8)) -> (BadGood ParseError a)
runParserT_277 (l0, l1, l2, l3) =
  runPT_274 (l0, l1, l2, l3)


runParser_278 :: ((ParsecT a), Int64, (Vector Word8) , (Iter Word8)) -> (BadGood ParseError a)
runParser_278 (l0, l1, l2, l3) =
  runP_276 (l0, l1, l2, l3)


parse_279 :: ((ParsecT a), (Vector Word8) , (Iter Word8)) -> (BadGood ParseError a)
parse_279 (l0, l1, l2) =
  runP_276 (l0, 0, l1, l2)


updateParserState_286 :: (ParseState -> ParseState) -> (ParsecT ParseState)
updateParserState_286 l0 =
  ParsecT (\ l1 -> let l2 = l0 l1 in handleEOK_235 (l2, l2, unknownError_237 l2))


setPosition_282 :: SourcePos -> (ParsecT (()))
setPosition_282 l0 =
  parsecMap_246 ((\ l_0 -> ()), updateParserState_286 (\ l1 -> let (ParseState (l2, l_0, l3)) = l1 in l_0 `seq` ParseState (l2, l0, l3)))


setInput_283 :: (Iter Word8) -> (ParsecT (()))
setInput_283 l0 =
  parsecMap_246 ((\ l_1 -> ()), updateParserState_286 (\ l1 -> let (ParseState (l_0, l2, l3)) = l1 in l_0 `seq` ParseState (l0, l2, l3)))


getParserState_284 :: () -> (ParsecT ParseState)
getParserState_284 _ =
  updateParserState_286 id_134


getPosition_280 :: () -> (ParsecT SourcePos)
getPosition_280 _ =
  parsecMap_246 (statePos_244, getParserState_284 ())


getInput_281 :: () -> (ParsecT (Iter Word8))
getInput_281 _ =
  parsecMap_246 (stateInput_243, getParserState_284 ())


setParserState_285 :: ParseState -> (ParsecT ParseState)
setParserState_285 l0 =
  updateParserState_286 (\ l_1 -> l0)


getState_287 :: () -> (ParsecT Int64)
getState_287 _ =
  parsecMap_246 (stateUser_245, getParserState_284 ())


putState_288 :: Int64 -> (ParsecT (()))
putState_288 l0 =
  parsecMap_246 ((\ l_2 -> ()), updateParserState_286 (\ l1 -> let (ParseState (l2, l3, l4)) = l1 in ParseState (l2, l3, l0)))


modifyState_289 :: (Int64 -> Int64) -> (ParsecT (()))
modifyState_289 l0 =
  parsecMap_246 ((\ l_0 -> ()), updateParserState_286 (\ l1 -> let (ParseState (l2, l3, l4)) = l1 in ParseState (l2, l3, l0 l4)))


setState_290 :: Int64 -> (ParsecT (()))
setState_290 l0 =
  putState_288 l0


updateState_291 :: (Int64 -> Int64) -> (ParsecT (()))
updateState_291 l0 =
  modifyState_289 l0


satisfy_309 :: (Word8 -> Bool) -> (ParsecT Word8)
satisfy_309 l0 =
  tokenPrim_266 ((\ l1 -> (V.fromList [l1])), (\ (l1, l2, l_0) -> updatePosChar_206 (l1, l2)), (\ l1 -> case l0 l1 of True -> Some l1; False -> None))


oneOf_292 :: (Vector Word8)  -> (ParsecT Word8)
oneOf_292 l0 =
  satisfy_309 (\ l1 -> elem_90 (l1, l0, (\ (l2, l3) -> uncurry (==) (l2, l3))))


noneOf_293 :: (Vector Word8)  -> (ParsecT Word8)
noneOf_293 l0 =
  satisfy_309 (\ l1 -> not (elem_90 (l1, l0, (\ (l2, l3) -> uncurry (==) (l2, l3)))))


space_295 :: () -> (ParsecT Word8)
space_295 _ =
  withErr_255 (satisfy_309 isSpace_124, (V.fromList [115, 112, 97, 99, 101]))


spaces_294 :: () -> (ParsecT (()))
spaces_294 _ =
  withErr_255 (skipMany_271 (space_295 ()), (V.fromList [119, 104, 105, 116, 101, 32, 115, 112, 97, 99, 101]))


upper_300 :: () -> (ParsecT Word8)
upper_300 _ =
  withErr_255 (satisfy_309 isUpper_125, (V.fromList [117, 112, 112, 101, 114, 99, 97, 115, 101, 32, 108, 101, 116, 116, 101, 114]))


lower_301 :: () -> (ParsecT Word8)
lower_301 _ =
  withErr_255 (satisfy_309 isLower_126, (V.fromList [108, 111, 119, 101, 114, 99, 97, 115, 101, 32, 108, 101, 116, 116, 101, 114]))


alphaNum_302 :: () -> (ParsecT Word8)
alphaNum_302 _ =
  withErr_255 (satisfy_309 isAlphaNum_129, (V.fromList [108, 101, 116, 116, 101, 114, 32, 111, 114, 32, 100, 105, 103, 105, 116]))


letter_303 :: () -> (ParsecT Word8)
letter_303 _ =
  withErr_255 (satisfy_309 isAlpha_128, (V.fromList [108, 101, 116, 116, 101, 114]))


digit_304 :: () -> (ParsecT Word8)
digit_304 _ =
  withErr_255 (satisfy_309 isDigit_127, (V.fromList [100, 105, 103, 105, 116]))


hexDigit_305 :: () -> (ParsecT Word8)
hexDigit_305 _ =
  withErr_255 (satisfy_309 isHexDigit_130, (V.fromList [104, 101, 120, 97, 100, 101, 99, 105, 109, 97, 108, 32, 100, 105, 103, 105, 116]))


octDigit_306 :: () -> (ParsecT Word8)
octDigit_306 _ =
  withErr_255 (satisfy_309 isOctDigit_131, (V.fromList [111, 99, 116, 97, 108, 32, 100, 105, 103, 105, 116]))


char_307 :: Word8 -> (ParsecT Word8)
char_307 l0 =
  withErr_255 (satisfy_309 (\ l1 -> uncurry (==) (l1, l0)), (V.fromList [l0]))


newline_296 :: () -> (ParsecT Word8)
newline_296 _ =
  withErr_255 (char_307 10, (V.fromList [108, 102, 32, 110, 101, 119, 45, 108, 105, 110, 101]))


crlf_297 :: () -> (ParsecT Word8)
crlf_297 _ =
  withErr_255 (parserBind_248 (char_307 13, const_136 (char_307 10)), (V.fromList [99, 114, 108, 102, 32, 110, 101, 119, 45, 108, 105, 110, 101]))


endOfLine_298 :: () -> (ParsecT Word8)
endOfLine_298 _ =
  withErr_255 (tryOr_256 (newline_296 (), crlf_297 ()), (V.fromList [110, 101, 119, 45, 108, 105, 110, 101]))


tab_299 :: () -> (ParsecT Word8)
tab_299 _ =
  withErr_255 (char_307 9, (V.fromList [116, 97, 98]))


anyChar_308 :: () -> (ParsecT Word8)
anyChar_308 _ =
  satisfy_309 (const_136 True)


string_310 :: (Vector Word8)  -> (ParsecT (Vector Word8) )
string_310 l0 =
  tokens_259 ((\ (l1, l2) -> uncurry (==) (l1, l2)), from_iter_86, updatePosString_205, items_19 l0)


string1_311 :: (Vector Word8)  -> (ParsecT (Vector Word8) )
string1_311 l0 =
  tokens1_262 (from_iter_86, updatePosString_205, items_19 l0)


choice_312 :: (Vector ((ParsecT a)))  -> (ParsecT a)
choice_312 l0 =
  foldr_36 (items_19 l0, parserZero_253 (), tryOr_256)


option_313 :: (a, (ParsecT a)) -> (ParsecT a)
option_313 (l0, l1) =
  tryOr_256 (l1, parserReturn_247 l0)


optionMaybe_314 :: (ParsecT a) -> (ParsecT (Option a))
optionMaybe_314 l0 =
  option_313 (None, parsecMap_246 (Some, l0))


optional_315 :: (ParsecT a) -> (ParsecT (()))
optional_315 l0 =
  tryOr_256 (parserBind_248 (l0, (\ l_1 -> parserReturn_247 ())), parserReturn_247 ())


between_316 :: ((ParsecT a), (ParsecT b), (ParsecT c)) -> (ParsecT c)
between_316 (l0, l1, l2) =
  parserBind_248 (l0, (\ l_2 -> parserBind_248 (l2, (\ l3 -> parserBind_248 (l1, (\ l_3 -> parserReturn_247 l3))))))


skipMany1_317 :: (ParsecT a) -> (ParsecT (()))
skipMany1_317 l0 =
  parserBind_248 (l0, (\ l_4 -> skipMany_271 l0))


sepBy1_319 :: ((ParsecT a), (ParsecT b)) -> (ParsecT (Vector a) )
sepBy1_319 (l0, l1) =
  parserBind_248 (l0, (\ l2 -> parserBind_248 (many_269 (parserBind_248 (l1, (\ l_5 -> l0))), (\ l3 -> parserReturn_247 ((push_55 ()) (l3, l2))))))


sepBy_318 :: ((ParsecT a), (ParsecT b)) -> (ParsecT (Vector a) )
sepBy_318 (l0, l1) =
  tryOr_256 (sepBy1_319 (l0, l1), parserReturn_247 ((V.fromList [])))


sepEndBy_321 :: ((ParsecT a), (ParsecT b)) -> (ParsecT (Vector a) )
sepEndBy_321 (l0, l1) =
  tryOr_256 (sepEndBy1_320 (l0, l1), parserReturn_247 ((V.fromList [])))
sepEndBy1_320 (l0, l1) =
  parserBind_248 (l0, (\ l2 -> tryOr_256 (parserBind_248 (l1, (\ l_6 -> parserBind_248 (sepEndBy_321 (l0, l1), (\ l3 -> parserReturn_247 ((push_55 ()) (l3, l2)))))), parserReturn_247 ((V.fromList [l2])))))


endBy1_322 :: ((ParsecT a), (ParsecT b)) -> (ParsecT (Vector a) )
endBy1_322 (l0, l1) =
  many1_270 (parserBind_248 (l0, (\ l2 -> parserBind_248 (l1, (\ l_7 -> parserReturn_247 l2)))))


endBy_323 :: ((ParsecT a), (ParsecT b)) -> (ParsecT (Vector a) )
endBy_323 (l0, l1) =
  many_269 (parserBind_248 (l0, (\ l2 -> parserBind_248 (l1, (\ l_8 -> parserReturn_247 l2)))))


count_324 :: (Int64, (ParsecT a)) -> (ParsecT (Vector a) )
count_324 (l0, l1) =
  case uncurry (<=) (l0, 0) of True -> parserReturn_247 ((V.fromList [])); False -> parserSequence_249 (fill_84 (l0, l1))


chainl1Rest_328 :: ((ParsecT a), (ParsecT ((((a, a)) -> a))), a) -> (ParsecT a)
chainl1Rest_328 (l0, l1, l2) =
  parserBind_248 (l1, (\ l3 -> parserBind_248 (l0, (\ l4 -> chainl1Rest_328 (l0, l1, l3 (l2, l4))))))


chainl1_327 :: ((ParsecT a), (ParsecT ((((a, a)) -> a)))) -> (ParsecT a)
chainl1_327 (l0, l1) =
  parserBind_248 (l0, (\ l2 -> chainl1Rest_328 (l0, l1, l2)))


chainl_326 :: ((ParsecT a), (ParsecT ((((a, a)) -> a))), a) -> (ParsecT a)
chainl_326 (l0, l1, l2) =
  tryOr_256 (chainl1_327 (l0, l1), parserReturn_247 l2)


chainr1Rest_331 :: (a, (ParsecT a), (ParsecT ((((a, a)) -> a)))) -> (ParsecT a)
chainr1Rest_331 (l0, l1, l2) =
  tryOr_256 (parserBind_248 (l2, (\ l3 -> parserBind_248 (chainr1Scan_330 (l1, l2), (\ l4 -> parserReturn_247 (l3 (l0, l4)))))), parserReturn_247 l0)
chainr1Scan_330 (l0, l1) =
  parserBind_248 (l0, (\ l2 -> chainr1Rest_331 (l2, l0, l1)))


chainr1_329 :: ((ParsecT a), (ParsecT ((((a, a)) -> a)))) -> (ParsecT a)
chainr1_329 (l0, l1) =
  chainr1Scan_330 (l0, l1)


chainr_325 :: ((ParsecT a), (ParsecT ((((a, a)) -> a))), a) -> (ParsecT a)
chainr_325 (l0, l1, l2) =
  tryOr_256 (chainr1_329 (l0, l1), parserReturn_247 l2)


anyToken_332 :: () -> (ParsecT Word8)
anyToken_332 _ =
  tokenPrim_266 ((\ l0 -> (V.fromList [l0])), (\ (l0, l_9, l_10) -> l0), Some)


notFollowedBy_334 :: (ParsecT Word8) -> (ParsecT (()))
notFollowedBy_334 l0 =
  try_263 (tryOr_256 (parserBind_248 (try_263 l0, (\ l1 -> unexpected_239 ((V.fromList [l1])))), parserReturn_247 ()))


eof_333 :: () -> (ParsecT (()))
eof_333 _ =
  withErr_255 (notFollowedBy_334 (anyToken_332 ()), (V.fromList [101, 110, 100, 32, 111, 102, 32, 105, 110, 112, 117, 116]))


manyTillScan_336 :: ((ParsecT a), (ParsecT b)) -> (ParsecT (Vector a) )
manyTillScan_336 (l0, l1) =
  tryOr_256 (parserBind_248 (l1, (\ l_11 -> parserReturn_247 ((V.fromList [])))), parserBind_248 (l0, (\ l2 -> parserBind_248 (manyTillScan_336 (l0, l1), (\ l3 -> parserReturn_247 ((push_55 ()) (l3, l2)))))))


manyTill_335 :: ((ParsecT a), (ParsecT b)) -> (ParsecT (Vector a) )
manyTill_335 (l0, l1) =
  manyTillScan_336 (l0, l1)


main_337 :: () -> ()
main_337 () =
  let l0 = (V.fromList [105, 110, 110, 80, 117, 116]) in (case parse_279 (string1_311 ((V.fromList [105, 110, 80, 117, 116])), (V.fromList [73, 110, 112, 117, 116, 32, 102, 105, 108, 101]), items_19 l0) of (Bad l1) -> output (showParseError_224 l1); (Good l1) -> output ((V.fromList [68, 79, 78, 69, 33])))


empty_338 :: () -> (Bucket a b)
empty_338 () =
  Bucket ((V.fromList []))


find_339 :: ((Bucket a b), a, (((a, a)) -> Bool)) -> (Option b)
find_339 (l0, l1, l2) =
  let (Bucket l3) = l0 in (let l4 = (let l4 = l3 in items_19 l4) in find_map_43 (l4, (\ (Entry (l5, l6)) -> case l2 (l5, l1) of True -> Some l6; False -> None)))


insert_340 :: ((Bucket a b), a, b, (((a, a)) -> Bool)) -> (Bucket a b)
insert_340 (l0, l1, l2, l3) =
  let (Bucket l4) = l0 in (case find_index_62 (l4, (\ (Entry (l5, l_0)) -> l3 (l5, l1))) of (Some l5) -> Bucket (set_56 (l4, l5, Entry (l1, l2))); (None) -> Bucket ((push_55 ()) (l4, Entry (l1, l2))))


remove_341 :: ((Bucket a b), a, (((a, a)) -> Bool)) -> (Bucket a b)
remove_341 (l0, l1, l2) =
  let (Bucket l3) = l0 in Bucket (let l4 = (let l4 = (let l4 = l3 in items_19 l4) in filter_26 (l4, (\ (Entry (l5, l_0)) -> not (l2 (l5, l1))))) in from_iter_86 l4)


empty_342 :: ((a -> Int64), (((a, a)) -> Bool)) -> (HashMap a b)
empty_342 (l0, l1) =
  HashMap (8, l0, l1, fill_with_83 (8, (\ () -> empty_338 ())), 0)


int_mod_343 :: (Int64, Int64) -> Int64
int_mod_343 (l0, l1) =
  uncurry (-) (l0, uncurry (*) (uncurry divv (l0, l1), l1))


get_bucket_index_344 :: (Int64, Int64) -> Int64
get_bucket_index_344 (l0, l1) =
  int_mod_343 ((case uncurry (<) (l0, 0) of True -> negate l0; False -> l0), l1)


get_345 :: ((HashMap a b), a) -> (Option b)
get_345 (l0, l1) =
  let (HashMap (l2, l3, l4, l5, l_0)) = l0; l6 = get_bucket_index_344 (l3 l1, l2) in l_0 `seq` (case try_get_58 (l5, l6) of (Some l7) -> find_339 (l7, l1, l4); (None) -> None)


insert_internal_347 :: ((HashMap a b), a, b) -> (HashMap a b)
insert_internal_347 (l0, l1, l2) =
  let (HashMap (l3, l4, l5, l6, l7)) = l0; l8 = get_bucket_index_344 (l4 l1, l3); l9 = (get_53 ()) (l6, l8); l10 = is_some_0 (find_339 (l9, l1, l5)); l11 = update_57 (l6, l8, (\ l11 -> insert_340 (l11, l1, l2, l5))) in HashMap (l3, l4, l5, l11, (case l10 of True -> l7; False -> uncurry (+) (l7, 1)))


resize_if_needed_346 :: (HashMap a b) -> (HashMap a b)
resize_if_needed_346 l0 =
  let (HashMap (l1, l2, l3, l4, l5)) = l0 in (case uncurry (>) (l5, uncurry divv (uncurry (*) (l1, 3), 4)) of True -> (let l6 = uncurry (*) (l1, 2); l7 = fill_with_83 (l6, (\ () -> empty_338 ())); l8 = HashMap (l6, l2, l3, l7, 0) in (let l9 = (let l9 = l4 in items_19 l9) in foldl_35 (l9, l8, (\ (l10, (Bucket l11)) -> let l12 = (let l12 = l11 in items_19 l12) in foldl_35 (l12, l10, (\ (l13, (Entry (l14, l15))) -> insert_internal_347 (l13, l14, l15))))))); False -> l0)


insert_348 :: ((HashMap a b), a, b) -> (HashMap a b)
insert_348 (l0, l1, l2) =
  let l3 = insert_internal_347 (l0, l1, l2) in resize_if_needed_346 l3


remove_349 :: ((HashMap a b), a) -> (HashMap a b)
remove_349 (l0, l1) =
  let (HashMap (l2, l3, l4, l5, l6)) = l0; l7 = get_bucket_index_344 (l3 l1, l2); l8 = (get_53 ()) (l5, l7); l9 = is_some_0 (find_339 (l8, l1, l4)); l10 = update_57 (l5, l7, (\ l10 -> remove_341 (l10, l1, l4))) in HashMap (l2, l3, l4, l10, (case l9 of True -> uncurry (-) (l6, 1); False -> l6))


is_empty_350 :: (HashMap a b) -> Bool
is_empty_350 l0 =
  let (HashMap (l_0, l_1, l_2, l_3, l1)) = l0 in l_0 `seq` l_1 `seq` l_2 `seq` l_3 `seq` uncurry (==) (l1, 0)


string_hash_351 :: (Vector Word8)  -> Int64
string_hash_351 l0 =
  let l1 = (let l1 = l0 in items_19 l1) in foldl_35 (l1, 0, (\ (l2, l3) -> uncurry (+) (uncurry (*) (l2, 31), fromIntegral l3)))


string_eq_352 :: ((Vector Word8) , (Vector Word8) ) -> Bool
string_eq_352 (l0, l1) =
  equal_81 (l0, l1, (\ (l2, l3) -> uncurry (==) (l2, l3)))


string_hashmap_353 :: () -> (HashMap (Vector Word8)  a)
string_hashmap_353 _ =
  empty_342 (string_hash_351, string_eq_352)


to_string_354 :: ((HashMap a b), (a -> (Vector Word8) ), (b -> (Vector Word8) )) -> (Vector Word8) 
to_string_354 (l0, l1, l2) =
  let (HashMap (l_0, l_1, l_2, l3, l_3)) = l0 in l_0 `seq` l_1 `seq` l_2 `seq` l_3 `seq` (let l4 = concat_122 ((V.fromList [72, 97, 115, 104, 77, 97, 112, 123]), join_123 ((let l4 = (let l4 = (let l4 = (let l4 = l3 in items_19 l4) in flat_map_25 (l4, (\ (Bucket l5) -> items_19 l5))) in map_23 (l4, (\ (Entry (l5, l6)) -> join_123 ((V.fromList [l1 l5, (V.fromList [32, 45, 62, 32]), l2 l6]), (V.fromList []))))) in from_iter_86 l4), (V.fromList [44, 32]))) in concat_122 (l4, (V.fromList [125])))


repeat_355 :: (Int64, ((()) -> a)) -> (Option a)
repeat_355 (l0, l1) =
  case uncurry (<) (l0, 1) of True -> None; False -> (let l2 = l1 () in (case uncurry (==) (l0, 1) of True -> Some l2; False -> repeat_355 (uncurry (-) (l0, 1), l1)))


ascii_space_356 :: () -> Word8
ascii_space_356 _ =
  32


ascii_line_feed_357 :: () -> Word8
ascii_line_feed_357 _ =
  10


ascii_carriage_return_358 :: () -> Word8
ascii_carriage_return_358 _ =
  13


ascii_tab_359 :: () -> Word8
ascii_tab_359 _ =
  9


ascii_zero_360 :: () -> Word8
ascii_zero_360 _ =
  48


ascii_nine_361 :: () -> Word8
ascii_nine_361 _ =
  57


ascii_star_362 :: () -> Word8
ascii_star_362 _ =
  42


ascii_plus_363 :: () -> Word8
ascii_plus_363 _ =
  43


ascii_minus_364 :: () -> Word8
ascii_minus_364 _ =
  45


ascii_tilde_365 :: () -> Word8
ascii_tilde_365 _ =
  126


ascii_and_366 :: () -> Word8
ascii_and_366 _ =
  38


ascii_pipe_367 :: () -> Word8
ascii_pipe_367 _ =
  124


ascii_lt_368 :: () -> Word8
ascii_lt_368 _ =
  60


ascii_eq_369 :: () -> Word8
ascii_eq_369 _ =
  61


ascii_dollar_370 :: () -> Word8
ascii_dollar_370 _ =
  36


ascii_open_paren_371 :: () -> Word8
ascii_open_paren_371 _ =
  40


ascii_close_paren_372 :: () -> Word8
ascii_close_paren_372 _ =
  41


ascii_lower_a_373 :: () -> Word8
ascii_lower_a_373 _ =
  97


ascii_lower_z_374 :: () -> Word8
ascii_lower_z_374 _ =
  122


ascii_upper_a_375 :: () -> Word8
ascii_upper_a_375 _ =
  65


ascii_upper_z_376 :: () -> Word8
ascii_upper_z_376 _ =
  90


ascii_underscore_377 :: () -> Word8
ascii_underscore_377 _ =
  95


getOp_378 :: Word8 -> Operator
getOp_378 l0 =
  case l0 of 42 -> Mul; 43 -> Add; 45 -> Neg; 126 -> Not; 38 -> And; 124 -> Or; 60 -> Lt; 61 -> Eq; l_0 -> panic ((V.fromList [117, 110, 114, 101, 97, 99, 104, 97, 98, 108, 101]))


p_or_382 :: ((ParsecT a), (ParsecT a)) -> (ParsecT a)
p_or_382 (l0, l1) =
  let l2 = try_263 l0 in tryOr_256 (l2, l1)


space_379 :: () -> (ParsecT (()))
space_379 _ =
  let l0 = (let l0 = (let l0 = (let l0 = space_295 () in p_or_382 (l0, newline_296 ())) in p_or_382 (l0, crlf_297 ())) in p_or_382 (l0, tab_299 ())) in parserBind_248 (l0, (\ l_0 -> parserReturn_247 ()))


spaces_380 :: () -> (ParsecT (()))
spaces_380 _ =
  skipMany_271 (space_379 ())


spaced_381 :: (ParsecT a) -> (ParsecT a)
spaced_381 l0 =
  between_316 (spaces_380 (), spaces_380 (), l0)


paren_383 :: (ParsecT a) -> (ParsecT a)
paren_383 l0 =
  between_316 (char_307 (ascii_open_paren_371 ()), char_307 (ascii_close_paren_372 ()), l0)


p_digit_387 :: () -> (ParsecT Int64)
p_digit_387 _ =
  parsecMap_246 ((\ l0 -> fromIntegral (uncurry (-) (l0, ascii_zero_360 ()))), digit_304 ())


p_int_388 :: () -> (ParsecT Int64)
p_int_388 _ =
  parsecMap_246 ((\ l0 -> foldl_66 (l0, 0, (\ (l1, l2) -> uncurry (+) (uncurry (*) (10, l1), l2)))), many1_270 (p_digit_387 ()))


p_boolean_389 :: () -> (ParsecT Bool)
p_boolean_389 _ =
  let l0 = parsecMap_246 ((\ l_0 -> True), string1_311 ((V.fromList [116, 114, 117, 101]))) in p_or_382 (l0, parsecMap_246 ((\ l_1 -> False), string1_311 ((V.fromList [102, 97, 108, 115, 101]))))


p_bool_390 :: () -> (ParsecT NamedExpression)
p_bool_390 _ =
  parsecMap_246 (NBool, p_boolean_389 ())


p_num_391 :: () -> (ParsecT NamedExpression)
p_num_391 _ =
  parsecMap_246 (NNum, p_int_388 ())


p_char_392 :: () -> (ParsecT Word8)
p_char_392 _ =
  p_or_382 (alphaNum_302 (), char_307 (ascii_underscore_377 ()))


p_name_393 :: () -> (ParsecT (Vector Word8) )
p_name_393 _ =
  let l0 = (\ (l0, l1) -> uncurry (==) (l0, l1)) in (let l1 = parserBind_248 (spaced_381 (many1_270 (p_char_392 ())), (\ l1 -> case equal_81 (l1, (V.fromList [105, 110]), l0) of True -> parserFail_252 ((V.fromList [82, 101, 115, 101, 114, 118, 101, 100, 32, 107, 101, 121, 119, 111, 114, 100, 58, 32, 34, 105, 110, 34])); False -> (case equal_81 (l1, (V.fromList [116, 104, 101, 110]), l0) of True -> parserFail_252 ((V.fromList [82, 101, 115, 101, 114, 118, 101, 100, 32, 107, 101, 121, 119, 111, 114, 100, 58, 32, 34, 116, 104, 101, 110, 34])); False -> (case equal_81 (l1, (V.fromList [101, 108, 115, 101]), l0) of True -> parserFail_252 ((V.fromList [82, 101, 115, 101, 114, 118, 101, 100, 32, 107, 101, 121, 119, 111, 114, 100, 58, 32, 34, 101, 108, 115, 101, 34])); False -> parserReturn_247 l1)))) in withErr_255 (l1, (V.fromList [112, 95, 110, 97, 109, 101, 32, 101, 114, 114, 111, 114])))


p_var_394 :: () -> (ParsecT NamedExpression)
p_var_394 _ =
  parsecMap_246 (NVar, p_name_393 ())


p_op_395 :: () -> (ParsecT Operator)
p_op_395 _ =
  parsecMap_246 ((\ l0 -> case l0 of 42 -> Mul; 43 -> Add; 45 -> Neg; 126 -> Not; 38 -> And; 124 -> Or; 60 -> Lt; 61 -> Eq; l_0 -> panic ((V.fromList [117, 110, 114, 101, 97, 99, 104, 97, 98, 108, 101]))), oneOf_292 ((V.fromList [42, 43, 45, 126, 38, 124, 60, 61])))


p_operator_396 :: () -> (ParsecT NamedExpression)
p_operator_396 _ =
  parsecMap_246 (NOp, p_op_395 ())


p_word_398 :: (Vector Word8)  -> (ParsecT (()))
p_word_398 l0 =
  spaced_381 (parsecMap_246 ((\ l_1 -> ()), string1_311 l0))


p_func_385 :: () -> (ParsecT Type)
p_func_385 () =
  parserBind_248 (p_basic_384 (), (\ l0 -> parserBind_248 (optionMaybe_314 (parserBind_248 (p_word_398 ((V.fromList [45, 62])), (\ l_2 -> lazy_232 p_func_385))), (\ l1 -> parserReturn_247 (case l1 of (None) -> l0; (Some l2) -> Func (l0, l2))))))
p_basic_384 _ =
  let l0 = (let l0 = paren_383 (lazy_232 p_func_385) in p_or_382 (l0, parserBind_248 (p_word_398 ((V.fromList [105, 110, 116])), (\ l_0 -> parserReturn_247 Integer)))) in p_or_382 (l0, parserBind_248 (p_word_398 ((V.fromList [98, 111, 111, 108])), (\ l_1 -> parserReturn_247 Boolean)))


p_type_386 :: () -> (ParsecT Type)
p_type_386 _ =
  parserBind_248 (p_word_398 ((V.fromList [58, 58])), (\ l_2 -> lazy_232 p_func_385))


p_apply_403 :: () -> (ParsecT NamedExpression)
p_apply_403 () =
  parsecMap_246 ((\ l0 -> reduce_67 (l0, NApply)), many1_270 (try_263 (p_expr_402 ())))
p_expr_402 _ =
  let l0 = (let l0 = (let l0 = p_let_400 () in p_or_382 (l0, lazy_232 p_fun_399)) in p_or_382 (l0, lazy_232 p_if_401)) in p_or_382 (l0, lazy_232 p_atom_397)
p_if_401 () =
  parserBind_248 (p_word_398 ((V.fromList [105, 102])), (\ l_0 -> parserBind_248 (lazy_232 p_apply_403, (\ l0 -> parserBind_248 (p_word_398 ((V.fromList [116, 104, 101, 110])), (\ l_1 -> parserBind_248 (lazy_232 p_apply_403, (\ l1 -> parserBind_248 (p_word_398 ((V.fromList [101, 108, 115, 101])), (\ l_2 -> parserBind_248 (lazy_232 p_apply_403, (\ l2 -> parserReturn_247 (NIf (l0, l1, l2))))))))))))))
p_let_400 _ =
  parserBind_248 (p_word_398 ((V.fromList [108, 101, 116])), (\ l_3 -> parserBind_248 (p_name_393 (), (\ l0 -> parserBind_248 (p_type_386 (), (\ l1 -> parserBind_248 (p_word_398 ((V.fromList [61])), (\ l_4 -> parserBind_248 (lazy_232 p_apply_403, (\ l2 -> parserBind_248 (p_word_398 ((V.fromList [105, 110])), (\ l_5 -> parserBind_248 (lazy_232 p_apply_403, (\ l3 -> parserReturn_247 (NLet (l0, l1, l2, l3))))))))))))))))
p_fun_399 () =
  parserBind_248 (p_word_398 ((V.fromList [102, 117, 110])), (\ l_6 -> parserBind_248 (p_name_393 (), (\ l0 -> parserBind_248 (paren_383 (p_name_393 ()), (\ l1 -> parserBind_248 (p_type_386 (), (\ l2 -> parserBind_248 (p_word_398 ((V.fromList [61])), (\ l_7 -> parserBind_248 (lazy_232 p_apply_403, (\ l3 -> parserReturn_247 (NFun (l0, l1, l2, l3))))))))))))))
p_atom_397 () =
  let l0 = (let l0 = (let l0 = (let l0 = (let l0 = paren_383 (lazy_232 p_apply_403) in p_or_382 (l0, p_operator_396 ())) in p_or_382 (l0, p_num_391 ())) in p_or_382 (l0, p_bool_390 ())) in p_or_382 (l0, p_var_394 ())) in spaced_381 l0


parse_expression_404 :: (Iter Word8) -> (BadGood ParseError NamedExpression)
parse_expression_404 l0 =
  parse_279 (p_apply_403 (), (V.fromList [77, 105, 110, 72, 83]), l0)


type_eq_406 :: (Type, Type) -> Bool
type_eq_406 (l0, l1) =
  case (l0, l1) of ((Integer), (Integer)) -> True; ((Boolean), (Boolean)) -> True; ((Func (l2, l3)), (Func (l4, l5))) -> (case type_eq_406 (l2, l4) of False -> False; True -> type_eq_406 (l3, l5)); (l_0, l_1) -> False


type_print_407 :: Type -> (Vector Word8) 
type_print_407 l0 =
  case l0 of (Dummy) -> (V.fromList [42]); (Integer) -> (V.fromList [105, 110, 116]); (Boolean) -> (V.fromList [98, 111, 111, 108]); (Func (l1, l2)) -> (let l3 = (let l3 = paren_func_405 l1 in concat_122 (l3, (V.fromList [32, 45, 62, 32]))) in concat_122 (l3, paren_func_405 l2))
paren_func_405 l0 =
  case l0 of (Func (l_0, l_1)) -> (let l1 = (let l1 = (V.fromList [40]) in concat_122 (l1, type_print_407 l0)) in concat_122 (l1, (V.fromList [41]))); l_0 -> type_print_407 l0


dename_408 :: ((Vector ((Vector Word8) )) , NamedExpression) -> Expression
dename_408 (l0, l1) =
  case l1 of (NVar l2) -> (case find_index_62 (l0, (\ l3 -> equal_121 (l2, l3))) of (None) -> panic (let l3 = (V.fromList [118, 97, 114, 105, 97, 98, 108, 101, 32, 110, 111, 116, 32, 105, 110, 32, 115, 99, 111, 112, 101, 58, 32]) in concat_122 (l3, l2)); (Some l3) -> EVar l3); (NNum l2) -> ENum l2; (NBool l2) -> EBool l2; (NOp l2) -> EOp l2; (NApply (l2, l3)) -> EApply (dename_408 (l0, l2), dename_408 (l0, l3)); (NIf (l2, l3, l4)) -> EIf (dename_408 (l0, l2), dename_408 (l0, l3), dename_408 (l0, l4)); (NLet (l2, l3, l4, l5)) -> (let l6 = (push_55 ()) (l0, l2); l7 = (len_54 ()) l0 in ELet (l7, l3, dename_408 (l0, l4), dename_408 (l6, l5))); (NFun (l2, l3, l4, l5)) -> (let l6 = (let l6 = (let l6 = l0 in (push_55 ()) (l6, l2)) in (push_55 ()) (l6, l3)); (l7, l8) = ((len_54 ()) l0, uncurry (+) ((len_54 ()) l0, 1)) in EFun (l7, l8, l4, dename_408 (l6, l5)))


dename_expression_409 :: NamedExpression -> Expression
dename_expression_409 l0 =
  dename_408 ((V.fromList []), l0)


var_print_410 :: Int64 -> (Vector Word8) 
var_print_410 l0 =
  concat_122 ((V.fromList [36]), int_to_string_115 l0)


minhs_print_412 :: MinHS -> (Vector Word8) 
minhs_print_412 l0 =
  case l0 of (HNum l1) -> int_to_string_115 l1; (HBool l1) -> bool_to_string_113 l1; (HApply (l1, l2)) -> (let l3 = (let l3 = (let l3 = (let l3 = (V.fromList [40, 65, 112, 112, 32]) in concat_122 (l3, minhs_print_412 l1)) in concat_122 (l3, (V.fromList [32]))) in concat_122 (l3, minhs_print_412 l2)) in concat_122 (l3, (V.fromList [41]))); (HLet (l1, l_0)) -> (let l2 = (let l2 = (V.fromList [40, 76, 101, 116, 32]) in concat_122 (l2, minhs_print_412 l1)) in concat_122 (l2, (V.fromList [32, 40, 46, 46, 46, 41, 41]))); l_0 -> (V.fromList [40, 46, 46, 46, 41])


operator_print_413 :: Operator -> (Vector Word8) 
operator_print_413 l0 =
  case l0 of (Neg) -> (V.fromList [45]); (Not) -> (V.fromList [126]); (Add) -> (V.fromList [43]); (Mul) -> (V.fromList [42]); (And) -> (V.fromList [38]); (Or) -> (V.fromList [124]); (Eq) -> (V.fromList [61]); (Lt) -> (V.fromList [60])


expression_print_411 :: Expression -> (Vector Word8) 
expression_print_411 l0 =
  case l0 of (EVar l1) -> var_print_410 l1; (ENum l1) -> int_to_string_115 l1; (EBool l1) -> bool_to_string_113 l1; (EOp l1) -> operator_print_413 l1; (EIf (l1, l2, l3)) -> (let l4 = (let l4 = (let l4 = (let l4 = (let l4 = (V.fromList [105, 102, 32]) in concat_122 (l4, expression_print_411 l1)) in concat_122 (l4, (V.fromList [32, 116, 104, 101, 110, 32]))) in concat_122 (l4, expression_print_411 l2)) in concat_122 (l4, (V.fromList [32, 101, 108, 115, 101, 32]))) in concat_122 (l4, expression_print_411 l3)); (EApply (l1, l2)) -> (let l3 = (let l3 = (let l3 = expression_print_411 l1 in concat_122 (l3, (V.fromList [40]))) in concat_122 (l3, expression_print_411 l2)) in concat_122 (l3, (V.fromList [41]))); (ELet (l1, l_0, l2, l3)) -> (let l4 = (let l4 = (let l4 = (let l4 = (let l4 = (V.fromList [108, 101, 116, 32]) in concat_122 (l4, var_print_410 l1)) in concat_122 (l4, (V.fromList [32, 61, 32]))) in concat_122 (l4, expression_print_411 l2)) in concat_122 (l4, (V.fromList [32, 105, 110, 32]))) in concat_122 (l4, expression_print_411 l3)); (EFun (l1, l2, l_0, l3)) -> (let l4 = (let l4 = (let l4 = (let l4 = (let l4 = (V.fromList [102, 117, 110, 32]) in concat_122 (l4, var_print_410 l1)) in concat_122 (l4, (V.fromList [32]))) in concat_122 (l4, var_print_410 l2)) in concat_122 (l4, (V.fromList [32, 61, 32]))) in concat_122 (l4, expression_print_411 l3))


get_operator_type_415 :: Operator -> Type
get_operator_type_415 l0 =
  case l0 of (Not) -> Func (Boolean, Boolean); (Neg) -> Func (Integer, Integer); (Add) -> Func (Integer, Func (Integer, Integer)); (Mul) -> Func (Integer, Func (Integer, Integer)); (And) -> Func (Boolean, Func (Boolean, Boolean)); (Or) -> Func (Boolean, Func (Boolean, Boolean)); (Eq) -> Func (Integer, Func (Integer, Boolean)); (Lt) -> Func (Integer, Func (Integer, Boolean))


infer_type_414 :: ((HashMap Int64 Type), Expression) -> (Result Type ((Expression, Type, Type)))
infer_type_414 (l0, l1) =
  case l1 of (ENum l_0) -> Ok Integer; (EBool l_1) -> Ok Boolean; (EOp l2) -> Ok (get_operator_type_415 l2); (EVar l2) -> (case get_345 (l0, l2) of (Some l3) -> Ok l3; (None) -> panic (concat_122 ((V.fromList [117, 110, 98, 111, 117, 110, 100, 32, 118, 97, 114, 105, 97, 98, 108, 101, 58, 32]), int_to_string_115 l2))); (EIf (l2, l3, l4)) -> (let l5 = infer_type_414 (l0, l2); l6 = infer_type_414 (l0, l3); l7 = infer_type_414 (l0, l4) in (case (l5, l6, l7) of ((Ok (Boolean)), (Ok l8), (Ok l9)) -> (case type_eq_406 (l8, l9) of True -> Ok l8; False -> Err (l4, l8, l9)); ((Ok l8), l_0, l_1) -> Err (l2, Boolean, l8); ((Err (l_2, l_3, l_4)), l_5, l_6) -> l5; (l_7, (Err (l_8, l_9, l_10)), l_11) -> l6; (l_12, l_13, (Err (l_14, l_15, l_16))) -> l7)); (EApply (l2, l3)) -> (let l4 = infer_type_414 (l0, l2); l5 = infer_type_414 (l0, l3) in (case l5 of (Ok l6) -> (case l4 of (Ok (Func (l7, l8))) -> (case type_eq_406 (l7, l6) of True -> Ok l8; False -> Err (l3, l8, l6)); (Ok l7) -> Err (l1, Func (Dummy, Dummy), l7); (Err (l_0, l_1, l_2)) -> l4); (Err (l_3, l_4, l_5)) -> l5)); (ELet (l2, l3, l4, l5)) -> (case infer_type_414 (l0, l4) of (Err (l6, l7, l8)) -> Err (l6, l7, l8); (Ok l6) -> (case type_eq_406 (l6, l3) of True -> infer_type_414 (insert_348 (l0, l2, l3), l5); False -> Err (l4, l3, l6))); (EFun (l2, l3, l4, l5)) -> (case l4 of (Func (l6, l7)) -> (let l8 = (let l8 = (let l8 = l0 in insert_348 (l8, l2, l4)) in insert_348 (l8, l3, l6)) in (case infer_type_414 (l8, l5) of (Err (l9, l10, l11)) -> Err (l9, l10, l11); (Ok l9) -> (case type_eq_406 (l9, l7) of True -> Ok l4; False -> Err (l5, l7, l9)))); l_0 -> Err (l1, Func (Dummy, Dummy), l4))


substitution_416 :: (Int64, MinHS) -> (Int64 -> (Result MinHS Int64))
substitution_416 (l0, l1) =
  \ l2 -> case uncurry (==) (l0, l2) of True -> Ok l1; False -> Err l2


compose_sub_417 :: ((Int64 -> (Result MinHS Int64)), (Int64 -> (Result MinHS Int64))) -> (Int64 -> (Result MinHS Int64))
compose_sub_417 (l0, l1) =
  \ l2 -> either_104 (l0 l2, l1 l2)


cant_apply1_418 :: (Operator, MinHS) -> (Vector Word8) 
cant_apply1_418 (l0, l1) =
  let l2 = (let l2 = (let l2 = (V.fromList [99, 97, 110, 39, 116, 32, 97, 112, 112, 108, 121, 32, 111, 112, 101, 114, 97, 116, 111, 114, 32]) in concat_122 (l2, operator_print_413 l0)) in concat_122 (l2, (V.fromList [32, 116, 111, 32]))) in concat_122 (l2, minhs_print_412 l1)


cant_apply2_419 :: (Operator, MinHS, MinHS) -> (Vector Word8) 
cant_apply2_419 (l0, l1, l2) =
  let l3 = (let l3 = cant_apply1_418 (l0, l1) in concat_122 (l3, (V.fromList [32, 97, 110, 100, 32]))) in concat_122 (l3, minhs_print_412 l2)


eval_421 :: MinHS -> MinHS
eval_421 l0 =
  case l0 of (HIf (l1, l2, l3)) -> (case eval_421 l1 of (HBool True) -> eval_421 l2; (HBool False) -> eval_421 l3; l_0 -> panic ((V.fromList [117, 110, 114, 101, 97, 99, 104, 97, 98, 108, 101, 32, 99, 111, 100, 101]))); (HLet (l1, l2)) -> (let l3 = l2 l1 in eval_421 l3); (HApply (l1, l2)) -> (case eval_421 l1 of (HLam l3) -> (let l4 = l3 l2 in eval_421 l4); (HFun l3) -> (let l4 = (l3 l1) l2 in eval_421 l4); l_0 -> panic (let l3 = (let l3 = (let l3 = (V.fromList [99, 97, 110, 110, 111, 116, 32, 97, 112, 112, 108, 121, 32]) in concat_122 (l3, minhs_print_412 l1)) in concat_122 (l3, (V.fromList [32, 116, 111, 32]))) in concat_122 (l3, minhs_print_412 l2))); l_0 -> l0


lift_expression_420 :: (Expression, (Int64 -> (Result MinHS Int64))) -> MinHS
lift_expression_420 (l0, l1) =
  case l0 of (EVar l2) -> unwrap_105 (l1 l2); (ENum l2) -> HNum l2; (EBool l2) -> HBool l2; (EOp l2) -> (case l2 of (Neg) -> HLam (\ l3 -> case eval_421 l3 of (HNum l4) -> HNum (negate l4); l_1 -> panic (cant_apply1_418 (l2, l3))); (Not) -> HLam (\ l3 -> case eval_421 l3 of (HBool l4) -> HBool (not l4); l_2 -> panic (cant_apply1_418 (l2, l3))); (Add) -> HLam (\ l3 -> HLam (\ l4 -> case (eval_421 l3, eval_421 l4) of ((HNum l5), (HNum l6)) -> HNum (uncurry (+) (l5, l6)); l_3 -> panic (cant_apply2_419 (l2, l3, l4)))); (Mul) -> HLam (\ l3 -> HLam (\ l4 -> case (eval_421 l3, eval_421 l4) of ((HNum l5), (HNum l6)) -> HNum (uncurry (*) (l5, l6)); l_4 -> panic (cant_apply2_419 (l2, l3, l4)))); (And) -> HLam (\ l3 -> HLam (\ l4 -> case (eval_421 l3, eval_421 l4) of ((HBool l5), (HBool l6)) -> HBool (case l5 of False -> False; True -> l6); l_5 -> panic (cant_apply2_419 (l2, l3, l4)))); (Or) -> HLam (\ l3 -> HLam (\ l4 -> case (eval_421 l3, eval_421 l4) of ((HBool l5), (HBool l6)) -> HBool (case l5 of True -> True; False -> l6); l_6 -> panic (cant_apply2_419 (l2, l3, l4)))); (Eq) -> HLam (\ l3 -> HLam (\ l4 -> case (eval_421 l3, eval_421 l4) of ((HNum l5), (HNum l6)) -> HBool (uncurry (==) (l5, l6)); l_7 -> panic (cant_apply2_419 (l2, l3, l4)))); (Lt) -> HLam (\ l3 -> HLam (\ l4 -> case (eval_421 l3, eval_421 l4) of ((HNum l5), (HNum l6)) -> HBool (uncurry (<) (l5, l6)); l_8 -> panic (cant_apply2_419 (l2, l3, l4))))); (EIf (l2, l3, l4)) -> HIf (lift_expression_420 (l2, l1), lift_expression_420 (l3, l1), lift_expression_420 (l4, l1)); (EApply (l2, l3)) -> HApply (lift_expression_420 (l2, l1), lift_expression_420 (l3, l1)); (ELet (l2, l_9, l3, l4)) -> HLet (lift_expression_420 (l3, l1), (\ l5 -> let l6 = substitution_416 (l2, l5); l7 = compose_sub_417 (l6, l1) in lift_expression_420 (l4, l7))); (EFun (l2, l3, l_0, l4)) -> HFun (\ l5 -> \ l6 -> let l7 = substitution_416 (l2, l5); l8 = substitution_416 (l3, l6); l9 = (let l9 = (let l9 = l7 in compose_sub_417 (l9, l8)) in compose_sub_417 (l9, l1)) in lift_expression_420 (l4, l9))


run_program_422 :: (Vector Word8)  -> (Result MinHS (Vector Word8) )
run_program_422 l0 =
  let l1 = parse_expression_404 (items_19 l0) in (case l1 of (Bad l2) -> Err (showParseError_224 l2); (Good l2) -> (let l3 = dename_expression_409 l2; l4 = empty_342 (id_134, (\ (l4, l5) -> uncurry (==) (l4, l5))) in (case infer_type_414 (l4, l3) of (Ok l5) -> Ok (let l6 = lift_expression_420 (l3, (\ l6 -> Err l6)) in eval_421 l6); (Err (l5, l6, l7)) -> (let l8 = (let l8 = (let l8 = (let l8 = (let l8 = (let l8 = (V.fromList [84, 121, 112, 101, 32, 101, 114, 114, 111, 114, 58]) in concat_122 (l8, expression_print_411 l5)) in concat_122 (l8, (V.fromList [32, 104, 97, 115, 32, 116, 121, 112, 101, 32]))) in concat_122 (l8, type_print_407 l7)) in concat_122 (l8, (V.fromList [32, 98, 117, 116, 32, 101, 120, 112, 101, 99, 116, 101, 100, 32, 116, 121, 112, 101, 32]))) in concat_122 (l8, type_print_407 l6)) in Err l8))))


main_423 :: () -> ()
main_423 () =
  case string_to_nat_119 (input ()) of (Some l0) -> (let l1 = input () in (case repeat_355 (l0, (\ () -> run_program_422 l1)) of (Some (Ok l2)) -> output (minhs_print_412 l2); (Some (Err l2)) -> output l2; (None) -> output ((V.fromList [82, 101, 112, 101, 97, 116, 101, 100, 32, 60, 32, 49, 32, 116, 105, 109, 101, 115])))); (None) -> output ((V.fromList [69, 110, 116, 101, 114, 32, 105, 116, 101, 114, 97, 116, 105, 111, 110, 32, 99, 111, 117, 110, 116, 32, 97, 110, 100, 32, 109, 105, 110, 104, 115, 32, 112, 114, 111, 103, 114, 97, 109]))



main :: IO ()
main = main_423 () `seq` return ()
