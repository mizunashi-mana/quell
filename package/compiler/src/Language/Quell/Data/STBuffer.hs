module Language.Quell.Data.STBuffer (
    T,
    STBuffer (..),
    new,
    length,
    isEmpty,
    index,
    unsafeIndex,
    head,
    unsafeHead,
    last,
    unsafeLast,
    toList,
    appendHead,
    appendLast,
    consumeHead,
    consumeLast,
    unsafeConsumeHead,
    unsafeConsumeLast,
    consumeHeads,
    consumeLasts,
    unsafeConsumeHeads,
    unsafeConsumeLasts,
) where

import Language.Quell.Prelude hiding (length, index, unsafeIndex, unsafeHead, last, unsafeLast)

import qualified Language.Quell.Data.Unlifted.MutInt as MutInt
import qualified GHC.ST as ST
import qualified Unsafe.Coerce as Unsafe


type T = STBuffer

data STBuffer s a = STBuffer
    {
        unSTBuffer# :: MutVar# s (STBufferArray s a),
        indexHead# :: MutInt.T# s,
        bufferLength# :: MutInt.T# s
    }

data STBufferArray s a = STBufferArray
    {
        unSTBufferArray# :: MutableArray# s a
    }

new :: forall e s. [e] -> ST s (STBuffer s e)
new xs0 = ST.ST \s0# ->
    let dlen# = 16# -- must be positive number
        !(# s1#, miH# #) = MutInt.new# 0# s0#
    in case xs0 of
        [] ->
            let x0 = Unsafe.unsafeCoerce Nothing :: e
                !(# s2#, arr0# #) = newArray# dlen# x0 s1#
                bufferArr = STBufferArray arr0#
                !(# s3#, marr# #) = newMutVar# bufferArr s2#
                !(# s4#, mBufLen# #) = MutInt.new# 0# s3#
                buffer = STBuffer
                    {
                        unSTBuffer# = marr#,
                        indexHead# = miH#,
                        bufferLength# = mBufLen#
                    }
            in (# s4#, buffer #)
        x:xs ->
            let !(# s2#, arr0# #) = newArray# dlen# x s1#
                !(# s3#, arr1#, bufLen1# #) = go# arr0# 1# dlen# s2# xs
                bufferArr = STBufferArray arr1#
                !(# s4#, marr# #) = newMutVar# bufferArr s3#
                !(# s5#, mBufLen# #) = MutInt.new# bufLen1# s4#
                buffer = STBuffer
                    {
                        unSTBuffer# = marr#,
                        indexHead# = miH#,
                        bufferLength# = mBufLen#
                    }
            in (# s5#, buffer #)
    where
        go# arr0# i# len0# s0# = \case
            [] -> (# s0#, arr0#, i# #)
            x:xs -> case i# <# len0# of
                0# ->
                    let len1# = len0# *# 2#
                        !(# s1#, arr1# #) = newArray# len1# x s0#
                        s2# = copyMutableArray# arr0# 0# arr1# 0# len0# s1#
                    in go# arr1# (i# +# 1#) len1# s2# xs
                _ ->
                    let s1# = writeArray# arr0# i# x s0#
                    in go# arr0# (i# +# 1#) len0# s1# xs

length :: STBuffer s e -> ST s Int
length buf = ST.ST \s0# ->
    let !(# s1#, len# #) = MutInt.read#
            do bufferLength# buf
            do s0#
    in (# s1#, I# len# #)

isEmpty :: STBuffer s e -> ST s Bool
isEmpty buf = ST.ST \s0# ->
    let !(# s1#, len# #) = MutInt.read#
            do bufferLength# buf
            do s0#
    in case len# of
        0# -> (# s1#, True #)
        _  -> (# s1#, False #)

index :: STBuffer s e -> Int -> ST s (Maybe e)
index buf (I# i#) = ST.ST \s0# ->
    let !(# s1#, bufLen# #) = MutInt.read#
            do bufferLength# buf
            do s0#
    in case i# <# bufLen# of
        0# -> (# s1#, Nothing #)
        _  ->
            let !(# s2#, x #) = unsafeIndex# buf i# s1#
            in (# s2#, Just x #)

unsafeIndex :: STBuffer s e -> Int -> ST s e
unsafeIndex buf (I# i#) = ST.ST \s0# -> unsafeIndex# buf i# s0#

unsafeIndex# :: STBuffer s e -> Int# -> State# s -> (# State# s, e #)
unsafeIndex# buf i# s0# =
    let !(# s1#, iH# #) = MutInt.read#
            do indexHead# buf
            do s0#
        !(# s2#, arr #) = readMutVar#
            do unSTBuffer# buf
            do s1#
        arr# = unSTBufferArray# arr
        arrLen# = sizeofMutableArray# arr#
        arrIndex# = incIndex# arrLen# iH# i#
        !(# s3#, x #) = readArray# arr# arrIndex# s2#
    in (# s3#, x #)

head :: STBuffer s e -> ST s (Maybe e)
head buf = index buf 0

unsafeHead :: STBuffer s e -> ST s e
unsafeHead buf = unsafeIndex buf 0

last :: STBuffer s e -> ST s (Maybe e)
last buf = ST.ST \s0# ->
    let !(# s1#, bufLen# #) = MutInt.read#
            do bufferLength# buf
            do s0#
    in case bufLen# of
        0# -> (# s1#, Nothing #)
        _  ->
            let !(# s2#, x #) = unsafeIndex# buf
                    do bufLen# -# 1#
                    do s1#
            in (# s2#, Just x #)

unsafeLast :: STBuffer s e -> ST s e
unsafeLast buf = ST.ST \s0# ->
    let !(# s1#, bufLen# #) = MutInt.read#
            do bufferLength# buf
            do s0#
        !(# s2#, x #) = unsafeIndex# buf
            do bufLen# -# 1#
            do s1#
    in (# s2#, x #)

appendHead :: e -> STBuffer s e -> ST s ()
appendHead x buf = ST.ST \s0# ->
    let !(# s1#, arr0 #) = readMutVar#
            do unSTBuffer# buf
            do s0#
        arr0# = unSTBufferArray# arr0
        arr0Len# = sizeofMutableArray# arr0#
        !(# s2#, bufLen0# #) = MutInt.read#
            do bufferLength# buf
            do s1#
    in case bufLen0# <# arr0Len# of
        0# ->
            let !(# s3#, arr1Len# #) =
                    reallocBufferArray arr0# arr0Len# bufLen0# x buf s2#
                s4# = MutInt.modify#
                    do indexHead# buf
                    do \i# -> decIndex# arr1Len# i# 1#
                    do s3#
                s5# = MutInt.write#
                    do bufferLength# buf
                    do bufLen0# +# 1#
                    do s4#
            in (# s5#, () #)
        _ ->
            let !(# s3#, iH0# #) = MutInt.read#
                    do indexHead# buf
                    do s2#
                iH1# = decIndex# arr0Len# iH0# 1#
                s4# = writeArray# arr0# iH1# x s3#
                s5# = MutInt.write#
                    do indexHead# buf
                    do iH1#
                    do s4#
                s6# = MutInt.write#
                    do bufferLength# buf
                    do bufLen0# +# 1#
                    do s5#
            in (# s6#, () #)

appendLast :: e -> STBuffer s e -> ST s ()
appendLast x buf = ST.ST \s0# ->
    let !(# s1#, arr0 #) = readMutVar#
            do unSTBuffer# buf
            do s0#
        arr0# = unSTBufferArray# arr0
        arr0Len# = sizeofMutableArray# arr0#
        !(# s2#, bufLen0# #) = MutInt.read#
            do bufferLength# buf
            do s1#
    in case bufLen0# <# arr0Len# of
        0# ->
            let !(# s3#, _ #) =
                    reallocBufferArray arr0# arr0Len# bufLen0# x buf s2#
                s4# = MutInt.write#
                    do bufferLength# buf
                    do bufLen0# +# 1#
                    do s3#
            in (# s4#, () #)
        _ ->
            let !(# s3#, iH0# #) = MutInt.read#
                    do indexHead# buf
                    do s2#
                iL0# = incIndex# arr0Len# iH0# bufLen0#
                s4# = writeArray# arr0# iL0# x s3#
                s5# = MutInt.write#
                    do bufferLength# buf
                    do bufLen0# +# 1#
                    do s4#
            in (# s5#, () #)

consumeHead :: STBuffer s e -> ST s (Maybe e)
consumeHead buf = ST.ST \s0# ->
    let !(# s1#, bufLen0# #) = MutInt.read#
            do bufferLength# buf
            do s0#
    in case bufLen0# of
        0# -> (# s1#, Nothing #)
        _ ->
            let !(# s2#, x #) = unsafeConsumeHead# bufLen0# buf s1#
            in (# s2#, Just x #)

unsafeConsumeHead :: STBuffer s e -> ST s e
unsafeConsumeHead buf = ST.ST \s0# ->
    let !(# s1#, bufLen0# #) = MutInt.read#
            do bufferLength# buf
            do s0#
    in unsafeConsumeHead# bufLen0# buf s1#

consumeLast :: STBuffer s e -> ST s (Maybe e)
consumeLast buf = ST.ST \s0# ->
    let !(# s1#, bufLen0# #) = MutInt.read#
            do bufferLength# buf
            do s0#
    in case bufLen0# of
        0# -> (# s1#, Nothing #)
        _ ->
            let !(# s2#, x #) = unsafeConsumeLast# bufLen0# buf s1#
            in (# s2#, Just x #)

unsafeConsumeLast :: STBuffer s e -> ST s e
unsafeConsumeLast buf = ST.ST \s0# ->
    let !(# s1#, bufLen0# #) = MutInt.read#
            do bufferLength# buf
            do s0#
    in unsafeConsumeLast# bufLen0# buf s1#

consumeHeads :: STBuffer s e -> Int -> ST s (Maybe [e])
consumeHeads buf (I# c#) = ST.ST \s0# ->
    let !(# s1#, bufLen0# #) = MutInt.read#
            do bufferLength# buf
            do s0#
    in case c# <=# bufLen0# of
        0# -> (# s1#, Nothing #)
        _ ->
            let !(# s2#, xs #) = unsafeConsumeHeads# bufLen0# buf c# s1#
            in (# s2#, Just xs #)

unsafeConsumeHeads :: STBuffer s e -> Int -> ST s [e]
unsafeConsumeHeads buf (I# c#) = ST.ST \s0# ->
    let !(# s1#, bufLen0# #) = MutInt.read#
            do bufferLength# buf
            do s0#
    in unsafeConsumeHeads# bufLen0# buf c# s1#

consumeLasts :: STBuffer s e -> Int -> ST s (Maybe [e])
consumeLasts buf (I# c#) = ST.ST \s0# ->
    let !(# s1#, bufLen0# #) = MutInt.read#
            do bufferLength# buf
            do s0#
    in case c# <=# bufLen0# of
        0# -> (# s1#, Nothing #)
        _ ->
            let !(# s2#, xs #) = unsafeConsumeLasts# bufLen0# buf c# s1#
            in (# s2#, Just xs #)

unsafeConsumeLasts :: STBuffer s e -> Int -> ST s [e]
unsafeConsumeLasts buf (I# c#) = ST.ST \s0# ->
    let !(# s1#, bufLen0# #) = MutInt.read#
            do bufferLength# buf
            do s0#
    in unsafeConsumeLasts# bufLen0# buf c# s1#

toList :: STBuffer s e -> ST s [e]
toList buf = ST.ST \s0# ->
    let !(# s1#, iH# #) = MutInt.read#
            do indexHead# buf
            do s0#
        !(# s2#, arr #) = readMutVar#
            do unSTBuffer# buf
            do s1#
        arr# = unSTBufferArray# arr
        arrLen# = sizeofMutableArray# arr#
        !(# s3#, bufLen# #) = MutInt.read#
            do bufferLength# buf
            do s2#
        iL# = incIndex# arrLen# iH# bufLen#
        !(# s4#, xs #) = go# arr# arrLen# iH# iL# [] s3#
    in (# s4#, xs #)
    where
        go# arr# arrLen# iH# iL0# xs s0# = case iH# ==# iL0# of
            0# ->
                let iL1# = decIndex# arrLen# iL0# 1#
                    !(# s1#, x #) = readArray# arr# iL1# s0#
                in go# arr# arrLen# iH# iL1# (x:xs) s1#
            _  -> (# s0#, xs #)

unsafeConsumeHead# :: Int# -> STBuffer s e -> State# s -> (# State# s, e #)
unsafeConsumeHead# bufLen0# buf s0# =
    let !(# s1#, arr #) = readMutVar#
            do unSTBuffer# buf
            do s0#
        arr# = unSTBufferArray# arr
        arrLen# = sizeofMutableArray# arr#
        !(# s2#, iH0# #) = MutInt.read#
            do indexHead# buf
            do s1#
        !(# s3#, x #) = readArray# arr# iH0# s2#
        iH1# = incIndex# arrLen# iH0# 1#
        s4# = MutInt.write#
            do indexHead# buf
            do iH1#
            do s3#
        s5# = MutInt.write#
            do bufferLength# buf
            do bufLen0# -# 1#
            do s4#
    in (# s5#, x #)

unsafeConsumeLast# :: Int# -> STBuffer s e -> State# s -> (# State# s, e #)
unsafeConsumeLast# bufLen0# buf s0# =
    let !(# s1#, arr #) = readMutVar#
            do unSTBuffer# buf
            do s0#
        arr# = unSTBufferArray# arr
        arrLen# = sizeofMutableArray# arr#
        !(# s2#, iH0# #) = MutInt.read#
            do indexHead# buf
            do s1#
        bufLen1# = bufLen0# -# 1#
        iL1# = incIndex# arrLen# iH0# bufLen1#
        !(# s3#, x #) = readArray# arr# iL1# s2#
        s4# = MutInt.write#
            do bufferLength# buf
            do bufLen1#
            do s3#
    in (# s4#, x #)

unsafeConsumeHeads# :: Int# -> STBuffer s e -> Int# -> State# s -> (# State# s, [e] #)
unsafeConsumeHeads# bufLen0# buf c# = \s0# ->
    let !(# s1#, arr #) = readMutVar#
            do unSTBuffer# buf
            do s0#
        arr# = unSTBufferArray# arr
        arrLen# = sizeofMutableArray# arr#
        !(# s2#, iH0# #) = MutInt.read#
            do indexHead# buf
            do s1#
        iH1# = incIndex# arrLen# iH0# c#
        !(# s3#, xs #) = go# arr# arrLen# iH0# iH1# [] s2#
        s4# = MutInt.write#
            do indexHead# buf
            do iH1#
            do s3#
        s5# = MutInt.write#
            do bufferLength# buf
            do bufLen0# -# c#
            do s4#
    in (# s5#, xs #)
    where
        go# arr# arrLen# iH0# iH1# xs s0# = case iH0# ==# iH1# of
            0# ->
                let !(# s1#, x #) = readArray# arr# iH1# s0#
                    iH2# = decIndex# arrLen# iH1# 1#
                in go# arr# arrLen# iH0# iH2# (x:xs) s1#
            _  -> (# s0#, xs #)

unsafeConsumeLasts# :: Int# -> STBuffer s e -> Int# -> State# s -> (# State# s, [e] #)
unsafeConsumeLasts# bufLen0# buf c# = \s0# ->
    let !(# s1#, arr #) = readMutVar#
            do unSTBuffer# buf
            do s0#
        arr# = unSTBufferArray# arr
        arrLen# = sizeofMutableArray# arr#
        !(# s2#, iH0# #) = MutInt.read#
            do indexHead# buf
            do s1#
        iL0# = incIndex# arrLen# iH0# do bufLen0# -# 1#
        iL1# = decIndex# arrLen# iL0# c#
        !(# s3#, xs #) = go# arr# arrLen# iL0# iL1# [] s2#
        s4# = MutInt.write#
            do bufferLength# buf
            do bufLen0# -# c#
            do s3#
    in (# s4#, xs #)
    where
        go# arr# arrLen# iL0# iL1# xs s0# = case iL0# ==# iL1# of
            0# ->
                let !(# s1#, x #) = readArray# arr# iL1# s0#
                    iL2# = incIndex# arrLen# iL1# 1#
                in go# arr# arrLen# iL0# iL2# (x:xs) s1#
            _  -> (# s0#, xs #)

reallocBufferArray :: MutableArray# s e -> Int# -> Int# -> e
    -> STBuffer s e -> State# s -> (# State# s, Int# #)
reallocBufferArray arr0# arr0Len# bufLen0# x buf s0# =
    let !(# s1#, iH0# #) = MutInt.read#
            do indexHead# buf
            do s0#
        arr1Len# = arr0Len# *# 2#
        !(# s2#, arr1# #) = newArray# arr1Len# x s1#
        arr0LastLen# = case iH0# +# bufLen0# <=# arr0Len# of
            0# -> arr0Len# -# iH0#
            _  -> bufLen0#
        s3# = copyMutableArray# arr0# iH0# arr1# iH0#
            do arr0LastLen#
            do s2#
        s4# = case bufLen0# ==# arr0LastLen# of
            0# -> copyMutableArray# arr0# 0# arr1# arr0Len#
                do arr0LastLen# -# bufLen0#
                do s3#
            _  -> s3#
        arr1 = STBufferArray arr1#
        s5# = writeMutVar#
            do unSTBuffer# buf
            do arr1
            do s4#
    in (# s5#, arr1Len# #)

decIndex# :: Int# -> Int# -> Int# -> Int#
decIndex# len# i0# l# =
    let i1# = i0# -# l#
    in case i1# <# 0# of
        0# -> i1#
        _  -> i1# +# len#

incIndex# :: Int# -> Int# -> Int# -> Int#
incIndex# len# i0# l# =
    let i1# = i0# +# l#
    in case i1# >=# len# of
        0# -> i1#
        _  -> i1# -# len#
