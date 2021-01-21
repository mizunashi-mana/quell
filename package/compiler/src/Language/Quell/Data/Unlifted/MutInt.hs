module Language.Quell.Data.Unlifted.MutInt (
    T#,
    MutInt# (..),
    new#,
    read#,
    write#,
    modify#,
) where

import           Language.Quell.Prelude


type T# = MutInt#

newtype MutInt# s = MutInt#
    {
        unMutInt# :: MutableByteArray# s
    }

new# :: Int# -> State# s -> (# State# s, MutInt# s #)
new# i# s0# =
    let intSize# = sizeOf# @Int 0
        !(# s1#, arr# #) = newByteArray# intSize# s0#
        s2# = writeIntArray# arr# 0# i# s1#
    in (# s2#, MutInt# arr# #)

read# :: MutInt# s -> State# s -> (# State# s, Int# #)
read# (MutInt# arr#) s0# = readIntArray# arr# 0# s0#

write# :: MutInt# s -> Int# -> State# s -> State# s
write# (MutInt# arr#) i# s0# = writeIntArray# arr# 0# i# s0#

modify# :: MutInt# s -> (Int# -> Int#) -> State# s -> State# s
modify# mi# f s0# =
    let !(# s1#, i# #) = read# mi# s0#
        s2# = write# mi#
            do f i#
            do s1#
    in s2#
