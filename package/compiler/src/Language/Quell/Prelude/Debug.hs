module Language.Quell.Prelude.Debug (
    debugTrace,
    debugTraceShow,
    debugTraceShowId,
) where

import           Language.Quell.Prelude.Core
import           Language.Quell.Prelude.Literal

import qualified Debug.Trace                    as Debug


debugTrace :: StringLit -> a -> a
debugTrace msg x = Debug.trace msg x

debugTraceShow :: Show a => a -> b -> b
debugTraceShow v x = Debug.traceShow v x

debugTraceShowId :: Show a => a -> a
debugTraceShowId x = Debug.traceShowId x
