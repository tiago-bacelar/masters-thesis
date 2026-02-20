module CompReal.Instances () where

{-

This module only reexports the instance declarations for all submodules in CompReal.Instances

We cannot export the types themselves because their names would conflict
(e.g. both CDAR and ERA call their types CR). When using multiple of these
types at once they should be imported separately and qualified, like so:
import qualified CompReal.Instances.CDAR as CDAR
import qualified CompReal.Instances.ERA as ERA

Since the submodules already reexport their respective types,
there is no reason to ever import this module
Why did I write this module anyways?

-}

import CompReal.Instances.CDAR ()
import CompReal.Instances.AERN2 ()
import CompReal.Instances.ERA ()
import CompReal.Instances.ExactReal ()
import CompReal.Instances.IReal ()