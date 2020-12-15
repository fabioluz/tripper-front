module Tripper.Shared.Utils.Types where

import Tripper.Prelude
import Halogen (Slot)
 
type NoSlot = ()

type NoOutput = Void

type NoInput = Unit

data NoQuery a = Void

type OpaqueSlot slot = Slot NoQuery NoOutput slot