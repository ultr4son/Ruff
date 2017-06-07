module RuffByteCode where

import RuffModel
import Data.Binary

data RuffByte =
  ClearScope
| ExecGetIt{functionName::String, args::[FArg], result::Reference}
| ExecGet{named::String, functionName::String, args::FArg, result::Reference}
| ExecPutIt{functionName::String, args::[FArg], writeVal::Reference}
| ExectPut{named::String, functionName::String, args::[FArg], writeVal::Reference}


class ByteCodeConvertable a where
  toByteCode::a->[RuffByte]

instance ByteCodeConvertable Statement where
  toByteCode (Get g n) = ClearScope:(toByteCode g):(restOf n) where
    restOf::Statement -> RuffByte
    restOf (G g n) = (toByteCode g):(restOf n)
    restOf (P p) = (toByteCode p):[]

instance ByteCodeConvertable Get where
  toByteCode (Get It (Function name args result)) = [(ExecGetIt name args result)]
  toByteCode (Get (Named n) (Function name args result)) = [(ExectGet n name args result)]

instance ByteCodeConvertable Put where
  toByteCode (Put (Function name args _) value It) = [(ExecPutIt name args value)]
  toByteCode (Put (Function name args _) value (Named n)) = [(ExecPut n name args value)]
