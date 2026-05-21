-- | The `HasLog` type class: environments that carry a `LogAction`.
-- |
-- | Port of @Colog.Core.Class@. Uses a multi-parameter class with a functional
-- | dependency `env -> msg m` (required for PureScript inference) and a
-- | hand-rolled van-Laarhoven `Lens'`, so there is no dependency on a lens
-- | library — exactly as in the Haskell original.
module Colog.Core.Class
  ( class HasLog
  , getLogAction
  , setLogAction
  , overLogAction
  , logActionL
  , Lens'
  , lens
  ) where

import Prelude

import Colog.Core.Action (LogAction)

-- | A monomorphic van-Laarhoven lens.
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- | Build a `Lens'` from a getter and a setter.
lens :: forall s a. (s -> a) -> (s -> a -> s) -> Lens' s a
lens getter setter f s = setter s <$> f (getter s)

-- | Environments that contain a `LogAction m msg`. The fundep `env -> msg m`
-- | says the environment determines the message and monad types.
-- |
-- | (PureScript has no default method implementations, so every instance must
-- | provide all four members. The `lens` helper makes `logActionL` a one-liner.)
class HasLog env msg m | env -> msg m where
  getLogAction :: env -> LogAction m msg
  setLogAction :: LogAction m msg -> env -> env
  overLogAction :: (LogAction m msg -> LogAction m msg) -> env -> env
  logActionL :: Lens' env (LogAction m msg)

-- | A `LogAction` is trivially its own environment.
instance HasLog (LogAction m msg) msg m where
  getLogAction = identity
  setLogAction = const
  overLogAction = identity
  logActionL f s = s <$ f s
