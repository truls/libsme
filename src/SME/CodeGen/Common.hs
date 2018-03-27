{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module SME.CodeGen.Common
  ( Env
  , TopDef
  , DefType
  , OutputPlan
  , OutputFile (..)
  , GenM
  , runGenM
  , execGenM
  ) where

import           Control.Monad.Except   (MonadError)
import           Control.Monad.Identity (Identity)
import           Control.Monad.State    (MonadState)
import qualified Data.Text              as T

import           Language.SMEIL.Pretty
import           SME.Error
import           SME.Representation

type Env = BaseEnv Void
type TopDef = BaseTopDef Void
type DefType = BaseDefType Void

type OutputPlan = [OutputFile]

data OutputFile = OutputFile
  { destFile :: FilePath
  , ext      :: String
  , content  :: T.Text
  }

newtype GenM a = GenM
  {unGenM :: ReprM Identity Void a }
               deriving (Functor,
                         Applicative,
                         Monad,
                         MonadState Env,
                         MonadError TypeCheckErrors)

instance (MonadRepr Void) GenM

runGenM :: Env -> GenM a -> (Either TypeCheckErrors a, Env)
runGenM env act = runReprMidentity env $ unGenM act

execGenM :: Env -> GenM a -> Env
execGenM env act =
  let (_, res) = runGenM env act
  in res
