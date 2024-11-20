module Utils where

import           Prelude                                  (IO, putStr, show, ($), (++), (.), (<$>))
import           Prelude                               (FilePath, IO, Maybe (..), Show (..), putStr, ($),
                                                        (++), (.))
import qualified Prelude                                     as Haskell
import           Codec.CBOR.Write                         (toStrictByteString)
import           Data.Aeson                               (decode)
import qualified Data.Aeson                               as Aeson
import           Data.ByteString                          as BS (ByteString, writeFile)
import qualified Data.ByteString.Lazy                     as BL
import           Data.Maybe                               (fromJust)
import           Data.String                              (IsString (..))
import           Control.Monad                         (void)
import           Data.Aeson                            (encode)
import           System.Directory                      (createDirectoryIfMissing)
import           Test.QuickCheck.Arbitrary             (Arbitrary (..))
import           Test.QuickCheck.Gen                   (generate)

import           ZkFold.Base.Algebra.Basic.Class             (FromConstant (..))
import           ZkFold.Base.Algebra.EllipticCurve.BN254     (BN254_G1, Fr)
import           ZkFold.Base.Data.Vector                     (Vector(..), unsafeToVector)
import           ZkFold.Base.Protocol.Plonk
import           ZkFold.Base.Protocol.Plonkup.Witness
import           ZkFold.Base.Protocol.Plonkup.Prover.Secret
import           ZkFold.Base.Protocol.Plonkup.Utils          (getParams)
import           ZkFold.Base.Protocol.NonInteractiveProof    (NonInteractiveProof (..), HaskellCore)
import           ZkFold.Symbolic.Class
import           ZkFold.Symbolic.Compiler                    (ArithmeticCircuit (..), compileForceOne)
import           ZkFold.Symbolic.Data.Bool                   (Bool (..))
import           ZkFold.Symbolic.Data.Eq                     (Eq (..))
import           ZkFold.Symbolic.Data.FieldElement


data EqualityCheckContract = EqualityCheckContract {
    x           :: Fr
  , ps          :: PlonkupProverSecret BN254_G1
  , targetValue :: Fr
} deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance FromJSON (V.Vector 19 Fr)

deriving anyclass instance ToJSON   (PlonkupProverSecret BN254_G1)
deriving anyclass instance FromJSON (PlonkupProverSecret BN254_G1)

mkSetup = undefined
mkInput = undefined
mkProof = undefined

