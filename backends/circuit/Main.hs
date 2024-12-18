module Main where

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
import Utils

equalityCheckContract :: forall a c . (Symbolic c, FromConstant a (BaseField c)) => a -> FieldElement c -> Bool c
equalityCheckContract targetValue inputValue = inputValue == fromConstant targetValue

main :: IO ()
main = do
  x           <- generate arbitrary
  ps          <- generate arbitrary
  targetValue <- generate arbitrary

  createDirectoryIfMissing Haskell.True "../test-data"

  let contract = EqualityCheckContract x ps targetValue
  BL.writeFile "../test-data/plonk-raw-contract-data.json" $ encode contract

  let ac = equalityCheckContract @Fr @(ArithmeticCircuit Fr (Vector 1)) targetValue
  compileIOWith "../test-data/equalityCheckContract" forceOne ac
