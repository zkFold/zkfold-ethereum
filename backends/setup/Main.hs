module Main where

import           Prelude                                     hiding (Bool, Eq (..), Fractional (..), Num (..), length)
import qualified Prelude                                     as Haskell

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

equalityCheckVerificationBytes :: 
     Fr
  -> PlonkupProverSecret BN254_G1
  -> Fr
  -> ArithmeticCircuit Fr (Vector 1) (Vector 1)
  -> SetupBytes
equalityCheckVerificationBytes x ps targetValue ac =
    let (omega, k1, k2) = getParams 32
        plonk   = Plonk omega k1 k2 ac x :: PlonkN 1 32
        setupV  = setupVerify @_ @HaskellCore plonk

    in mkSetup setupV

data EqualityCheckContract = EqualityCheckContract {
    x           :: Fr
  , ps          :: PlonkupProverSecret BN254_G1
  , targetValue :: Fr
} deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance FromJSON (V.Vector 19 Fr)

deriving anyclass instance ToJSON   (PlonkupProverSecret BN254_G1)
deriving anyclass instance FromJSON (PlonkupProverSecret BN254_G1)

main :: IO ()
main = do
  contract <- decode @EqualityCheckContract =<< BL.readFile "../test-data/plonk-raw-contract-data.json"
  
  ac <- readFileJSON "../test-data/equalityCheckContract"

  let setup = targetToProve contract ac
  
  createDirectoryIfMissing True "../assets"
  
  BS.writeFile "../assets/setup" $ fromString $ show $ input
