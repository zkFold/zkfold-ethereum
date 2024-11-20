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
import Utils


targetToProve :: 
     EqualityCheckContract
  -> ArithmeticCircuit Fr (Vector 1) (Vector 1)
  -> (InputBytes, ProofBytes)
targetToProve (EqualityCheckContract x ps targetValue) ac =
    let (omega, k1, k2) = getParams 32
        witnessInputs  = unsafeToVector [targetValue]
        plonk   = Plonk omega k1 k2 ac x :: PlonkN 1 32
        setupP  = setupProve @_ @HaskellCore plonk
        witness = (PlonkupWitnessInput witnessInputs, ps)
        (input, proof) = prove @(PlonkN 1 32) @HaskellCore setupP witness

    in (mkInput input, mkProof proof)

main :: IO ()
main = do
  contract <- decode @EqualityCheckContract =<< BL.readFile "../test-data/plonk-raw-contract-data.json"
  ac <- readFileJSON "../test-data/equalityCheckContract"

  let (input, proof) = targetToProve contract ac
  
  createDirectoryIfMissing True "../assets"
  
  BS.writeFile "../assets/input" $ fromString $ show $ input
  BS.writeFile "../assets/proof" $ fromString $ show proof
