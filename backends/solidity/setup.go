package main

import (
	"os"
	"path/filepath"

	"github.com/consensys/gnark-crypto/ecc"
	"github.com/consensys/gnark/backend/plonk"
	cs "github.com/consensys/gnark/constraint/bn254"
	"github.com/consensys/gnark/frontend/cs/scs"

	"github.com/consensys/gnark/frontend"
	"github.com/consensys/gnark/test/unsafekzg"
)

type Circuit struct {
	X frontend.Variable
	Y frontend.Variable `gnark:",public"`
}

// Define declares the circuit's constraints
func (circuit *Circuit) Define(api frontend.API) error {

	output := frontend.Variable(1)

	output = api.Mul(output, circuit.X)

	api.AssertIsEqual(circuit.Y, output)

	return nil
}

func main() {

	// import "../assets/setup"
	// hmmm import "../assets/kzg.SRS" ???
	// hmmm import "../assets/srsLagrange" ???

	var circuit Circuit

	ccs, _ := frontend.Compile(ecc.BN254.ScalarField(), scs.NewBuilder, &circuit)

	scs := ccs.(*cs.SparseR1CS)
	srs, srsLagrange, _ := unsafekzg.NewSRS(scs)

	_, vk, _ := plonk.Setup(ccs, srs, srsLagrange)

	fSolidity, _ := os.Create(filepath.Join("..", "..", "e2e-test", "template", "symbolic", "contracts", "Verifier.sol"))

	vk.ExportSolidity(fSolidity)

	fSolidity.Close()
}
