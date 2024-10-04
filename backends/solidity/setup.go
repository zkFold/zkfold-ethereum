package main

import (
	"fmt"
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

	var circuit Circuit

	// // building the circuit...
	ccs, err := frontend.Compile(ecc.BN254.ScalarField(), scs.NewBuilder, &circuit)
	if err != nil {
		fmt.Println("circuit compilation error")
	}

	// create the necessary data for KZG.
	// This is a toy example, normally the trusted setup to build ZKG
	// has been run before.
	// The size of the data in KZG should be the closest power of 2 bounding //
	// above max(nbConstraints, nbVariables).
	scs := ccs.(*cs.SparseR1CS)
	srs, srsLagrange, err := unsafekzg.NewSRS(scs)
	if err != nil {
		panic(err)
	}

	{
		// w.X = 4
		// w.Y = 4

		_, vk, _ := plonk.Setup(ccs, srs, srsLagrange)

		// fmt.Println(vk)

		fSolidity, _ := os.Create(filepath.Join("..", "..", "e2e-test", "template", "symbolic", "contracts", "Verifier.sol"))

		vk.ExportSolidity(fSolidity)

		fSolidity.Close()
	}
}
