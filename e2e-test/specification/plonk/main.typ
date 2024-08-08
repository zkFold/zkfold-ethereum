#import "@preview/unequivocal-ams:0.1.0": ams-article, theorem, proof, script-size, small-size, normal-size

#import "transaction.typ": *

#set align(center)
#set text(size: 18pt)
*End-to-end test for the Plonk verifier*

#set align(start)
#set text(size: 12pt)

*A ZK-KYC example:* Alice wants to mint and send Bob a token that represents a proof of some statement about Bob.

- The first transaction is a setup that needs to be performed only once for this application;
- In the second transaction, Alice minting tokens and sends ether + tokens to Bob;
- In the third transaction, Bob burning tokens.

#set text(size: small-size)
#v(3em)
#transaction(
  [*Upload script to blockchain*],
  inputs: (
    (
      name: "Someone",
      address: "Public key hash"
    ),
  ),
  outputs: (
    (
      name: "Plonk: setup Above 18",
      address: "zkfold-main"
    ),
  ),
  signatures: (
    "Someone",
  ),
  notes: [Someone posts the Plonk verifier script on-chain.]
)
#v(4em)

#transaction(
  [*Upload script to blockchain*],
  inputs: (
    (
      name: "Someone",
      address: "Public key hash"
    ),
  ),
  outputs: (
    (
      name: "Forwarding minting",
      address: "zkfold-main"
    ),
  ),
  signatures: (
    "Someone",
  ),
  notes: [Someone posts the Plonk verifier script on-chain.]
)
#v(4em)

#transaction(
  [*Transfer transaction*],
  inputs: (
    (
      name: "Stas",
      address: "Public key hash",
      value: (
        ether: 100
      )
    ),
  ),
  outputs: (
    (
      name: "Symbolhash to Plonk \"Above 18?\"",
      address: "Forwarding mint script",
      value: (
        ether: 100
      ),
      storage: (
        "input": "<setup address>"
      )
    ),
  ),
  signatures: (
    "Stas",
  ),
  notes: [Stas sents ether to a smart contract address.]
)
#v(10em)

#transaction(
  [*Minting transaction*],
  inputs: (
    (
      name: "Alice",
      address: "Public key hash"
    ),
    (
      name: "Plonk: setup Above 18",
      reference: true,
      address: "zkfold-main"
    )
  ),
  outputs: (
    (
      name: "Bob",
      address: "Public key hash",
      value: (
        tokenName: 1,
      )
    ),
  ),
  signatures: (
    "Alice",
  ),
  notes: [Alice sents ether and plonk tokens to Bob.]
)
#v(4em)

#transaction(
  [*Burning transaction*],
  inputs: (
    (
      name: "Bob",
      address: "Public key hash",
      value: (
        tokenName: 1,
      )
    ),
    (
      name: "Symbolhash to Plonk \"Above 18?\"",
      address: "Forwarding mint script",
      value: (
        ether: 100
      ),
      storage: (
        "input": "<setup address>"
      )
    ),
    (
      name: "Plonk: setup Above 18",
      address: "zkfold-main",
      reference: true
    ),
    (
      name: "Forwarding minting",
      address: "zkfold-main",
      reference: true
    ),
  ),
  outputs: (
    (
      name: "Bob",
      address: "Public key hash",
      value: (
        ether: 100,
      ),
    ),
  ),
  signatures: (
    "Bob",
  ),
  notes: [Bob burn plonk tokens.]
)