#import "@preview/unequivocal-ams:0.1.0": ams-article, theorem, proof, script-size, small-size, normal-size

#import "transaction.typ": *

#set align(center)
#set text(size: 18pt)
*End-to-end test for the Symbolic verifier*

#set align(start)
#set text(size: 12pt)

*A ZK-KYC example:* Alice wants to send ether to Bob, which he can then use on the condition that he is above 18 years old.

- The first transaction is a setup that needs to be performed only once for this application;
- In the second transaction, Alice sends ether to a smart contract;
- In the third transaction, Bob proves that he is above 18 year old and withdraws the funds to his wallet address.

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
      name: "Symbolic: setup Above 18 + <Bob address>",
      address: "stake pool"
    ),
  ),
  signatures: (
    "Someone",
  ),
  notes: [Someone posts the Symbolic verifier script and the "Above 18?" forwarding script on-chain.]
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
      name: "Forwarding reward",
      address: "zkfold-main"
    ),
  ),
  signatures: (
    "Someone",
  ),
  notes: [Someone posts the Symbolic verifier script and the "Above 18?" forwarding script on-chain.]
)
#v(4em)

#transaction(
  [*Transfer transaction*],
  inputs: (
    (
      name: "Alice",
      address: "Public key hash",
      value: (
        ether: 100
      )
    ),
  ),
  outputs: (
    (
      name: "Scripthash to Symbolic \"Above 18?\"",
      address: "Forwarding reward script",
      value: (
        ether: 100
      ),
      storage: (
        "input": "<setup address>"
      )
    ),
  ),
  signatures: (
    "Alice",
  ),
  notes: [Alice sents ether to a smart contract address.]
)
#v(4em)

#transaction(
  [*Withdraw transaction*],
  inputs: (
    (
      name: "Bob",
      address: "Public key hash"
    ),
    (
      name: "Scripthash to Symbolic \"Above 18?\"",
      address: "Forwarding reward script",
      value: (
        ether: 100
      ),
      storage: (
        "input": "<setup address>"
      )
    ),
    (
      name: "Symbolic: setup Above 18 + <Bob address>",
      address: "stake pool"
    ),
    (
      name: "Forwarding reward",
      address: "zkfold-main"
    ),
  ),
  outputs: (
    (
      name: "Bob",
      address: "Public key hash",
      value: (
        ether: 100
      ),
    ),
  ),
  staking: (
    "zkFold Symbolic verifier",
  ),
  signatures: (
    "Bob",
  ),
  notes: [Bob must prove that he is above 18 to withdraw ether sent by Alice.]
)