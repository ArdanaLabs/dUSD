# Low level offchain api spec

This is a spec for the low level api that will be used in the cli and browser components

# Admin only API

This part of the api will require the Admin wallet available using yubihsm

## Admin signing

I assume the signatures are all via yubihsm.
If the standard use case of yubihsm will be signing with 3 keys
we probably want to make some changes to facilitate this more
easily (ie. not with 3 separate calls)

## Initialize protocol

Inputs:
	The list of admin keys?
	The initial protocol parameters
	Initial price data? (we could just start with [])

Outputs: Protocol type which includes:
	Either
	- The Protocol parameter utxo's nft's currency symbol
	  and then addresses can be re computed?
	  It may be nice to make this small as it has to get sent arnound a lot
	  and recomputing is cheap anyway
	Or
	- The validators computed in the initialization procedure
	Or
	- Just the addresses because the policies
	  are provided by reference scripts
	Or
	- Nothing we could use the reference script utxos
	  and querry an admin controled address
    to just discover the protocol info as needed

Note the protocol type may need to be easy to broadcast

Should do:
- Create an NFT for the protocol parameters utxo
- Create an NFT for the price oracle utxo
- compute parameterized scripts
	- protocol address validator
	- price oracle address validator
	- vault address validator
	- vault validation token minting policy
	- dUSD minting policy
- send that nft to the protocol parameter address with the initial parameters
	- this may be a good place for a reference script?
- send that nft to the price oracle address with initial price data
	- this may also be a good place for a reference script?
- create reference script utxos as needed
	- We could make an admin controlled address and a token
		certifiying them and then look them up easily offchain.

I think the admin address for lookups makes the most sense
This doesn't require any additional trust from users
(ie. you could make a custom api not use this)
This solves the issue of communicating to the browser code
because the admin controlled address and currency symbol can be
known at compile time which means the offchain code
can discover all the protocol information from
scratch as needed.

## Add price data

Inputs: Protocol, the current price

Outputs: none

Should do:
- update the price data to include the new price
- drop any old price data

## Set Protocol parameters

Inputs: Protocol, New Parameters
Outputs: unit

## Trigger buffer auction

I'm not sure how this should work

# Public API

This part of the api should work with any wallet

## Get protocol

I'm not sure what the best way to get the protocol data to users would be.
For admin/the cli it can just read it from a file which the cli can create when
the protocol is initialized.
The browser tests on the other hand I'm not sure what's best:
- Hard codding
	- makes the most sense in production where the value is determined once and never changes
	forces lots of recompiling during tests
- Environment variable
	- easy for nix testing but not clear how that would work in production
- Serve it as a file
	- this might be good?
	I don't know enough web development and should ask Evan about this.
- Get it from onchain (ties in with reference scripts)

I think getting it from onchain is the best solution for the browser app,
but power users still need a way to get the protocol parameters.
We could just push it to github or host the cli file ourselves.


## Open a vault

Inputs: Protocol, initial ada amount

Outputs: vault uuid

Should do:
- Create a valid vault with the initial ada amount and a valid nft
- return the token name of that nft (which is the vault's uuid)

## Query all vaults

Inputs: Protocol

Outputs: uuids for all valid vaults

Should do:
- lookup vault address
- filter out invalid vaults

## Query User's vaults

Inputs: Protocol, PubKeyHash

Outputs: uuids for all valid vaults with that owner

Should do:
- lookup vault address
- filter out invalid vaults
- filter vaults owned by pubkey

## Query Vault by id
Inputs: Protocol, vault uuid

Outputs: owner, debt, ada, anything else in datum?

## Take out a loan

Inputs: Protocol, vault uuid, amount

Outputs: unit

Should do:
- increase debt by amount
- mint amount dUSD

## Deposit
Inputs: Protocol, vault uuid, amount

Outputs: unit

Should do
- add amount of ada to the vault

## Withdraw
Inputs: Protocol, vault uuid, amount

Outputs: unit

Should do
- remove ada from vault
- (vault must remain above liquidation ratio)

## Pay back debt
Inputs: Protocol, vault uuid, amount

Outputs: unit

Should do
- burn dUSD
- reduce debt appropriately

## Maybe adjust?

In the onchain code it makes sense to treat
take out loan, deposit, withdraw, and payback debt
as one type of transaction. If we do that we
might as well expose that here too as it would
often be cheaper and faster.

## Liquidate

inputs: protocol, vault uuid, ?

outputs: ?

I'm not sure how this should work
it needs to interface with the auction module

## Bid in an auction

I'm not sure how this will work
