# Low level offchain api spec

This is a spec for the low level api that will be used in the cli and browser components

# Admin only API

This part of the api will require the Admin wallet available using yubihsm

## Initialize protocol

Inputs:
	The list of admin keys?
	The initial protocol parameters

Outputs: Protocol type which includes:
	- The Protocol parameter utxo's nft's currency symbol
	- Protocol addresses which depend on that
		- Vault address
		- dUSD minting policy
		- auction address ?
		- buffer address?
Note the protocol type needs to be easy to broad

Should do:
	Create an NFT
	send that nft with to the protocol parameter address with the initial parameters
	compute parametized scripts for all of the scripts that depend on that NFT (including transitiviely)

## Add price data

Inputs: Protocol, the current price
Outputs: none

Should do:
	update the price data to include the new price
	and drop any old price data

## Set Protocol parameters

Inputs: Protocol, New Parameters
Outputs: unit

## Trigger buffer auction

I'm not sure how this should work

# Anyone

This part of the api should work with any wallet

## Get protocol

I'm not sure what the best way to get the protocol data to users would be.
For admin/the cli it can just read it from a file which the cli can create when
the protocol is initialized.
The browser tests on the other hand I'm not sure what's best:
- Hard codding
	makes the most sense in production where the value is determined once and never changes
	forces lots of recompiling during tests
- Environment variable
	easy for nix testing but not clear how that would work in production
- Serve it as a file
	this might be good?
	I don't know enough web development and should ask Evan about this.

## Open a vault

Inputs: Protocol, initial ada amount
Outputs: vault uuid

Should do:
	Create a valid vault with the initial ada amount and a valid nft
	return the token name of that nft (which is the vault's uuid)

## Query all vaults

Inputs: Protocol
Outputs: uuids for all valid vaults
Should do:
	lookup vault address
	filter out invalid vaults

## Query User's vaults

Inputs: Protocol, PubKeyHash
Outputs: uuids for all valid vaults with that owner
Should do:
	lookup vault address
	filter out invalid vaults
	filter vaults owned by pubkey

## Query Vault by id
Inputs: Protocol, vault uuid
Outputs: owner, debt, ada, anything else in datum?

## Take out a loan

Inputs: Protocol, vault uuid, amount
Outputs: unit

Should do:
	increase debt by amount
	mint amount dUSD

## Deposit
Inputs: Protocol, vault uuid, amount
Outputs: unit

Should do
	add amount of ada to the vault

## Withdraw
Inputs: Protocol, vault uuid, amount
Outputs: unit

Should do
	remove ada from vault
	(vault must remain above liquidation ratio)

## Pay back debt
Inputs: Protocol, vault uuid, amount
Outputs: unit

Should do
	burn dUSD
	and reduce debt apropriately

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
