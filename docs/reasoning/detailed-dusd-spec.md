# low level spec for dUSD

The spec doesn't seem detailed enough to say much about testing,
so this is a more detailed version.

# Changes

I'm assuming like with hello discovery we are just using one
vault address

# Onchain Components

### Admin

3 keys?

## Validators

### Vault Address validator

### Auction Address validator

Performs (dutch?) auctions or maybe flat sales?
vault liquidation auctions handled here


### Buffer Address

### Protocol parameter Address validator

Constraints:

- Only allows update only by admin

We could add some constraints so Admin can't brick the protocol

- New datum parses?
- Nft remains at address?

### Price oracle Address

Constraints:
- Only admin can write

Maybe more constraints?

We could add some more constraints that would prevent
Admin from suddenly adding 2 fradulent entries to
liquidate all the vaults without warning

- Must be valid update
	- any expired entries dropped
	- Previous data otherwise not altered
	- One new entry
- Must wait between writes
	- write must occur at least one hour after the last entry


## Minting Policies

### dUSD minting policy

### Vault validation+tracking minting policy

# Offchain components

## Admin

### Initialize protocol

Inputs:
	The list of admin keys?
	The initial protocol parameters

Outputs: Protocol type which includes:
	- The Protocol parameter utxo's nft's currency symbol
	- Protocol addresses which depend on that
		- Vault address
		- auction address ?
		- dUSD minting policy
		- buffer address?
Note the protocol type needs to be easy to broad

Should do:
	Create an NFT
	send that nft with to the protocol parameter address with the initial parameters
	compute parametized scripts for all of the scripts that depend on that NFT (including transitiviely)

### Add price data

Inputs: Protocol, the current price
Outputs: none

Should do:
	update the price data to include the new price
	and drop any old price data

### Set Protocol parameters

Inputs: Protocol, New Parameters
Outputs: unit

## Anyone

### Open a vault

Inputs: Protocol, initial ada amount
Outputs: vault uuid

Should do:
	Create a valid vault with the initial ada amount and a valid nft
	return the token name of that nft (which is the vault's uuid)

### Querry all vaults

Inputs: Protocol
Outputs: uuids for all valid vaults
Should do:
	lookup vault address
	filter out invalid vaults

### Querry User's vaults

Inputs: Protocol, PubKeyHash
Outputs: uuids for all valid vaults with that owner
Should do:
	lookup vault address
	filter out invalid vaults
	filter vaults owned by pubkey

### Querry Vault by id
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

## Bot/non-script Addresses

