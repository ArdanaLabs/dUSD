# onchain spec

# Changes

I'm assuming like with hello discovery we are just using one
vault address

# Address Validators

## Vault Address validator

Constraints:
- input must be a valid vault
- all outputs at vault address are valid vaults
- redeemer parsed
- parameter utxo is valid
- price oracle utxo is valid

We can enforce that the oracle and parameter utxos are only referenced for free
so we might as well

If the redeemer action is close:
	- Validation NFT burned
	- Debt paid (Maybe we require that the debt was already 0?)
	- Signed by owner
If the redeemer was adjust:
	I think it makes sense to bundle deposit withdraw etc into one action
	They all have the same constraints so this seems simpler
	- output vault is above liquidation ratio
	- exactly the original vault validation nft is at the output
	- Signed by owner
  - (total?) debt increase/decrease matches minted dUSD
	- debt is not negative
If the redeemer was liquidate:
	- Vault was bellow liquidation ratio
	- Auction triggered appropriately? maybe first purchase included in this?

## Auction Address validator

Performs (dutch?) auctions or maybe flat sales?
vault liquidation and buffer auctions handled here

## Buffer Address

I'm not clear on what the buffer should do.
It says admin can trigger an auction from the buffer,
so it probably needs to support starting auctions.

## Protocol parameter Address validator

Constraints:

- Admin signed tx
Maybe:
- New datum parses?
- Nft remains at address?

We need these to prevent admin from being able to freeze everything

## Price oracle Address

Constraints:
- Only admin can write

Maybe:

We could add some more constraints that would prevent
Admin from suddenly adding 2 fraudulent entries to
liquidate all the vaults without warning

- Must be valid update
	- any expired entries dropped
	- Previous data otherwise not altered
	- One new entry
	- NFT remains
- Must wait between writes
	- write must occur at least slightly less than one hour after the last entry


# Minting Policies

## dUSD minting policy

Constraints:

- parameter input is valid and parses
- (at least or maybe exactly one) Input must be a valid vault (The vault address must come from the parameter utxo)

We don't want much logic here because we would need to change the
currency symbol of dUSD to update it

## Vault validation+tracking minting policy

- Seed tx must be consumed
- Exactly one token is minted
- Token name is the hash of the seed tx
- Output utxo (with the token) must be at the vault address
- Output utxo datum must parse as a vault datum
- Output utxo datum must have a debt of 0
Maybe:
- Output utxo datum must have an owner who signed this tx

I can't see any obvious harm in letting someone open a vault in someone else's name
but there's not much advantage either. I don't think we should allow it

## Generic NFT minting policy

This would be used to make the NFTs used to authenticate
the price oracle and protocol parameters utxos

Constraints:

- Seed tx must be consumed
