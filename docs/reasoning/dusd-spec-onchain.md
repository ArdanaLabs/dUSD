# Onchain spec

# Changes

I'm assuming like with hello discovery we are just using one
vault address

# Address Validators

Validators which are invoked when a utxo from a certain address (the hash of the validator itself) is spent

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
- Signed by owner

Either:
- The vault has no debt

Or
- Any remaining debt is paid off
- The time stamp is updated to the end of the time window of the tx
- the stability fee on that debt is paid

If we allow debt in the input vault we probably want to re use code in adjust for this
we could do that by putting most of the adjust logic in a function that looks at the input and output
and calling it with an output with 0 debt, 0 ada and probably a time stamp of the end of the time
window so the stability fee includes time right up to closing

If the redeemer was adjust:

I think it makes sense to bundle deposit withdraw etc into one action
They all have the same constraints so this seems simpler

- output vault is above liquidation ratio
- exactly the original vault validation nft is at the output
- Signed by owner
- (total?) debt increase/decrease matches minted dUSD
- debt is not negative
- the time stamp of the debt is updated
	- the time window of the tx is not too large (maybe an hour or a day)
	- the time stamp is set to a time within that window
	- the new time zone is after the last
		(this is probably redundant as even a naive implementation
		would just require negative tokens to be sent to the buffer
		which the node will reject anyway)
- the stability fee is paid to the buffer address
	- we could allow the stability fee to just increase the debt
	  but this is equivelent to just minting dUSD to pay it as part of the same transaction

If the redeemer was liquidate:
- Vault was bellow liquidation ratio
	- this may need to include debt that would be induced by borrowing dUSD to pay stability fees
		- if this is allowed those fees should be paid by minted dUSD increaseing the debt
		- this update also needs to use the time at the
			begining of the time range as the liquidator is incentivised
			to update into the future if that would push the vault over
			and the time range should be pretty small
		- this part could probably also reuse the core
		  adjust logic
- Auction triggered appropriately? maybe first purchase included in this?

Stability fee calculation presumably includes some amount of rounding.
For the onchain code we probably want to enforce a maximum rounding error
and a maximum precision.

## Auction Address validator

Performs (dutch?) auctions or maybe flat sales?
vault liquidation and buffer auctions handled here

## Buffer Address

I'm not clear on what the buffer should do.
It says admin can trigger an auction from the buffer,
so it probably needs to support starting auctions.
Post MVP this needs to support payment to DANA owners.
The buffer may need to support protocol migration.
Maybe the buffer just checks for an admin signature as allow migration
is basically just letting admin control it anyway?

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

Maybe:
- Exactly one token is minted

Arguably it's simpler to trust the correctness of the protocol if we add this constraint
as it wouldn't depend on the initialization being done correctly,
but if the initialization is done correctly that will be observable to anyone
by looking at the block chain so it shouldn't really matter.
I think it's better not to enforce this.
