# Tests for dUSD

## Tests of onchain code with offchain code

### Initialize
	- Initializing the protocol doesn't error

### Open Vault
	- opening a vault doesn't error

### Vault lookups
	- lookup shows now vaults after init
	- once a vault is opened it shows up
	- getMyVaults shows your vaults and not vaults opened by another key
	- Invalid vaults don't show up
	- Closing a vault makes it stop showing up

### Vault Deposit
	- doesn't throw error
	- lookup reflects change (ada balance increased)

### Vault Withdraw
	- doesn't throw error
	- lookup reflects change (ada balance decreased)

### Vault loan
	- doesn't throw error
	- lookup reflects change (debt increased)
	- dUSD in wallet after loan

### Vault liquidation
	This will require simulating a price drop
	The liquidation should also be done by
	a wallet other than the vault owner
	- doesn't throw error
	- lookup sees vault as closed

### Attack tests

As these are attack tests of onchain code they should all fail at script evaluation

#### Protocol parameter utxo
	- Can't edit without admin
	Maybe:
	- Admin can't steal nft
	- Admin can't edit if it doesn't parse

#### Price oracle spec
	- Can't edit without admin
	Maybe:
	- can't change old data (except removing old enough entries)
	- can't push (much) faster than once per hour

#### Vault validation nfts
	- Can't mint with no vault
	- Can't steal on:
		- open
		- deposit
		- withdraw
		- close

#### Ownership
	- For a vault you don't own you can't :
		- withdraw
		- close
		- deposit (could be a rescource contention issue)
		- open (is it a problem to open vaults in someone's name?)

