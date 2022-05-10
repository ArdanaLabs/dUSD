# dUSD on-chain architecture

## Main Script
In order to make every element of the protocol upgradeable we need to allow for mutability of all protocol logic.

This can be achieved by placing all of the logic inside Minting Policy scripts and delegating control to these scripts from a Main Validator.

This means that all UTXOs belonging to the dUSD protocol will reside at a single address.

In order to unlock a UTXO the Main Script will defer to a Minting Policy Lookup Table `MPLT` in a UTXO and dereference the logic required.

## Configuration
We will call the UTXO containing the Minting Policy Lookup Table `MPLT` the `Configuration` UTXO.

This UTXO will be uniquely identifiable by an NFT.

This NFT will be baked in to the `Main Script` which will require it to be present in the input for all protocol transactions.

When making a protocol transaction the redeemer for the main script will act as an index into the `MPLT`.

The Currency Symbol for the Minting Policy at this index must be minted ensuring its logic is executed.

In this manner we delegate control to the Minting Policies referenced by the mutable `MPLT`.

e.g. Making a Vault action may require dereferencing index 2
  - The Minting Policy at index 2 will take a redeemer corresponding to a Vault action and ensure that the action is allowed

## Mutability of the Configuration
Index 0 of the `MPLT` will be reserved for the logic that allows modification of the Configuration.

Initially this will be a minting policy that checks that the transaction is signed by an authorized public key.

The owner of this public key is able to spend the Configuration UTXO and replace it with a new Configuration - upgrading the protocol.

The owner may cede control of this power to a Minting Policy that allows protocol changes to be made by a DAO.

## Read Only UTXOs
Without Read Only UTXO inputs this design can only be single threaded. That is to say each transaction must consume and re-emit the Configuration.

With Read Only UTXO inputs the only transaction that consumes the Configuration is the one that modifies it.

All other protocol actions simple acquire the Configuration as a Read Only Input which allows these actions to be parallelized.

## Price Oracle Module
The price oracle module will reside at the Main Script address in a UTXO that is uniquely identifiable by an NFT.

The logic for price oracle updates will be stored in a minting policy reference in the Configuration MPLT.

Initially this logic will expect that the transaction is signed by a specific public key owned by the bot.

This logic can be replaced by protocol upgrade.


The same caveat applies regarding Read Only Inputs - without them this design would be single threaded for all actions.

## Buffer Module
The Buffer module will be a UTXO uniquely identified by an NFT.

The Buffer actions will be mediated by the logic in a minting policy referenced by the MPLT.

This Minting Policy will have a redeemer that is a Sum Type with a constructor for each Buffer action.

## Vault Actions
Vaults will be locked by a user provided minting policy. This minting policy will be referenced by the Vault UTXO.

This allows Users to set arbitrary locking/unlocking conditions on a vault - this could be pubkey, multisig, script authorized etc...

The Vault actions will be mediated by the logic in a minting policy referenced by the MPLT.

This Minting Policy will have a redeemer that is a Sum Type with a constructor for each Vault action.

