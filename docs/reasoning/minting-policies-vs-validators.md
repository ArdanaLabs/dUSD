# Descriptions

## Minting Policies

Protocol versions are defined by entries in the configuration utxo.
Each entry is a list of currency symbols who's minting policies define the allowable protocol actions.
Protocol updates are issued by appending new protocols to the list.
The script validate only needs to control which actions are available for which utxos when (and that some action is always taken):
 - Certain actions are available only in the newest version.
   - Minting dUSD (this can be enforced by the dUSD minting policy)
   - Releasing a new protocol version
   - Opening a vault
 - Vaults can only use actions from their protocol version except migration which must be from the next version.

[This approach is based on this article](https://github.com/Plutonomicon/plutonomicon/blob/main/transaction-token-pattern.md)

## Validators

Protocol versions are defined by script addresses.
The dUSD minting policy must check with a config utxo to know which address corresponds to the newest version and can be used for minting.
The validators must also check the config to know where migrations are valid.
For migrations to be possible for vaults which are more than one version behind the configuration must keep a list of addresses, or each protocol needs a new config utxo.
Vaults need to receive new NFTs to be migrated so that the new protocol can define the migration logic.
Ideally the old vault also knows the currency symbol for new vault NFTs so it can enforce that this switch occurs otherwise a vault could be lost by transferring without the switch.

# Trade offs

### Minting policy Pros

#### Script sizes would likely be smaller

The main validator can be fairly simple and the minting policies spread out the logic so less unused code needs to be loaded in each Tx.

#### Migration is simpler

Since the migration logic can be called (indirectly) by the main script the tracking NFTs don't need to change and the problem of making sure
migration logic was invokes becomes trivial.

#### Simplifies non-trivial vault control

This won't be part of the MVP but the minting policy approach makes it easy to allow users to define vault control policies with arbitrary logic.
This would allow some cool things such as control by NFTs or DAOs.

#### Existing framework

There is already a framework [outlined in the same article linked above](https://github.com/Plutonomicon/plutonomicon/blob/main/transaction-token-pattern.md).
This should be helpful while defining implementation details and should simplify the organization of the on-chain code.

### Minting policy Cons

#### Protocol versions need to be stored in datum

Since the addresses are the same the distinction must be kept in datum.
This slightly increases the size of the datum and requires some main validator logic to enforce.

#### Protocol Risks

[Taken from the same article](https://github.com/Plutonomicon/plutonomicon/blob/main/transaction-token-pattern.md#what-are-the-risks)
We run the following risks:

- Vendor decisions obsoleting the techniques used
- An increased attack surface via invalid assumptions made about the underlying technology
- Incompatibility with legacy and future developments not utilizing the pattern
- Adding auditing, testing, specification, and cognitive ramp-up burden to existing projects that wish to adopt the pattern

In our case the cognitive ramp up cost is pretty low as this decision has minimal impact on the off-chain code and I've already familiarized myself with this framework.
