# CIPS

This document gives a brief overview of the CIPs in the babage/vasil forks and the extent to which we plan to use them.

## Inline Datum

This cip allows datums to be stored directly in utxos rather than storing their hashes.

### Pros

- Simpler validator code
- Smaller transactions

### Cons

- If you have several identical datum in the same transaction which are much larger than hashes it could reduce total tx size at the cost of some script complexity to use hashes instead.
	This should be rare or imposible in dUSD. Liqlidating large numbers of identical vaults might be slightly slower.

### Options:

#### Require
This almost certainly has the best performance.
I think we should do this.

#### Allow
This has the most script complexity but retains the most of the rest of the transaction size reduction.
I don't think this makes sense to do this.

### Disallow
This has slightly more script complexity and almost always worse performance.
I don't think this makes sense.

## Reference Inputs

This allows inputs to be examined without spending them.

### Pros

- Oracles can have reasonable performance
- Reading global configurations can have better performance

### Options

#### Require

This will have the best performance. We should definitely do this.

### Allow

This would allow malicious users to cause resource contention issues.

### Disallow

This would force all users to cause resource contention issues.

## Reference Scripts

### Pros

- Huge bandwidth improvement

### Options

#### Require

The validators would be a bit more complex than if we make this optional.

#### Allow

The validators would probably be a bit simpler this way.
This would enable users to waste Ada on fees, but I don't think there's any actual harm in this.
I think we should do this.

#### Disallow

This would likely endanger the viability of the protocol.
