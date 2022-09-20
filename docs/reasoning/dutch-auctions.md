## Redeemer
- Number of slots the vault has been under-collatoralized

## Validator Logic
- We look at the vault, calculate the price where the vault can be liquidated
- Calculate how long the vault has been under-collatoralized from the history
- Using the time, compute the current dutch auction price.
- We allow someone to exchange their dUSD for the store ADA at the calculated price.
- Vault debt is decreased.
- Vault owner is unchanged.
