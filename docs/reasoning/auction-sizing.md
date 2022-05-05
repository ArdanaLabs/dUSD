# Auction Sizing Algorithm

The auction sizing algorithm is designed to respond to events in peg deviation and adjusts the circulating supply of dUSD through minting and burning to drive it's value back to peg. The mechanism can be called at any point in time but will only execute market actions if the price is deviating from the peg.

## Capital Requirement

To enable the stabilization mechanism to perform below-peg price correction, enough capital needs to be available to buy excess dUSD from the market in exchange for ADA and burned. This capital can be accumulated through exchanging the dUSD collected from vault stability fees for ADA and/or from dedicated protocol reserves.

## Parameters

Parameters can be adjusted to tune the effect.

- Lower peg bound, as in the minimum we allow the price to deviate.
- Upper peg bound, as in the maximum we allow the price to deviate.
- Deviation time threshold, as in max time the price can deviate from the peg for peg stabilization to occur. 
- Stabilization Interval, as in the length of time between market actions.

## Temporal Inputs

Temporal inputs are sourced from external feeds such as exchanges and indexes.

- Price of dUSD in USD
- Price of ADA in USD
- Circulating supply of dUSD

## Sizing Calculation

We calculate this quantity of stablecoin to burn/mint with the following equation:

```
quantity = supply * (price - 1)
```

## Algorithm

1. Close all open orders (buying and/or selling dUSD from past runs) 
2. Check if the current price is within peg bounds. If so, exit. If not, continue.
3. After calculating supply adjustment quantity
    - If quantity is positive, initiate mint and sell order of dUSD of that quantity.
    - if quantity is negative, initiate a buy and burn order of dUSD of that quantity.
