
If blocks get undone, it would be nice to cleanly roll back the database and not need to rebuild it all.

So idealy it should be some kind of append-only, immutable database.


Lets remember some stuff for every account by pubkey.
* current balance.
* any sub-accounts owned by the same person.
* the history of txs that involved this account.

Lets remember for every tx by txid
* who was involved
* what block it had occured in.
* details depending on which tx type it was.

lets remember for every block by blockhash
* block height
* the txs in that block

Lets remember some stuff for every sub-account by sub account id.
* current balance.
* associated smart contract or market.
* the owner account.

for every smart contract by contract id.
* total existing shares.
* open/closed
* allow people to submit off-chain data so we can display that too.
  - contract text
  - is it one of the standard contracts? display the info well.


for every market by market id
* how many coins and shares.
* which coins can be traded, links to those smart contracts.