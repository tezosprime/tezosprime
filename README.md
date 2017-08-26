# dalilcoin
A Formal Library as a Bitcoin Spin-Off

https://www.reddit.com/r/dalilcoin/

Dalilcoin is a fork of Qeditas (qeditas.org) which was designed
to use a blockchain to support a revival of the QED Project.

https://www.cs.ru.nl/~freek/qed/qed.html

Dalilcoin changes the consensus protocol of Qeditas by using Proof of
Burn to anchor Dalilcoin blocks into a Proof of Work blockchain
(Litecoin's).  In addition, the target block time has been slowed
considerably so that Dalilcoin is only expected to average 4 blocks
per day.  Most of the code is due to the Qeditas developers and the
(open source) copyright notices reflect this fact (and must continue
to do so).  Additional copyright notices for Dalilcoin developers have
been added to files changed after the Dalilcoin fork was created in
August 2017.

Dalilcoin is still under development.

* Installation

./configure
make

Instead of make, it is also possible to use the makebytecode script:

./configure
./makebytecode

* Running

./bin/dalilcoin -testnet

Without the initial ledger tree, there is little sensible to do except exit.

> exit

* Obtaining the Initial Ledger Tree

The initial ledger tree contains the initial distribution
of Dalilcoin assets (at least for the testnet) and has hash root
d4b10e4b72eaa8a61427b805f206e828b22bb59192373b83fe0df501e68a5bed.

The full tree is available as the file initdbfixed256.tgz
(900MB) at:
https://mega.nz/#!JGx1VSwY!04XmXU0Zv0iye4Sa2LhSvp7ERh_HiJAKn_2IsQsYkN4

The sha256 hash of this file is
9fc0e0ec040f9dcbc597e8951ea3cd80959805579a9b691faefcf2a107d71758.

After downloading the file cd to the testnet subdirectory of your Dalilcoin data directory.
Most likely this mean:

cd ~/.dalilcoin/testnet

Move the downloaded file to here and untar it. For example:

tar xzvf initdbfixed256.tgz

It will create a db subdirectory with all the necessary information.

* Importing Watch Addresses and Viewing Balances

Here is an example of how to view assets from the initial distribution.
This assumes you have a version of the initial ledger tree (see above).

We first import 3 p2pkh addresses and one p2sh address. We give them
as Bitcoin addresses and Dalilcoin prints the corresponding Dalilcoin address.
Dalilcoin p2pkh addresses start with D. Dalilcoin p2sh addresses start with d.
Testnet p2pkh addresses start with tD and testnet p2sh addresses start with td.

Here is an example:

./bin/dalilcoin -testnet

> importwatchbtcaddr 14M2d3UDXkkTL8AJmTUifUmAEwYo67cW2Q
Importing as Dalilcoin address tDRMjAdpzoVSLTmPoSUHQM42gNjUB9UrYTV
> importwatchbtcaddr 1LvNDhCXmiWwQ3yeukjMLZYgW7HT9wCMru
Importing as Dalilcoin address tDhw4mHZK3TCpXhD9amY328pCduCqHe6gBT
> importwatchbtcaddr 15muB9t6z5UZBCWTkTApgEUYnMZdcnumKo
Importing as Dalilcoin address tDSnbikEtFpASJqjxRTyWMok4v9V1pWGuSK
> importwatchbtcaddr 37GxXLE4tiFEKwsLzNGFdHyjVfUEbj6wt2
Importing as Dalilcoin address tdbsqDtyhDNRJSRmc1TPTymDXFCZVquTpuy

We next ask Dalilcoin to print the assets. The initial ledger root
d4b10e4b72eaa8a61427b805f206e828b22bb59192373b83fe0df501e68a5bed
is the default ledger root for now. In the future, the default 
ledger root will be the ledger root of the best block.

> printassets
Assets in ledger with root d4b10e4b72eaa8a61427b805f206e828b22bb59192373b83fe0df501e68a5bed:
Controlled p2pkh assets:
Possibly controlled p2sh assets:
Assets via endorsement:
Watched assets:
tDRMjAdpzoVSLTmPoSUHQM42gNjUB9UrYTV:
737b615b1608bb483754b4dddc8e1aee692d89d51bd86491cd4c1d69b9bcd2f4: 0000000000000000000000000d5edd430a4ffe63fa96dd5c189989bd39b628cb [0] Currency 0.1 fraenks (10000000000 cants)
tDhw4mHZK3TCpXhD9amY328pCduCqHe6gBT:
5f6eea1d442bdc4683aab077382b3aeff14247117dc383dc4b3f07dc721f1081: 00000000000000000000000037cfcd67a77ded709ff0b03c1d80ed5fbed8b33f [0] Currency 0.0015 fraenks (150000000 cants)
tDSnbikEtFpASJqjxRTyWMok4v9V1pWGuSK:
5f3d6ade600b330ba405413935f32cb07dfe904ee52c894c015b8cb27ff702c4: 0000000000000000000000003b522a6135a10dff029666431e145aa4a2d0e824 [0] Currency 0.0015 fraenks (150000000 cants)
tdbsqDtyhDNRJSRmc1TPTymDXFCZVquTpuy:
0a50e9aaae7517139d3c5c1453adae6499c70218b4ef4695579a953a38753eba: 000000000000000000000000ab6f7c6f2d94d3f7a36f39c64b46f4f6d5b492d0 [0] Currency 0.0015 fraenks (150000000 cants)
Total p2pkh: 0.0000000000 fraenks
Total p2sh: 0.0000000000 fraenks
Total via endorsement: 0.0000000000 fraenks
Total watched: 0.1045 fraenks

For each of the imported addresses, there is a currency asset.

By Bitcoin Block 350,000

14M2d3UDXkkTL8AJmTUifUmAEwYo67cW2Q had a balance of 0.1 bitcoins (10000000 satoshis)

Hence the corresponding Dalilcoin testnet address tDRMjAdpzoVSLTmPoSUHQM42gNjUB9UrYTV
has 0.1 fraenks (10000000000 cants). The number of cants is 1000 times more than the number of satoshis
since Dalilcoin has three extra digits of precision.

Similarly, by Bitcoin Block 350,000 the addresses
1LvNDhCXmiWwQ3yeukjMLZYgW7HT9wCMru, 15muB9t6z5UZBCWTkTApgEUYnMZdcnumKo
and 37GxXLE4tiFEKwsLzNGFdHyjVfUEbj6wt2 had balances of 0.0015 bitcoins
(150000 satoshis).  Consequently, the corresponding Dalilcoin testnet addresseses
have 0.0015 fraenks (150000000 cants) each.

If the node does not have the full ledger tree, Dalilcoin may warn you that some data is missing as follows:

Warning: The complete ledger is not in the local database and there are no connections to request missing data.

At the moment Dalilcoin can only be run with the -testnet option.
Overriding this (by commenting out the check in the code and explicitly giving a dummy 32 byte "seed")
for the sake of demonstrating the examples above with mainnet addresses, we have the
following (assuming the ledger db director is in .dalilcoin instead of or in addition to .dalilcoin/testnet):

./bin/dalilcoin -seed=0000000000000000000000000000000000000000000000000000000000000010

> importwatchbtcaddr 14M2d3UDXkkTL8AJmTUifUmAEwYo67cW2Q
Importing as Dalilcoin address DXpj9Qi9YM7cgZUzXTobhNCYkaY346HF8q
> importwatchbtcaddr 1LvNDhCXmiWwQ3yeukjMLZYgW7HT9wCMru
Importing as Dalilcoin address DpQ4k4STnJt6kVJLfm4ENSz51kGh8PsQVg
> importwatchbtcaddr 15muB9t6z5UZBCWTkTApgEUYnMZdcnumKo
Importing as Dalilcoin address DZFbhX82zfqiXdq9WTVhi7uwHzYsjEUGgy
> importwatchbtcaddr 37GxXLE4tiFEKwsLzNGFdHyjVfUEbj6wt2
Importing as Dalilcoin address dK1EDZZZF3dhqnii52aLqx7bzYNR3hFA5R

> printassets
Assets in ledger with root d4b10e4b72eaa8a61427b805f206e828b22bb59192373b83fe0df501e68a5bed:
Controlled p2pkh assets:
Possibly controlled p2sh assets:
Assets via endorsement:
Watched assets:
dK1EDZZZF3dhqnii52aLqx7bzYNR3hFA5R:
0a50e9aaae7517139d3c5c1453adae6499c70218b4ef4695579a953a38753eba: 000000000000000000000000ab6f7c6f2d94d3f7a36f39c64b46f4f6d5b492d0 [0] Currency 0.0015 fraenks (150000000 cants)
DZFbhX82zfqiXdq9WTVhi7uwHzYsjEUGgy:
5f3d6ade600b330ba405413935f32cb07dfe904ee52c894c015b8cb27ff702c4: 0000000000000000000000003b522a6135a10dff029666431e145aa4a2d0e824 [0] Currency 0.0015 fraenks (150000000 cants)
DpQ4k4STnJt6kVJLfm4ENSz51kGh8PsQVg:
5f6eea1d442bdc4683aab077382b3aeff14247117dc383dc4b3f07dc721f1081: 00000000000000000000000037cfcd67a77ded709ff0b03c1d80ed5fbed8b33f [0] Currency 0.0015 fraenks (150000000 cants)
DXpj9Qi9YM7cgZUzXTobhNCYkaY346HF8q:
737b615b1608bb483754b4dddc8e1aee692d89d51bd86491cd4c1d69b9bcd2f4: 0000000000000000000000000d5edd430a4ffe63fa96dd5c189989bd39b628cb [0] Currency 0.1 fraenks (10000000000 cants)
Total p2pkh: 0.0000000000 fraenks
Total p2sh: 0.0000000000 fraenks
Total via endorsement: 0.0000000000 fraenks
Total watched: 0.1045 fraenks
