# Tezos' (Tezos Prime)

Tezos' is an open source implementation of the concepts described in
the Tezos white paper. In particular, Tezos' is proof-of-stake, supports
metaconsensus for changing the protocol and formally verifiable smart
contracts. Tezos' further extends the Tezos concept by including
formal verification on chain.

## Cryptocurrency Ethos

In addition to the technical extension of on chain formal verification,
Tezos' is committed to the cryptocurrency ethos. Even cryptocurrency
advocates who are skeptical of the Tezos concept should support Tezos'
(at least with moral support) to send the message that the ethical
lapses of the current Tezos leadership are unacceptable in this space.

* open source

All code is open source. As of June 2018, the code for the original
Tezos implementation is under the exclusive copyright of Digital
Ledger Systems, a U.S. based company. The Tezos' code uses none of the
code written by Digital Ledger Systems, but instead uses code forked
from other OCaml open source projects dating back to 2015, more than a
year before the first public release of Tezos code by Digital Ledger
Systems.

* pseudonymously developed

All developers of Tezos' are currently using pseudonyms and this will
be strongly encouraged for new developers. The example of Tezos makes
it clear that once the identities of developers are known, it becomes
inevitable that certain decisions will be made that go against those
without power in deference of those with power.

* can be pseudonymously used

Neither the developers of Tezos' nor the "Tezos Prime Foundation" (to
the degree such a thing can be said to exist) can ask for your
identity to use the network. We are not asking you to trust us. You do
not have to trust us. All fundraising will be done in a way where
contributions can be made on an existing cryptocurrency blockchain
that supports pseudonymous users. Furthermore, in exchange for funds
raised there will be published Merkle roots of intermediate ledger
states (before the network begins) so that everyone has the ability to
pseudonymously prove they are entitled to a certain amount of prime
tezzies.

## Current Status

A number of changes need to be made to the code before launching the network.

* consensus

The current consensus system is mostly a form of proof-of-stake, but
needs to be changed to be closer to the proof-of-stake algorithm
described in the Tezos white paper. There need to be notions of
rolls, delegation and a 1 minute block time target by giving a unique
roll a chance to bake each minute.  There also need to be security
deposits that can be forfeited in case of a fraud proof
(denunciation). Also a commit-and-reveal scheme needs to be added
to generate an essentially random number to determine who will stake
in the future.

* smart contracts

The current code already has support for proof checking, but does not currently
have a Turing complete language for smart contracts. The plan is to
extend the type theory in the current code to encode such a Turing complete
language along with its operational semantics. Instead of executing a contract,
certain transactions will be justified by giving a proof that a contract
would have a certain result if it were to be executed. One advantage of this
approach is that the proof could take advantage of lemmas corresponding to optimizations
that have been formally proven correct.

* meta-consensus (on chain governance)

As described in the Tezos white paper, meta-consensus can be acheived by committing to the
a hash of the code for the new protocol in block headers and having bakers essentially
vote on whether to promote the code to the level of the testnet and eventually mainnet.

## How can you help?

Code reviewers (and people who can write documentation) would be most
helpful at the moment, but we are open to help in other ways.  If you
want to help, contact tezosprime@vistomail.com.  Please send the
email from an account not connected to your identity while logged in
over the Tor network.

## Official secure information source

Tezos' is not affiliated with the Tezos Foundation and Digital Ledger
Systems. Given that Tezos' may face significant resistance from these
well-funded and well-connected entities, accounts on centrally
controlled websites like github or reddit cannot be considered
secure. The most secure (and hence "official") information source for
Tezos' is the memo.cash account associated with the Bitcoin Cash
address 1MW4GTkrcnTRs5TjEAyv9anoZ8qCjH9Bre.

https://memo.cash/profile/1MW4GTkrcnTRs5TjEAyv9anoZ8qCjH9Bre

This memo.cash account should be consulted for the most important and most
up to date information.

                    "Laissez faire les proprietaires"

                              Pierre-Joseph Proudhon
