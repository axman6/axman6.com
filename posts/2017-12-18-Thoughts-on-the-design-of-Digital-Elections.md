---
title: Thoughts on the design of Digital Elections snippet: author: Alex Mason
draft: true ident: 07a19e56-54c0-4c77-b824-36e68cf600be
---

I have been thinking for quite some time about how a digital election system
should work and the properties that one should hold. Now seemed like as good a
time as any to write down these ideas and to see if others can fill in the gaps.
A lot of the design I've been considering revolves around cryptography, for
building both the identification infrastructure and for the ability to
independently verify the results. If the properties below can be built with high
confidence, then we could come to expect the same level of integrity in our
elections that we expect from our ~~banks~~ ...

I have been working on the assumption that the properties we expect from modern
cryptography are similar to those we'd expect from a voting system, though there
are further properties we'd also need to insure:

 1. The design must be open to all and open to scrutiny
 1. privacy should be guaranteed, only in the event that someone's private key
    is compromised should it be possible to tell how someone voted (I can't
    think of any way around this one, but it might be possible to provide
    electors with private keys they have no access to)
 1. it should be possible for any elector to verify that their vote was
    submitted as they intended
 1. it should be possible for any elector to verify that their vote was counted,
    and was counted how they expected
 1. it should be possible for any elector to verify that the number of votes is
    equal to the number of votes submitted

I'm a fan of the idea of [Liquid
Democracy](https://en.wikipedia.org/wiki/Delegative_democracy), so some other
properties I would like to see may also be built on top of this base, but that's
probably for another post. The short version of what it means (at least to me)
is all citizens can vote on all subjects, but may delegate this power at any
time to another person for a particular subject, and may revoke that delegation
at any time, creating a large incentive for people who choose to be
representitives to vote the way they have promised to.

## Open Protocols

Any advocate of cryptography will tell you that you shouldn't trust any crypto
which is protected by the fact its implementation is secret. Good crypto must
stand up to the harsh light of public disclosure and scrutiny by experts. The
same should be true for electronic voting; the protocol must be robust enough
that any person can build their own client for casting a vote, and even be able
to submit a valid vote created using their own implementation, without any fear
that the particular implementation is up to no good. The same is true of the
implementation of the server, anyone should be able to take the publicly
released information for the authority running the election, and be able to
reproduce the the result exactly.




 - the implementation should not actually matter, anyone with sufficient skill
   should be able to build a client or server alternative and come to the same
   exact result as the official result