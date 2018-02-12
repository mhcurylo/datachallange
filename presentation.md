

---
title: DevChallange 9. Workshops.
author: Mateusz Curylo
...

# DevChallange 9 Workshops

---

# Choice                     

Lets make a Data Aggregator in Haskell.                    

Reasons:                     

Docker for free (stack build tool)   

Potentially fast (???)                         

Good libraries (???)

Easy concurrency (???)

I like learning fp

---

# Libraries, very good:

---

## Vector.mutable

Very fast (c fast)

Stream fusion

Mutable Tricky! (V.write on immutable Vecotr is O(n), O(1) on mutable)

---

## Conduit
  
Very fast (faster than ByteString)

Complicated operators ($$, .| etc.) and concurrency for newbies.

---

# Libraries, extremely good:

---

## Servant,

Type-level Web APIs! 

An example of Haskell community at its best.

White paper from 2015 and an industry-strong library.

Let me show two files: 

src/Http/Servant.hs
src/Http/Server.hs

---

## QuickCheck

The property testing library.

It allows us to define properties which should hold true for our code and then
randomly test them.

It is a game-changer to me.

---

## Criterion

Statistically aware performance testing tool.

I have made separate project speedt to learn about performance of different
implementations / data structures.

Statistical awareness allows us to test small and large functions reliably.

---

## OK, so a fail?

Nop
I was to interested to learn about libraries and performance in Haskell to finish
In fact went from 15s per file to 600ms, metadata included.
I stopped at making Conduit concurrent by lack of knowledge on monad transformers. 

---

## Thanks!


