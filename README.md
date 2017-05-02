# Pardis
This repository contains the full source code for the paper
*Pardis: A Process Calculus for Parallel and Distributed Programming in Haskell*
by Christopher Blöcker and Ulrich Hoffmann.

## Summary
**Pardis** defines a set of process combinators and is implemented as a domain
specific language in Haskell. It has the purpose to reduce boilerplate code and
thereby sources of bugs in parallel and distributed programming. This is done by
abstracting from the concrete notion of parallelism and encapsulating it in the
process combinators. Effectively, Pardis removes manual process management and
synchronisation from parallel and distributed programming and allows to
interchange the underlying implementation of parallelism without changing
application code.

## Content
The repository contains the source code for **Pardis** and a web crawler as an
application example.

### Pardis
The source code for **Pardis** is located in the `src` folder. It contains the
definition of **Pardis** itself under `src/Pardis.hs`, the parallel implementation
using the `IO` monad in `src/Pardis/Parallel.hs`, and the distributed implementation
using *Cloud Haskell* and its `Process` monad in `src/Pardis/Distributed.hs`.
Furthermore, there is some auxiliary code written in *Template Haskell* in
`src/Pardis/Distributed/TH.hs`.

### Application Example: Web Crawler
The code for the web crawler is located in the `app` folder. The web crawler
comes in two versions, i.e., as parallel program and as a distributed program.
Code used by both versions is in `app/Crawler.hs`.

Up to date the parallel and distributed versions are slightly different due to
different requirements of the backends they use. Whereas we can use polymorh
functions in the parallel version to create processes, we have to turn them into
monomorph ones in the distributed version before we can use them in processes.
This is because *Cloud Haskell* can not guess the concrete return type of
functions, however, for serialisation of results, the specific type must be known.
We hope that in a future version of *Cloud Haskell* it will be possible to create
closures from polymorh functions. 

#### Parallel Crawler
The parallel version of the web crawler is in `app/Parallel.hs`. It uses the
parallel implementation of **Pardis** built on top of the `IO` monad.

#### Distributed Crawler
The distributed version of the web crawler is in `app/Distributed.hs`. It uses
the distributed implementation of **Pardis** built on top of *Cloud Haskell*'s
`Process` monad.

## Building
The source code can be compiled using the Haskell build tool
[stack](https://www.haskellstack.org/) with `stack build`.
This setup will use ghc 8.0.2 and Stackage version
[lts-8.12](https://www.stackage.org/lts-8.12).
