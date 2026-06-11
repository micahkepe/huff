# `huff`

A [Huffman coding](https://en.wikipedia.org/wiki/Huffman_coding) implementation
in Haskell.

## TODOs

- [ ] Initial working naive version
  - [ ] Need to change `String` to `[Bool]` since `[Char]` is 8 bits so bit compression is not as good as it could/should be.
- [ ] Test suite
- [ ] Benchmark on some corpus (speed + compression ratio)
- [ ] [Huet Zipper](https://wiki.haskell.org/index.php?title=Zipper_monad)
      traversal implementation
- [ ] Benchmark again
- [ ] Explore other optimizations
- [ ] Actually nice CLI with [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative-0.19.0.0#quick-start)
- [ ] Prosper?

## Tags

Generate [fast-tags](https://github.com/elaforge/fast-tags) with:

```bash
fast-tags -R (git rev-parse --show-toplevel)
```
