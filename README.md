# `huff`

A [Huffman coding](https://en.wikipedia.org/wiki/Huffman_coding) implementation
in Haskell.

## TODOs

- [x] Initial working naive version
  - [x] Need to change `String` to `[Bool]` since `[Char]` is 8 bits so bit
        compression is not as good as it could/should be.
  - [x] Swap out `String` input for `Data.Text`
- [x] Serialize the `HuffTree` in the compressed output so that the decoder does
      not need the original tree to recover the source.
- [ ] Remove `byteToBits` and `unpackBits` -> directly index into the encoded
      bytestring
- [ ] Test suite (`quickcheck` time baby, fuzz over ASCII string domain with
      some max size limit)
- [ ] Benchmark on some corpus (speed + compression ratio)
- [ ] [Huet Zipper](https://wiki.haskell.org/index.php?title=Zipper_monad)
      traversal implementation
- [ ] Benchmark again
- [ ] Explore other optimizations
- [ ] Actually nice CLI with
      [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative-0.19.0.0#quick-start)
- [ ] Prosper?
