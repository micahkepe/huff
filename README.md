# `huff`

A [Huffman coding](https://en.wikipedia.org/wiki/Huffman_coding) implementation
in Haskell.

> [!NOTE]
> Non-streaming (yet) as the symbol frequencies over the entire document must be
> known for the Huffman tree construction.

## TODOs

- [x] Initial working naive version
  - [x] Need to change `String` to `[Bool]` since `[Char]` is 8 bits so bit
        compression is not as good as it could/should be.
  - [x] Swap out `String` input for `Data.Text`
- [x] Serialize the `HuffTree` in the compressed output so that the decoder does
      not need the original tree to recover the source.
- [ ] Remove `byteToBits` and `unpackBits` -> directly index into the encoded
      bytestring
- [ ] Actually nice CLI with
      [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative-0.19.0.0#quick-start)
- [ ] Test suite (`quickcheck` time baby, fuzz over ASCII string domain with
      some max size limit)
- [ ] Benchmark on some corpus (speed + compression ratio)
- [ ] Experiment with [Huet Zipper](https://wiki.haskell.org/index.php?title=Zipper_monad)
      traversal implementation for tree traversal &rarr; _would need to be
      scraped if move to streaming algorithm since Huet zippers are only for
      cursors over an **immutable** tree structure_
- [ ] Benchmark again
- [ ] Explore other optimizations
  - [ ] Look into [adaptive/dynamic Huffman coding](https://en.wikipedia.org/wiki/Adaptive_Huffman_coding)
        _ [Faller-Gallager-Knuth](https://www.ittc.ku.edu/~jsv/Papers/Vit87.jacmACMversion.pdf)
        _ [Vitter](https://www.ittc.ku.edu/~jsv/Papers/HoV94.arithmetic_coding.pdf)
- [ ] Prosper?
