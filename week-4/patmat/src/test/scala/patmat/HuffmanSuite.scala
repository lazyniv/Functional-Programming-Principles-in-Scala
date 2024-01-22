package patmat

class HuffmanSuite extends munit.FunSuite:
  import Huffman.*

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }


  test("weight of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(weight(t1), 5)
  }


  test("chars of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(chars(t2), List('a','b','d'))
  }

  test("string2chars hello world") {
    assertEquals(string2Chars("hello, world"), List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("make ordered leaf list for some frequency table (15pts)") {
    assertEquals(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))), List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list (15pts)") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(combine(leaflist), List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }


  test("decode and encode a very short text should be identity (10pts)") {
    new TestTrees:
      assertEquals(decode(t2, encode(t2)("abbdddababdda".toList)), "abbdddababdda".toList)
  }

  test("decoding the secret") {
    assertEquals(decodedSecret, "huffmanestcool".toList)
  }

  test("encoding a single symbol using french code") {
    val symbol = "h".toList
    val expectedCode = "0011101".toList.map(d => d.asDigit)
    assertEquals(encode(frenchCode)(symbol), expectedCode)
  }

  test("decoding a single symbol using french code") {
    val code = "0011101".toList.map(d => d.asDigit)
    val expectedSymbol = "h".toList
    assertEquals(decode(frenchCode, code), expectedSymbol)
  }

  test("decoding two symbols using french code") {
    val code = "00111010111".toList.map(d => d.asDigit)
    val expectedSymbols = "hu".toList
    assertEquals(decode(frenchCode, code), expectedSymbols)
  }

  test("Quick encoding of one symbol gives current sequence") {
    val toEncode = "h".toList
    val expectedCode = "0011101".toList.map(d => d.asDigit)
    assertEquals(quickEncode(frenchCode)(toEncode), expectedCode)
  }

  test("Quick encoding of two symbols gives current sequence") {
    val toEncode = "hu".toList
    val expectedCode = "00111010111".toList.map(d => d.asDigit)
    assertEquals(quickEncode(frenchCode)(toEncode), expectedCode)
  }

  test("Quick encoding gives current sequence") {
    val toEncode = "huffmanestcool".toList
    assertEquals(quickEncode(frenchCode)(toEncode), secret)
  }


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
