package kuplrg

import Implementation.*

class Spec extends SpecBase {

  // The playground for tests
  def afterTest: Unit = {

    // You can dump any PDA
    pda1.dump

    // You can dump any CFG
    cfg1.dump
  }

  // ---------------------------------------------------------------------------
  // Tests for `pdafs2es` and `pdaes2fs`
  // ---------------------------------------------------------------------------
  lazy val pda1es: PDA = pdafs2es(pda1)
  test(mustEqualLang(pda1es.langByEmptyStacks, pda1.langByFinalStates))
  lazy val pda1fs: PDA = pdaes2fs(pda1es)
  test(mustEqualLang(pda1fs.langByEmptyStacks, pda1.langByFinalStates))
  // ---------------------------------------------------------------------------
  lazy val pda2es: PDA = pdafs2es(pda2)
  test(mustEqualLang(pda2es.langByEmptyStacks, pda2.langByFinalStates))
  lazy val pda2fs: PDA = pdaes2fs(pda2es)
  test(mustEqualLang(pda2fs.langByEmptyStacks, pda2.langByFinalStates))
  // ---------------------------------------------------------------------------
  lazy val pda3es: PDA = pdafs2es(pda3)
  test(mustEqualLang(pda3es.langByEmptyStacks, pda3.langByFinalStates))
  lazy val pda3fs: PDA = pdaes2fs(pda3es)
  test(mustEqualLang(pda3fs.langByEmptyStacks, pda3.langByFinalStates))
  // ---------------------------------------------------------------------------
  lazy val pda4es: PDA = pdafs2es(pda4)
  test(mustEqualLang(pda4es.langByEmptyStacks, pda4.langByFinalStates))
  lazy val pda4fs: PDA = pdaes2fs(pda4es)
  test(mustEqualLang(pda4fs.langByEmptyStacks, pda4.langByFinalStates))
  // ---------------------------------------------------------------------------
  lazy val pda5es: PDA = pdafs2es(pda5)
  test(mustEqualLang(pda5es.langByEmptyStacks, pda5.langByFinalStates))
  lazy val pda5fs: PDA = pdaes2fs(pda5es)
  test(mustEqualLang(pda5fs.langByEmptyStacks, pda5.langByFinalStates))
  // ---------------------------------------------------------------------------
  lazy val pda6es: PDA = pdafs2es(pda6)
  test(mustEqualLang(pda6es.langByEmptyStacks, pda6.langByFinalStates))
  lazy val pda6fs: PDA = pdaes2fs(pda6es)
  test(mustEqualLang(pda6fs.langByEmptyStacks, pda6.langByFinalStates))
  // ---------------------------------------------------------------------------
  lazy val pda7es: PDA = pdafs2es(pda7)
  test(mustEqualLang(pda7es.langByEmptyStacks, pda7.langByFinalStates))
  lazy val pda7fs: PDA = pdaes2fs(pda7es)
  test(mustEqualLang(pda7fs.langByEmptyStacks, pda7.langByFinalStates))
  // ---------------------------------------------------------------------------
  lazy val pda8es: PDA = pdafs2es(pda8)
  test(mustEqualLang(pda8es.langByEmptyStacks, pda8.langByFinalStates))
  lazy val pda8fs: PDA = pdaes2fs(pda8es)
  test(mustEqualLang(pda8fs.langByEmptyStacks, pda8.langByFinalStates))
  // ---------------------------------------------------------------------------
  lazy val pda9es: PDA = pdafs2es(pda9)
  test(mustEqualLang(pda9es.langByEmptyStacks, pda9.langByFinalStates))
  lazy val pda9fs: PDA = pdaes2fs(pda9es)
  test(mustEqualLang(pda9fs.langByEmptyStacks, pda9.langByFinalStates))
  // ---------------------------------------------------------------------------
  lazy val pda10es: PDA = pdafs2es(pda10)
  test(mustEqualLang(pda10es.langByEmptyStacks, pda10.langByFinalStates))
  lazy val pda10fs: PDA = pdaes2fs(pda10es)
  test(mustEqualLang(pda10fs.langByEmptyStacks, pda10.langByFinalStates))

  // ---------------------------------------------------------------------------
  // Tests for `cfg2pdaes`
  // ---------------------------------------------------------------------------
  val cfg1 = CFG("'S -> a 'S | <e> ;;")
  test(mustEqualLang(cfg2pdaes(cfg1).langByEmptyStacks, lang_an))
  val cfg2 = CFG("'S -> a 'S b | <e> ;;")
  test(mustEqualLang(cfg2pdaes(cfg2).langByEmptyStacks, lang_an_bn))
  val cfg3 = CFG("'S -> a 'S b b | <e> ;;")
  test(mustEqualLang(cfg2pdaes(cfg3).langByEmptyStacks, lang_an_b2n))
  val cfg4 = CFG("'S -> a 'S a | b 'S b | <e> ;;")
  test(mustEqualLang(cfg2pdaes(cfg4).langByEmptyStacks, lang_w_wR))
  val cfg5 = CFG("'S -> a 'S b 'S | b 'S a 'S | <e> ;;")
  test(mustEqualLang(cfg2pdaes(cfg5).langByEmptyStacks, lang_na_eq_nb))
  val cfg6 = CFG("'S -> <e> | a 'S b 'S | c 'S b 'S | b 'S a 'S | b 'S c 'S ;;")
  test(mustEqualLang(cfg2pdaes(cfg6).langByEmptyStacks, lang_na_eq_nb_minus_nc))
  val cfg7 = CFG("'S -> aa 'S bb | ab ;;")
  test(mustEqualLang(cfg2pdaes(cfg7).langByEmptyStacks, lang_a2n1_b2n1))
  val cfg8 = CFG("'S -> aa 'S bb | <e> ;;")
  test(mustEqualLang(cfg2pdaes(cfg8).langByEmptyStacks, lang_a2n_b2n))
  val cfg9 = CFG("'S -> b 'S bb | 'A ;; 'A -> a 'A | <e> ;;")
  test(mustEqualLang(cfg2pdaes(cfg9).langByEmptyStacks, lang_bn_am_b2n))
  val cfg10 = CFG("'S -> 'A 'S | <e> ;; 'A -> ( 'S ) | { 'S } | <e> ;;")
  test(mustEqualLang(cfg2pdaes(cfg10).langByEmptyStacks, lang_balanced))

  /* Write your own tests */

  // ---------------------------------------------------------------------------
  // Test Data
  // ---------------------------------------------------------------------------
  // pre-defined alphabets
  val X = 2
  val Y = 1
  val Z = 0

  // PDA list
  val pda1: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(2),
    (0, Some('a'), Z) -> (0, List(X, Z)),
    (0, Some('a'), X) -> (0, List(X, X)),
    (0, None, Z) -> (1, List(Z)),
    (0, None, X) -> (1, List(X)),
    (1, Some('b'), X) -> (1, List()),
    (1, None, Z) -> (2, List(Z)),
  )
  val pda2: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(2),
    (0, Some('a'), Z) -> (0, List(X, X, Z)),
    (0, Some('a'), X) -> (0, List(X, X, X)),
    (0, None, Z) -> (1, List(Z)),
    (0, None, X) -> (1, List(X)),
    (1, Some('b'), X) -> (1, List()),
    (1, None, Z) -> (2, List(Z)),
  )
  val pda3: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(2),
    (0, Some('a'), Z) -> (0, List(X, Z)),
    (0, Some('a'), X) -> (0, List(X, X)),
    (0, Some('a'), Y) -> (0, List(X, Y)),
    (0, Some('b'), Z) -> (0, List(Y, Z)),
    (0, Some('b'), X) -> (0, List(Y, X)),
    (0, Some('b'), Y) -> (0, List(Y, Y)),
    (0, None, Z) -> (1, List(Z)),
    (0, None, X) -> (1, List(X)),
    (0, None, Y) -> (1, List(Y)),
    (1, Some('a'), X) -> (1, List()),
    (1, Some('b'), Y) -> (1, List()),
    (1, None, Z) -> (2, List(Z)),
  )
  val pda4: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(1),
    (0, Some('a'), Z) -> (0, List(X, Z)),
    (0, Some('a'), X) -> (0, List(X, X)),
    (0, Some('a'), Y) -> (0, List()),
    (0, Some('b'), Z) -> (0, List(Y, Z)),
    (0, Some('b'), X) -> (0, List()),
    (0, Some('b'), Y) -> (0, List(Y, Y)),
    (0, None, Z) -> (1, List()),
  )
  val pda5: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(1),
    (0, Some('a'), Z) -> (0, List(X, Z)),
    (0, Some('a'), X) -> (0, List(X, X)),
    (0, Some('a'), Y) -> (0, List()),
    (0, Some('b'), Z) -> (0, List(Y, Z)),
    (0, Some('b'), X) -> (0, List()),
    (0, Some('b'), Y) -> (0, List(Y, Y)),
    (0, None, X) -> (1, List()),
    (0, None, Y) -> (1, List()),
  )
  val pda6: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(6),
    (0, Some('a'), Z) -> (0, List(X, Z)),
    (0, Some('a'), X) -> (0, List(X, X)),
    (0, Some('b'), Z) -> (0, List(X, Z)),
    (0, Some('b'), X) -> (0, List(X, X)),
    (0, Some('a'), Z) -> (1, List(Z)),
    (0, Some('a'), X) -> (1, List(X)),
    (0, Some('b'), Z) -> (3, List(Z)),
    (0, Some('b'), X) -> (3, List(X)),
    (1, Some('a'), X) -> (1, List()),
    (1, Some('b'), X) -> (1, List()),
    (1, None, Z) -> (6, List()),
    (1, None, Z) -> (2, List(Z)),
    (3, Some('a'), X) -> (3, List()),
    (3, Some('b'), X) -> (3, List()),
    (3, None, Z) -> (6, List()),
    (3, None, Z) -> (4, List(Z)),
    (2, Some('a'), Z) -> (2, List(X, Z)),
    (2, Some('a'), X) -> (2, List(X, X)),
    (2, Some('b'), Z) -> (2, List(X, Z)),
    (2, Some('b'), X) -> (2, List(X, X)),
    (4, Some('a'), Z) -> (4, List(X, Z)),
    (4, Some('a'), X) -> (4, List(X, X)),
    (4, Some('b'), Z) -> (4, List(X, Z)),
    (4, Some('b'), X) -> (4, List(X, X)),
    (2, Some('b'), Z) -> (5, List(Z)),
    (2, Some('b'), X) -> (5, List(X)),
    (4, Some('a'), Z) -> (5, List(Z)),
    (4, Some('a'), X) -> (5, List(X)),
    (5, Some('a'), X) -> (5, List()),
    (5, Some('b'), X) -> (5, List()),
    (5, None, Z) -> (6, List()),
  )
  val pda7: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(3),
    (0, Some('a'), Z) -> (0, List(X, Z)),
    (0, Some('a'), X) -> (0, List(X, X)),
    (0, None, Z) -> (1, List(Z)),
    (0, None, X) -> (1, List(X)),
    (1, Some('b'), X) -> (2, List()),
    (2, Some('b'), X) -> (1, List()),
    (2, None, Z) -> (3, List(Z)),
  )
  val pda8: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(3),
    (0, Some('a'), Z) -> (0, List(X, Z)),
    (0, Some('a'), X) -> (0, List(X, X)),
    (0, None, Z) -> (1, List(Z)),
    (0, None, X) -> (1, List(X)),
    (1, Some('b'), X) -> (2, List()),
    (1, None, Z) -> (3, List(Z)),
    (2, Some('b'), X) -> (1, List()),
  )
  val pda9: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(3),
    (0, Some('b'), Z) -> (0, List(X, X, Z)),
    (0, Some('b'), X) -> (0, List(X, X, X)),
    (0, None, Z) -> (1, List(Z)),
    (0, None, X) -> (1, List(X)),
    (1, Some('a'), Z) -> (1, List(Z)),
    (1, Some('a'), X) -> (1, List(X)),
    (1, None, Z) -> (2, List(Z)),
    (1, None, X) -> (2, List(X)),
    (2, Some('b'), X) -> (2, List()),
    (2, None, Z) -> (3, List()),
  )
  val pda10: PDA = PDA(
    initState = 0,
    initAlphabet = Z,
    finalStates = Set(1),
    (0, Some('('), Z) -> (0, List(X, Z)),
    (0, Some('('), X) -> (0, List(X, X)),
    (0, Some('('), Y) -> (0, List(X, Y)),
    (0, Some('{'), Z) -> (0, List(Y, Z)),
    (0, Some('{'), X) -> (0, List(Y, X)),
    (0, Some('{'), Y) -> (0, List(Y, Y)),
    (0, Some(')'), X) -> (0, List()),
    (0, Some('}'), Y) -> (0, List()),
    (0, None, Z) -> (1, List()),
  )

  val lang_an: Lang = (
    "ab".toSet,
    w => w.forall(_ == 'a')
  )
  val lang_an_bn: Lang = (
    "ab".toSet,
    w =>
      w.length % 2 == 0 &&
      w.substring(0, w.length / 2).forall(_ == 'a') &&
      w.substring(w.length / 2).forall(_ == 'b')
  )
  val lang_an_b2n: Lang = (
    "ab".toSet,
    w =>
      w.length % 3 == 0 &&
      w.substring(0, w.length / 3).forall(_ == 'a') &&
      w.substring(w.length / 3).forall(_ == 'b')
  )
  val lang_w_wR: Lang = (
    "ab".toSet,
    w =>
      w.length % 2 == 0 &&
      (0 until (w.length / 2)).forall(i => w(i) == w(w.length - i - 1))
  )
  val lang_na_eq_nb: Lang = (
    "ab".toSet,
    w => w.count(_ == 'a') == w.count(_ == 'b')
  )
  val lang_na_neq_nb: Lang = (
    "ab".toSet,
    w => w.count(_ == 'a') != w.count(_ == 'b')
  )
  val lang_na_eq_nb_minus_nc: Lang = (
    "abc".toSet,
    w => w.count(_ == 'a') == w.count(_ == 'b') - w.count(_ == 'c')
  )
  val lang_not_w_w: Lang = (
    "ab".toSet,
    w => !(
      w.length % 2 == 0 &&
      w.substring(0, w.length / 2) == w.substring(w.length / 2)
    )
  )
  val lang_a2n1_b2n1: Lang = (
    "ab".toSet,
    w =>
      w.length % 4 == 2 &&
      w.substring(0, w.length / 2).forall(_ == 'a') &&
      w.substring(w.length / 2).forall(_ == 'b')
  )
  val lang_a2n_b2n: Lang = (
    "ab".toSet,
    w =>
      w.length % 4 == 0 &&
      w.substring(0, w.length / 2).forall(_ == 'a') &&
      w.substring(w.length / 2).forall(_ == 'b')
  )
  val lang_bn_am_b2n: Lang = (
    "ab".toSet,
    w =>
      val len = w.length
      if (w contains 'a') {
        val s = w.indexOf('a')
        val e = w.lastIndexOf('a') + 1
        s * 2 == (len - e) && w.substring(s, e).forall(_ == 'a')
      } else len % 3 == 0
  )
  val lang_balanced: Lang = (
    "(){}".toSet,
    w => w.foldLeft((true, List[Char]())) {
      case ((true, stack), a) if a == '(' || a == '{' => (true, a :: stack)
      case ((true, '(' :: stack), ')') => (true, stack)
      case ((true, '{' :: stack), '}') => (true, stack)
      case _ => (false, Nil)
    } == (true, Nil)
  )
}
