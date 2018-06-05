sealed trait Term

case class ElementaryConjunction(vars: SingleTerm*) extends Term {

  def length = vars.length
  def conjuncts = vars

  def toSymbolSet = vars.map { v => v.symbol }.toSet

  def isSubConjunction(e: ElementaryConjunction): Boolean = {
    val subs = e.vars.filter {
      x: SingleTerm => vars contains x
    }

    subs.length == e.vars.length
  }

  def isSubConjunction(v: SingleTerm): Boolean = vars contains v

  override def toString = vars.map { _.toString }.mkString("")
  //override def compare(t: ElementaryConjunction): Int = 0
}

case class Disjunction(vars: ElementaryConjunction*) extends Term {
  override def toString = vars.map { _.toString }.mkString(" ∨ ")
  def length = vars.map { x => x.length }.sum

  def primeImplicants: Disjunction = {

    // all possible pairs to check
    val pairs = (for {
      (a, i) <- vars.zipWithIndex; 
      (b, j) <- vars.zipWithIndex
      if i < j
    } yield (a, b))

    // Get all implicants from the pairs
    // and filter elements that matched
    var matched: List[ElementaryConjunction] = Nil
    var primes: Seq[ElementaryConjunction] = pairs.map { 
      case (a, b) => Implicant.findPrime(a, b) match {
        case Some(x) => 
          matched = a :: b :: matched
          Some(x)
        case None => None
    }}
      .filterNot { _ == None }.map { _.get }.toSeq

      // if we haven't found any new implicants,
      // current DF is contained of prime implicants
      if(primes.length == 0)
        return this

      // We found more implicants,

      // based on matched elements, filter those
      // that didn't match
      val notMatched: Seq[ElementaryConjunction] = vars.filterNot { 
        matched.toSet contains _ 
      }

      // finally, implicants are those that matched
      // summed to those that didn't match with any
      var finalPrimes = Disjunction((primes ++ notMatched):_*)

      finalPrimes = finalPrimes.primeImplicants
      finalPrimes
  }

  def minimal = {

    def conjunctsImplicateFormula(implicants: Disjunction, formula: Disjunction) = {
      formula.vars.filter {
        // there exists at least 1 implicant such that
        // the predicate isSubConjunction is satisfied
        x => implicants.vars.filter { i => x isSubConjunction i }.length > 0
      }.length == formula.vars.length
    }

    val possibleMinDfs = this.primeImplicants.vars.toSet.subsets.map {
      x => Disjunction(x.toSeq:_*)
    }

    var minLength: Int = Int.MaxValue
    val minimalDfs = possibleMinDfs.filter {
      df: Disjunction =>
        conjunctsImplicateFormula(df, this) match {
          case true => 
            if(df.length < minLength)
              minLength = df.length
            true
          case false => false
        }
    }

    minimalDfs.filter { 
      df: Disjunction => df.length == minLength 
    }.toSeq
  }

}

sealed trait SingleTerm extends Term {
  def symbol: String
  //override def compare(v: SingleTerm): Int = symbol compare v.symbol
}

case class Variable(symbol: String) extends SingleTerm {
  override def toString = s"$symbol"

}

case class Complement(variable: Variable) extends SingleTerm {
  def symbol = variable.symbol

  override def toString = s"$variable'"
}

object Implicant {
  def findPrime(a: ElementaryConjunction, b: ElementaryConjunction): 
  Option[ElementaryConjunction] =

    if(a.length != b.length) None
    else {
      val pairs = a.conjuncts zip b.conjuncts
      if(pairs.filterNot { 
        case (l, r) => l.symbol == r.symbol 
      }.length > 0) None
    else {
      if(pairs.filterNot { case (l, r) => l == r }.length > 1)
        None
      else {
        // :_* converts sequence to variable array of arguments
        val e = ElementaryConjunction(pairs.filter { 
          case (l, r) => l == r 
        }.map { _._1 }:_*)
        Some(e)
      }
    }
    }
}

object Main {
  def main(args: Array[String]): Unit = {
    SymbolParser("xyuv + xy'uv + x'yuv' + xy'u'v' + x'yu'v' + xyuv' + xy'uv' + xy'u'v + x'yuv + x'yu'v + x'y'u'v'") match {
      case Some(x) => 
        x.minimal.foreach {
          println _ 
        }
      case None => 
    }
  }
}

// xyuv + xy'uv + x'yuv' + xy'u'v' + x'yu'v' + xyuv' + xy'uv' + xy'u'v + x'yuv + x'yu'v + x'y'u'v'
