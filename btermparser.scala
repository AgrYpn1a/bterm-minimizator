import scala.util.parsing.combinator._
import scala.language.postfixOps

object SymbolParser extends RegexParsers {
	def symbol: Parser[SingleTerm] = variable

	def variable: Parser[SingleTerm] = {
		"""[a-z]{1}""".r ~ ("'"?) ^^ { 
			case v ~ Some(b) => Complement(Variable(v.toString))
			case v ~ None => Variable(v.toString)
		}
	}

	def product: Parser[ElementaryConjunction] = {
		rep(symbol) ^^ {
			case s => ElementaryConjunction(s.toSeq:_*)
		}
	}

	def sum: Parser[Disjunction] = {
		rep1sep(product, "+") ^^ {
			case s => Disjunction(s.toSeq:_*)
		}
	}

	def apply(s: String) =
		parse(sum, s) match {
			case Success(matched, _) => Some(matched)
			case _ => None
		}
}