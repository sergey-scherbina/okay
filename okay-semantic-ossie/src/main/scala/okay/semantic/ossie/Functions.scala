package okay.semantic.ossie

import okay.semantic.Value
import Value.*

/** Pure scalar functions. Implementations must declare arity before execution. */
trait Functions extends Serializable:
  def accepts(name: String, arity: Int): Boolean
  def call(name: String, arguments: Vector[Value]): Either[String,Value]
object Functions:
  def orElse(primary: Functions, fallback: Functions): Functions = new Functions:
    def accepts(name: String, arity: Int): Boolean = primary.accepts(name,arity) || fallback.accepts(name,arity)
    def call(name: String, arguments: Vector[Value]): Either[String,Value] =
      if primary.accepts(name,arguments.size) then primary.call(name,arguments) else fallback.call(name,arguments)
  val portable: Functions = new Functions:
    private val one = Set("ABS","FLOOR","CEIL","CEILING","SIGN","SQRT","EXP","LN","LOG10","LOWER","UPPER","TRIM","LTRIM","RTRIM","LENGTH","ZEROIFNULL","NULLIFZERO")
    private val two = Set("MOD","POWER","LOG","NULLIF","IFNULL","NVL","LEFT","RIGHT","CONTAINS","STARTSWITH","ENDSWITH","CHARINDEX")
    private val three = Set("IF","IFF","NVL2","SUBSTRING","REPLACE","SPLIT_PART","BETWEEN")
    def accepts(name: String, arity: Int): Boolean =
      (one(name) && arity == 1) || (two(name) && arity == 2) || (three(name) && arity == 3) ||
        (Set("ROUND","TRUNC","TRUNCATE")(name) && (arity == 1 || arity == 2)) ||
        (Set("COALESCE","GREATEST","LEAST","CONCAT")(name) && arity >= 1) || (name == "IN" && arity >= 2)
    def call(name: String, arguments: Vector[Value]): Either[String,Value] =
      val a = arguments
      def number(v: Value): BigDecimal = v match
        case Number(n) => n
        case _ => throw new IllegalArgumentException(s"$name expects Number")
      def text(v: Value): String = v match
        case Text(s) => s
        case _ => throw new IllegalArgumentException(s"$name expects Text")
      def integer(v: Value): Int =
        val n = number(v)
        if !n.isWhole || !n.isValidInt then throw new IllegalArgumentException(s"$name expects an integral Int")
        n.toInt
      def approx(n: Double): Value = if n.isNaN || n.isInfinity then Null else Number(BigDecimal(n))
      if !accepts(name,a.size) then Left(s"unknown function/arity $name/${a.size}")
      else
        scala.util.Try {
          name match
            case "COALESCE" | "IFNULL" | "NVL" => a.find(_ != Null).getOrElse(Null)
            case "IF" | "IFF" => a(0) match
              case Bool(true) => a(1)
              case Bool(false) | Null => a(2)
              case _ => throw new IllegalArgumentException("IF condition must be Boolean")
            case "NVL2" => if a(0) != Null then a(1) else a(2)
            case "ZEROIFNULL" => if a.head == Null then Number(BigDecimal(0)) else a.head
            case "NULLIFZERO" => if a.head == Number(BigDecimal(0)) then Null else a.head
            case "NULLIF" => if a(0) == a(1) then Null else a(0)
            case "IN" => if a.head == Null then Null else if a.tail.contains(a.head) then Bool(true) else if a.tail.contains(Null) then Null else Bool(false)
            case _ if a.contains(Null) => Null
            case "ABS" => Number(number(a.head).abs)
            case "SIGN" => Number(BigDecimal(number(a.head).signum))
            case "FLOOR" => Number(number(a.head).setScale(0,BigDecimal.RoundingMode.FLOOR))
            case "CEIL" | "CEILING" => Number(number(a.head).setScale(0,BigDecimal.RoundingMode.CEILING))
            case "ROUND" | "TRUNC" | "TRUNCATE" =>
              val scale = if a.size == 1 then 0 else integer(a(1))
              if scale.toLong.abs > 10000 then throw new IllegalArgumentException("decimal scale exceeds 10000")
              Number(number(a.head).setScale(scale,if name == "ROUND" then BigDecimal.RoundingMode.HALF_UP else BigDecimal.RoundingMode.DOWN))
            case "MOD" => if number(a(1)) == 0 then Null else Number(BigDecimal(number(a(0)).bigDecimal.remainder(number(a(1)).bigDecimal)))
            case "POWER" => approx(math.pow(number(a(0)).toDouble,number(a(1)).toDouble))
            case "SQRT" => approx(math.sqrt(number(a.head).toDouble))
            case "EXP" => approx(math.exp(number(a.head).toDouble))
            case "LN" => approx(math.log(number(a.head).toDouble))
            case "LOG10" => approx(math.log10(number(a.head).toDouble))
            case "LOG" => approx(math.log(number(a(1)).toDouble) / math.log(number(a(0)).toDouble))
            case "GREATEST" | "LEAST" => a.reduceLeft((x,y) =>
              Value.compare(x,y) match
                case Some(c) => if (c >= 0) == (name == "GREATEST") then x else y
                case None => throw new IllegalArgumentException("incomparable arguments"))
            case "BETWEEN" =>
              val low = Value.compare(a(0),a(1)).getOrElse(throw new IllegalArgumentException("BETWEEN incomparable operands"))
              val high = Value.compare(a(0),a(2)).getOrElse(throw new IllegalArgumentException("BETWEEN incomparable operands"))
              Bool(low >= 0 && high <= 0)
            case "CONCAT" => Text(a.map(text).mkString)
            case "LOWER" => Text(text(a.head).toLowerCase)
            case "UPPER" => Text(text(a.head).toUpperCase)
            case "TRIM" => Text(text(a.head).trim)
            case "LTRIM" => Text(text(a.head).dropWhile(_.isWhitespace))
            case "RTRIM" => Text(text(a.head).reverse.dropWhile(_.isWhitespace).reverse)
            case "LENGTH" => Number(BigDecimal(text(a.head).codePointCount(0,text(a.head).length)))
            case "LEFT" => Text(text(a(0)).take(integer(a(1)).max(0)))
            case "RIGHT" => Text(text(a(0)).takeRight(integer(a(1)).max(0)))
            case "SUBSTRING" => Text(text(a(0)).drop((integer(a(1)) - 1).max(0)).take(integer(a(2)).max(0)))
            case "REPLACE" => Text(text(a(0)).replace(text(a(1)),text(a(2))))
            case "SPLIT_PART" =>
              val delimiter = text(a(1))
              if delimiter.isEmpty then throw new IllegalArgumentException("empty delimiter")
              val parts = text(a(0)).split(java.util.regex.Pattern.quote(delimiter),-1).toVector
              val i = integer(a(2))
              Text(parts.lift(if i > 0 then i - 1 else parts.size + i).getOrElse(""))
            case "CONTAINS" => Bool(text(a(0)).contains(text(a(1))))
            case "STARTSWITH" => Bool(text(a(0)).startsWith(text(a(1))))
            case "ENDSWITH" => Bool(text(a(0)).endsWith(text(a(1))))
            case "CHARINDEX" => Number(BigDecimal(text(a(1)).indexOf(text(a(0))) + 1))
            case _ => throw new IllegalArgumentException(s"unknown function $name")
        }.toEither.left.map(e => Option(e.getMessage).getOrElse(e.toString))

private[ossie] object Scalar:
  import Value.*
  def divide(x: BigDecimal, y: BigDecimal): Value =
    if y == 0 then Null else Number(BigDecimal(x.bigDecimal.divide(y.bigDecimal,java.math.MathContext.DECIMAL128)))
  def unary(op: String, v: Value): Either[String,Value] = (op,v) match
    case ("IS NULL",_) => Right(Bool(v == Null))
    case ("IS NOT NULL",_) => Right(Bool(v != Null))
    case (_,Null) => Right(Null)
    case ("-",Number(n)) => Right(Number(-n))
    case ("NOT",Bool(b)) => Right(Bool(!b))
    case _ => Left(s"$op: invalid operand $v")
  def binary(op: String, a: Value, b: Value): Either[String,Value] =
    if op == "AND" || op == "OR" then
      if Vector(a,b).exists(v => v != Null && !v.isInstanceOf[Bool]) then Left(s"$op expects Boolean")
      else if op == "AND" && (a == Bool(false) || b == Bool(false)) then Right(Bool(false))
      else if op == "OR" && (a == Bool(true) || b == Bool(true)) then Right(Bool(true))
      else if a == Null || b == Null then Right(Null)
      else Right(Bool(op == "AND"))
    else if a == Null || b == Null then Right(Null)
    else (op,a,b) match
      case ("+",Number(x),Number(y)) => Right(Number(BigDecimal(x.bigDecimal.add(y.bigDecimal))))
      case ("-",Number(x),Number(y)) => Right(Number(BigDecimal(x.bigDecimal.subtract(y.bigDecimal))))
      case ("*",Number(x),Number(y)) => Right(Number(BigDecimal(x.bigDecimal.multiply(y.bigDecimal))))
      case ("/",Number(x),Number(y)) => Right(divide(x,y))
      case ("%",Number(x),Number(y)) => Right(if y == 0 then Null else Number(BigDecimal(x.bigDecimal.remainder(y.bigDecimal))))
      case ("||",Text(x),Text(y)) => Right(Text(x + y))
      case ("LIKE" | "ILIKE",Text(x),Text(y)) =>
        // SQL wildcard matching on an explicit dynamic-programming row (no regex injection).
        val source = if op == "ILIKE" then x.toLowerCase else x
        val pattern = if op == "ILIKE" then y.toLowerCase else y
        var previous = Array.fill(source.length + 1)(false); previous(0) = true
        pattern.foreach { c =>
          val next = Array.fill(source.length + 1)(false)
          if c == '%' then next(0) = previous(0)
          var i = 1
          while i <= source.length do
            next(i) = if c == '%' then previous(i) || next(i - 1) else previous(i - 1) && (c == '_' || c == source(i - 1))
            i += 1
          previous = next
        }
        Right(Bool(previous(source.length)))
      case _ if Set("=","<>","!=","<",">","<=",">=")(op) =>
        Value.compare(a,b).toRight("incomparable operands").map(c => Bool(op match
          case "=" => c == 0
          case "<>" | "!=" => c != 0
          case "<" => c < 0
          case ">" => c > 0
          case "<=" => c <= 0
          case _ => c >= 0))
      case _ => Left(s"$op: invalid operands $a, $b")
