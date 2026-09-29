package okay.refine.fpml

import okay.codec.Json
import okay.codec.Json.{JObj, JStr}
import okay.refine.Refine
import okay.refine.Refine.json.{each, field, num, str}

/**
 * THE PUBLIC PROVER (specs/refine.md §4): enough FpML to read ISDA's
 * two example documents end to end and write them back as a skeleton.
 * Test scope on purpose — these patterns are the shape of the domain
 * work, not the domain: every further product, version and the CDM are
 * the private repository's.
 *
 * What a pattern here looks like: a path of `Refine.json` steps, each
 * named, so a verdict reads `dataDocument/trade/swap/swapStream/…` and a
 * refusal says exactly which element was missing. `write` builds the
 * skeleton the read needs — not the original document, which the
 * lossless tree keeps — so `read(write(x)) == x` is the law that holds.
 */
object Fpml:

  /** a swap as the risk system first wants it: the two legs, not the schedules */
  final case class Swap(tradeId: String, tradeDate: String, currency: String, notional: Double,
                        fixedRate: Double, floatingIndex: String, effective: String, termination: String)

  /** an FX forward: what is exchanged, when, at what rate */
  final case class FxForward(tradeId: String, tradeDate: String,
                             currency1: String, amount1: Double, currency2: String, amount2: Double,
                             valueDate: String, rate: Double)

  /** an FpML confirmation-view data document: the root names itself and
   * carries `fpmlVersion`; anything else is not FpML, whatever its tags */
  val document: Refine[Json, Json] =
    Refine.step[Json, Json]("dataDocument") {
      case JObj(Vector(("dataDocument", d @ JObj(fs)))) if fs.exists(_._1 == "@fpmlVersion") => Right(d)
      case JObj(Vector(("dataDocument", _))) => Left("a dataDocument without fpmlVersion")
      case JObj(Vector((root, _))) => Left(s"root element is <$root>, not <dataDocument>")
      case other => Left(s"not a document: ${Json.print(other).take(40)}")
    } {
      // the skeleton must read back: a dataDocument carries its version
      case JObj(fs) if fs.exists(_._1 == "@fpmlVersion") => JObj(Vector("dataDocument" -> JObj(fs)))
      case JObj(fs) => JObj(Vector("dataDocument" -> JObj(("@fpmlVersion" -> JStr("5-10")) +: fs)))
      case other => JObj(Vector("dataDocument" -> other))
    }

  val trade: Refine[Json, Json] = document andThen field("trade")

  private val tradeId: Refine[Json, String] =
    field("tradeHeader") andThen each("partyTradeIdentifier") andThen
      Refine.step[Vector[Json], String]("tradeId")(ids =>
        // a plain tradeId or a versioned one; the first party's
        ids.view.flatMap(id => (field("tradeId") <|> (field("versionedTradeId") andThen field("tradeId"))).run(id).toOption)
          .headOption.flatMap {
            case JStr(s) => Some(s)
            case JObj(fs) => fs.collectFirst { case ("#text", JStr(s)) => s }   // tradeId with a scheme attribute
            case _ => None
          }.toRight("no tradeId in any partyTradeIdentifier"))(
        s => Vector(JObj(Vector("tradeId" -> JStr(s)))))

  private val tradeDate: Refine[Json, String] = field("tradeHeader") andThen field("tradeDate") andThen str

  private val currencyText: Refine[Json, String] =
    Refine.step[Json, String]("currency") {
      case JStr(s) => Right(s)
      case JObj(fs) => fs.collectFirst { case ("#text", JStr(s)) => s }.toRight("currency without text")
      case other => Left(s"not a currency: ${Json.print(other)}")
    }(JStr(_))

  /** one swapStream's calculation, and whether it is the fixed or the floating leg */
  private def calc(stream: Json): Either[String, Json] =
    (field("calculationPeriodAmount") andThen field("calculation")).run(stream).toOption.toRight("a swapStream without a calculation")

  val swap: Refine[Json, Swap] =
    Refine.step[Json, Swap]("swap") { t =>
      for
        id <- tradeId.run(t).toOption.toRight("no tradeId")
        date <- tradeDate.run(t).toOption.toRight("no tradeDate")
        streams <- (field("swap") andThen each("swapStream")).run(t) match
          case okay.refine.Verdict.Took(vs, _, _) => Right(vs)
          case v => Left(v.reasons.map(_.reason).mkString("; "))
        calcs <- streams.foldLeft[Either[String, Vector[Json]]](Right(Vector.empty))((acc, s) => acc.flatMap(cs => calc(s).map(cs :+ _)))
        fixed <- calcs.flatMap(c => (field("fixedRateSchedule") andThen field("initialValue") andThen num).run(c).toOption).headOption
          .toRight("no fixed leg (fixedRateSchedule)")
        index <- calcs.flatMap(c => (field("floatingRateCalculation") andThen field("floatingRateIndex") andThen str).run(c).toOption).headOption
          .toRight("no floating leg (floatingRateCalculation)")
        notionalNode <- calcs.flatMap(c => (field("notionalSchedule") andThen field("notionalStepSchedule")).run(c).toOption).headOption
          .toRight("no notionalStepSchedule")
        notional <- (field("initialValue") andThen num).run(notionalNode).toOption.toRight("no notional initialValue")
        ccy <- (field("currency") andThen currencyText).run(notionalNode).toOption.toRight("no notional currency")
        dates <- (field("calculationPeriodDates")).run(streams.head).toOption.toRight("no calculationPeriodDates")
        eff <- (field("effectiveDate") andThen field("unadjustedDate") andThen str).run(dates).toOption.toRight("no effectiveDate")
        term <- (field("terminationDate") andThen field("unadjustedDate") andThen str).run(dates).toOption.toRight("no terminationDate")
      yield Swap(id, date, ccy, notional, fixed, index, eff, term)
    } { s =>
      def leg(calc: Json) = JObj(Vector(
        "calculationPeriodDates" -> JObj(Vector(
          "effectiveDate" -> JObj(Vector("unadjustedDate" -> JStr(s.effective))),
          "terminationDate" -> JObj(Vector("unadjustedDate" -> JStr(s.termination))))),
        "calculationPeriodAmount" -> JObj(Vector("calculation" -> calc))))
      val notional = "notionalSchedule" -> JObj(Vector("notionalStepSchedule" -> JObj(Vector(
        "initialValue" -> JStr(s.notional.toString), "currency" -> JStr(s.currency)))))
      val fixed = JObj(Vector(notional, "fixedRateSchedule" -> JObj(Vector("initialValue" -> JStr(s.fixedRate.toString)))))
      val floating = JObj(Vector(notional, "floatingRateCalculation" -> JObj(Vector("floatingRateIndex" -> JStr(s.floatingIndex)))))
      JObj(Vector(
        "tradeHeader" -> JObj(Vector(
          "partyTradeIdentifier" -> JObj(Vector("tradeId" -> JStr(s.tradeId))),
          "tradeDate" -> JStr(s.tradeDate))),
        "swap" -> JObj(Vector("swapStream" -> Json.JArr(Vector(leg(floating), leg(fixed)))))))
    }

  val fxForward: Refine[Json, FxForward] =
    Refine.step[Json, FxForward]("fxForward") { t =>
      def leg(n: Int) = field("fxSingleLeg") andThen field(s"exchangedCurrency$n") andThen field("paymentAmount")
      def take[X](r: Refine[Json, X], what: String) = r.run(t).toOption.toRight(s"no $what")
      for
        id <- take(tradeId, "tradeId")
        date <- take(tradeDate, "tradeDate")
        c1 <- take(leg(1) andThen field("currency") andThen currencyText, "exchangedCurrency1 currency")
        a1 <- take(leg(1) andThen field("amount") andThen num, "exchangedCurrency1 amount")
        c2 <- take(leg(2) andThen field("currency") andThen currencyText, "exchangedCurrency2 currency")
        a2 <- take(leg(2) andThen field("amount") andThen num, "exchangedCurrency2 amount")
        vd <- take(field("fxSingleLeg") andThen field("valueDate") andThen str, "valueDate")
        rate <- take(field("fxSingleLeg") andThen field("exchangeRate") andThen field("rate") andThen num, "exchangeRate rate")
      yield FxForward(id, date, c1, a1, c2, a2, vd, rate)
    } { f =>
      def amount(c: String, a: Double) = JObj(Vector("paymentAmount" -> JObj(Vector("currency" -> JStr(c), "amount" -> JStr(a.toString)))))
      JObj(Vector(
        "tradeHeader" -> JObj(Vector(
          "partyTradeIdentifier" -> JObj(Vector("tradeId" -> JStr(f.tradeId))),
          "tradeDate" -> JStr(f.tradeDate))),
        "fxSingleLeg" -> JObj(Vector(
          "exchangedCurrency1" -> amount(f.currency1, f.amount1),
          "exchangedCurrency2" -> amount(f.currency2, f.amount2),
          "valueDate" -> JStr(f.valueDate),
          "exchangeRate" -> JObj(Vector("rate" -> JStr(f.rate.toString)))))))
    }

  /** the level: a trade is one of these, and a verdict says which and why not the other */
  val instrument: Refine[Json, Product] =
    trade andThen (swap.widen[Product] <|> fxForward.widen[Product])
