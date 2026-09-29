package okay.refine.fpml

/**
 * The two public FpML 5.10 documents the prover reads (specs/refine.md
 * §4, "The domain is not here"): ISDA's own examples, taken from the
 * FINOS Common Domain Model repository
 * (github.com/finos/common-domain-model, Apache-2.0),
 * `rosetta-source/src/main/resources/ingest/input/fpml-5-10-products-rates/
 * ird-ex01-vanilla-swap-versioned.xml` and
 * `…/fpml-5-10-products-fx/fx-ex03-fx-fwd.xml`, verbatim. FpML itself is
 * ISDA's, published under the FpML Public License. Held as literals, not
 * resources, so the JS and Native suites read them too.
 */
object Samples:

  /** ird-ex01: a plain-vanilla EUR interest-rate swap, fixed 6% against EUR-LIBOR-BBA 6M */
  val vanillaSwap: String = """<?xml version="1.0" encoding="utf-8"?>
<dataDocument xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="http://www.fpml.org/FpML-5/confirmation ../../../../schemas/fpml-5-10/confirmation/fpml-main-5-10.xsd"
  xmlns="http://www.fpml.org/FpML-5/confirmation" fpmlVersion="5-10">
  <trade>
    <tradeHeader>
      <!--
      <partyTradeIdentifier>
        <partyReference href="party1" />
        <tradeId tradeIdScheme="http://www.partyA.com/swaps/trade-id">TW9235</tradeId>
      </partyTradeIdentifier>
      -->
      <partyTradeIdentifier>
        <partyReference href="party2" />
        <versionedTradeId>
          <tradeId tradeIdScheme="http://www.barclays.com/swaps/trade-id">SW2000</tradeId>
          <version>1</version>
        </versionedTradeId>
      </partyTradeIdentifier>
      <tradeDate>1994-12-12</tradeDate>
    </tradeHeader>
    <swap>
      <swapStream>
        <payerPartyReference href="party1" />
        <receiverPartyReference href="party2" />
        <calculationPeriodDates id="floatingCalcPeriodDates">
          <effectiveDate>
            <unadjustedDate>1994-12-14</unadjustedDate>
            <dateAdjustments>
              <businessDayConvention>NONE</businessDayConvention>
            </dateAdjustments>
          </effectiveDate>
          <terminationDate>
            <unadjustedDate>1999-12-14</unadjustedDate>
            <dateAdjustments>
              <businessDayConvention>MODFOLLOWING</businessDayConvention>
              <businessCenters id="primaryBusinessCenters">
                <businessCenter>DEFR</businessCenter>
              </businessCenters>
            </dateAdjustments>
          </terminationDate>
          <calculationPeriodDatesAdjustments>
            <businessDayConvention>MODFOLLOWING</businessDayConvention>
            <businessCentersReference href="primaryBusinessCenters" />
          </calculationPeriodDatesAdjustments>
          <calculationPeriodFrequency>
            <periodMultiplier>6</periodMultiplier>
            <period>M</period>
            <rollConvention>14</rollConvention>
          </calculationPeriodFrequency>
        </calculationPeriodDates>
        <paymentDates>
          <calculationPeriodDatesReference href="floatingCalcPeriodDates" />
          <paymentFrequency>
            <periodMultiplier>6</periodMultiplier>
            <period>M</period>
          </paymentFrequency>
          <payRelativeTo>CalculationPeriodEndDate</payRelativeTo>
          <paymentDatesAdjustments>
            <businessDayConvention>MODFOLLOWING</businessDayConvention>
            <businessCentersReference href="primaryBusinessCenters" />
          </paymentDatesAdjustments>
        </paymentDates>
        <resetDates id="resetDates">
          <calculationPeriodDatesReference href="floatingCalcPeriodDates" />
          <resetRelativeTo>CalculationPeriodStartDate</resetRelativeTo>
          <fixingDates>
            <periodMultiplier>-2</periodMultiplier>
            <period>D</period>
            <dayType>Business</dayType>
            <businessDayConvention>NONE</businessDayConvention>
            <businessCenters>
              <businessCenter>GBLO</businessCenter>
            </businessCenters>
            <dateRelativeTo href="resetDates" />
          </fixingDates>
          <resetFrequency>
            <periodMultiplier>6</periodMultiplier>
            <period>M</period>
          </resetFrequency>
          <resetDatesAdjustments>
            <businessDayConvention>MODFOLLOWING</businessDayConvention>
            <businessCentersReference href="primaryBusinessCenters" />
          </resetDatesAdjustments>
        </resetDates>
        <calculationPeriodAmount>
          <calculation>
            <notionalSchedule>
              <notionalStepSchedule>
                <initialValue>50000000.00</initialValue>
                <currency currencyScheme="http://www.fpml.org/coding-scheme/external/iso4217">EUR</currency>
              </notionalStepSchedule>
            </notionalSchedule>
            <floatingRateCalculation>
              <floatingRateIndex>EUR-LIBOR-BBA</floatingRateIndex>
              <indexTenor>
                <periodMultiplier>6</periodMultiplier>
                <period>M</period>
              </indexTenor>
            </floatingRateCalculation>
            <dayCountFraction>ACT/360</dayCountFraction>
          </calculation>
        </calculationPeriodAmount>
      </swapStream>
      <swapStream>
        <payerPartyReference href="party2" />
        <receiverPartyReference href="party1" />
        <calculationPeriodDates id="fixedCalcPeriodDates">
          <effectiveDate>
            <unadjustedDate>1994-12-14</unadjustedDate>
            <dateAdjustments>
              <businessDayConvention>NONE</businessDayConvention>
            </dateAdjustments>
          </effectiveDate>
          <terminationDate>
            <unadjustedDate>1999-12-14</unadjustedDate>
            <dateAdjustments>
              <businessDayConvention>MODFOLLOWING</businessDayConvention>
              <businessCentersReference href="primaryBusinessCenters" />
            </dateAdjustments>
          </terminationDate>
          <calculationPeriodDatesAdjustments>
            <businessDayConvention>MODFOLLOWING</businessDayConvention>
            <businessCentersReference href="primaryBusinessCenters" />
          </calculationPeriodDatesAdjustments>
          <calculationPeriodFrequency>
            <periodMultiplier>1</periodMultiplier>
            <period>Y</period>
            <rollConvention>14</rollConvention>
          </calculationPeriodFrequency>
        </calculationPeriodDates>
        <paymentDates>
          <calculationPeriodDatesReference href="fixedCalcPeriodDates" />
          <paymentFrequency>
            <periodMultiplier>1</periodMultiplier>
            <period>Y</period>
          </paymentFrequency>
          <payRelativeTo>CalculationPeriodEndDate</payRelativeTo>
          <paymentDatesAdjustments>
            <businessDayConvention>MODFOLLOWING</businessDayConvention>
            <businessCentersReference href="primaryBusinessCenters" />
          </paymentDatesAdjustments>
        </paymentDates>
        <calculationPeriodAmount>
          <calculation>
            <notionalSchedule>
              <notionalStepSchedule>
                <initialValue>50000000.00</initialValue>
                <currency currencyScheme="http://www.fpml.org/coding-scheme/external/iso4217">EUR</currency>
              </notionalStepSchedule>
            </notionalSchedule>
            <fixedRateSchedule>
              <initialValue>0.06</initialValue>
            </fixedRateSchedule>
            <dayCountFraction>30E/360</dayCountFraction>
          </calculation>
        </calculationPeriodAmount>
      </swapStream>
    </swap>
  </trade>
  <party id="party1">
    <partyId partyIdScheme="http://www.fpml.org/coding-scheme/external/iso9362">PARTYAUS33</partyId>
    <partyName>Party A</partyName>
  </party>
  <party id="party2">
    <partyId partyIdScheme="http://www.fpml.org/coding-scheme/external/iso9362">BARCGB2L</partyId>
  </party>
</dataDocument>
"""

  /** fx-ex03: an EUR/USD FX forward, 10 000 000 EUR against 9 175 000 USD at 0.9175 */
  val fxForward: String = """<?xml version="1.0" encoding="utf-8"?>
<dataDocument xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="http://www.fpml.org/FpML-5/confirmation ../../../../schemas/fpml-5-10/confirmation/fpml-main-5-10.xsd"
  xmlns="http://www.fpml.org/FpML-5/confirmation" fpmlVersion="5-10">
  <trade>
    <tradeHeader>
      <partyTradeIdentifier>
        <partyReference href="party1" />
        <tradeId tradeIdScheme="http://www.abn-amro.com/fx/trade-id">ABN1234</tradeId>
      </partyTradeIdentifier>
      <partyTradeIdentifier>
        <partyReference href="party2" />
        <tradeId tradeIdScheme="http://www.db.com/fx/trade-id">DB5678</tradeId>
      </partyTradeIdentifier>
      <tradeDate>2001-11-19</tradeDate>
    </tradeHeader>
    <fxSingleLeg>
      <exchangedCurrency1>
        <payerPartyReference href="party2" />
        <receiverPartyReference href="party1" />
        <paymentAmount>
          <currency>EUR</currency>
          <amount>10000000</amount>
        </paymentAmount>
      </exchangedCurrency1>
      <exchangedCurrency2>
        <payerPartyReference href="party1" />
        <receiverPartyReference href="party2" />
        <paymentAmount>
          <currency>USD</currency>
          <amount>9175000</amount>
        </paymentAmount>
      </exchangedCurrency2>
      <valueDate>2001-12-21</valueDate>
      <exchangeRate>
        <quotedCurrencyPair>
          <currency1>EUR</currency1>
          <currency2>USD</currency2>
          <quoteBasis>Currency2PerCurrency1</quoteBasis>
        </quotedCurrencyPair>
        <rate>0.9175</rate>
        <spotRate>0.9130</spotRate>
        <forwardPoints>0.0045</forwardPoints>
      </exchangeRate>
    </fxSingleLeg>
  </trade>
  <party id="party1">
    <partyId partyIdScheme="http://www.fpml.org/coding-scheme/external/iso17442">BFXS5XCH7N0Y05NIXW11</partyId>
  </party>
  <party id="party2">
    <partyId partyIdScheme="http://www.fpml.org/coding-scheme/external/iso17442">213800QILIUD4ROSUO03</partyId>
  </party>
</dataDocument>
"""
