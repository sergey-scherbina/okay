- [ ] semantic-layer — complete backend-independent analytical contracts
      above semantic-core: derived metrics, comparisons, having/order/page,
      temporal bucketing, mergeable aggregates, checked dimension joins,
      Source/Bulk/Tables/files/Arrow and a JSON query API.
      Spec: specs/semantic-layer.md. Related semantic-joins and
      semantic-time-derived records close when this lands.
      Validate algebra, join cardinality, SQL parity, source cancellation,
      file/Arrow roundtrips and serialization with focused platform gates.
