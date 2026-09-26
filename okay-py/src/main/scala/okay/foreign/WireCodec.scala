package okay.foreign

// The wire's codecs live in okay-codec since wire-givens-r, where okay-r's
// engine takes the same givens; these names keep `okay.foreign.WireFormat.Cbor.given`
// meaning what it meant.
export okay.codec.{FrameFormat, WireAuth, WireCbor, WireCompression, WireDeadline, WireFormat, WireSecurity}
