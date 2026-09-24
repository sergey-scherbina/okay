package okay.py

// The wire's codecs live in okay-codec since wire-givens-r, where okay-r's
// engine takes the same givens; these names keep `okay.py.WireFormat.Cbor.given`
// meaning what it meant.
export okay.codec.{WireAuth, WireCbor, WireCompression, WireDeadline, WireFormat}
