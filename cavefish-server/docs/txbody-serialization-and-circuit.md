# TxBody CBOR Serialization and Circuit Implications

This document explains how a Cardano TxBody is serialized, how that CBOR
encoding feeds the circuit, and why the txId logic is sensitive to the exact
bytes. It also summarizes canonical CBOR rules and how they apply here.

## Terminology

- TxBody map: The ledger TxBody itself (a CBOR map with integer keys).
- TxBody wrapper: The CBOR encoding produced by `Cardano.Api.serialiseToCBOR`
  for `TxBody`. In this codebase, it is a 4-item CBOR list whose first element
  is the TxBody map, followed by additional elements (scripts / script data /
  metadata / validity depending on era and `cardano-api` encoding).
- Message bits: The bit vector fed into the circuit, derived from the CBOR
  bytes of the wrapper and padded to a fixed length.

## Canonical CBOR (why txId is deterministic)

Cardano ledger serialization uses canonical CBOR, which makes the byte stream
deterministic for the same logical transaction body. Key rules:

- Map keys are sorted by canonical CBOR ordering (for small integer keys, this
  is numeric order: 0, 1, 2, ...).
- Integers use the shortest possible encoding (no leading zero bytes).
- Arrays and maps use definite lengths (no indefinite-length encodings).
- Byte strings use definite lengths with exact content.
- Sets are encoded with CBOR tag 258 (`d9 01 02`) and their elements are
  serialized in canonical order.

Because txId is `Blake2b-256` of the TxBody map bytes, *any* change to the
canonical CBOR encoding (field order, integer width, or set ordering) changes
the txId.

Implementation references:

- `wbps/lib/WBPS/Core/Session/Steps/Demonstration/Artefacts/Cardano/UnsignedTx.hs`
  uses `CBOR.serialize'` in `txBodyMapByteLength` to measure the ledger TxBody
  map length (canonical CBOR).

## Haskell serialization pipeline

1. The unsigned transaction body (`Api.TxBody`) is serialized:
   - Code: `toBitsPaddedToMaxSize` in
     `wbps/lib/WBPS/Core/Session/Steps/Demonstration/Artefacts/PreparedMessage/Prepare.hs`
   - It uses `Cardano.Api.serialiseToCBOR (txUnsigned unsignedTx)`.
2. The CBOR bytes are converted to bits:
   - Per-byte little-endian bit order (`payloadBits` in the same file).
3. The bit vector is padded to the circuit size (currently 8 * 254 bits).

The circuit therefore receives a fixed-length bit vector representing the
*wrapper* CBOR encoding, not just the map.

## Circuit txId computation

Inside the circuit:

- `ComputeTxId` in `wbps/setup/relation/relation.circom`:
  - Converts message bits back to bytes.
  - Extracts the TxBody map bytes from the wrapper.
  - Hashes those bytes with `Blake2b-256`.

Current logic (simplified):

```
// 1 byte list header + 3 trailing elements
tx_body_size_bytes = message_size_bytes - 4;
tx_body_bytes[i] = in_message_bytes[i + 1];
txId = Blake2b(tx_body_bytes);
```

This assumes the wrapper is:

```
[
  txBodyMap,
  <element2>,
  <element3>,
  <element4>
]
```

and that `<element2..4>` are each 1 byte in the current fixture (empty/default
values). The txId computed in-circuit therefore matches
`Cardano.Api.getTxId` in:

- `txIdFromMessage` in
  `wbps/lib/WBPS/Core/Session/Steps/Proving/Artefacts/Challenge.hs`

## Example (fixture CBOR)

The fixture in `wbps/tests/integration/fixtures/commitment/unsignedTx.json`
starts with:

```
84 a3 ...
   ... a0 f5 f6
```

Interpretation:

- `84`: CBOR list of 4 items (TxBody wrapper).
- `a3`: CBOR map with 3 entries (TxBody map).
- The trailing `a0 f5 f6` are one-byte encodings of empty/default fields,
  which makes the wrapper length `1 + txBodyMapBytes + 3`.

That exact byte layout is why `message_size_bytes - 4` works for the current
minimal transaction shape.

## Why this matters (constraints)

The circuit does **not** parse CBOR. It uses a fixed offset/length to isolate
the map bytes. This is only valid when:

- The wrapper is a 4-item list, and
- The last three items are encoded as 1 byte each.

If any of those elements are non-empty (e.g., auxiliary data, scripts, non-
default validity), those elements can take more than 1 byte and the circuit
will hash the wrong slice. In that case the txId will diverge from
`Cardano.Api.getTxId`.

### Practical implications

- Any change to the TxBody map contents changes txId (as expected).
- Any change that affects canonical CBOR encoding also changes txId.
- The circuit must either:
  - enforce this minimal wrapper shape, or
  - be given the actual map length, or
  - parse CBOR in-circuit (expensive).

## Current design limitations (what txs the circuit can handle)

The current circuit design only supports a narrow TxBody shape. In practice:

- **Era/encoding fixed**: The code assumes `Cardano.Api`'s Conway-era
  `TxBody` encoding. A different era or a change in `cardano-api`'s CBOR
  wrapper layout would change the byte layout and break the fixed offsets.
- **Wrapper must be minimal**: The CBOR wrapper is assumed to be a 4-item
  list where the last three items are 1-byte empty/default values. That means:
  - no scripts (native or Plutus),
  - no script data (datums/redeemers),
  - no auxiliary data (metadata).
  Any non-empty value in those positions shifts the bytes and the circuit
  hashes the wrong slice.
- **Fixed message size**: The circuit is compiled for a fixed message size
  (currently 254 bytes). Transactions must be small enough to fit and must be
  padded to exactly that size. Padding currently relies on splitting an
  ADA-only output, so transactions without a splittable ADA-only output may
  fail to pad.
- **Fixed input encoding**: The private overlay window assumes:
  - the inputs set is the first map entry (key `0`),
  - the CBOR map header is 1 byte (small map size),
  - the inputs set uses CBOR tag 258 and a 1-byte array header (`N <= 23`),
  - each input index (`TxIx`) is a small int (`0..23`) so its CBOR size is 1 byte.
  If any of these change, the hardcoded offset/size no longer matches the
  actual encoding.
- **Fixed input count**: The circuit enforces an exact input count
  (`CircuitTxInputSize`, default 1). Any transaction with a different number
  of inputs is rejected.

These constraints mean the circuit is suitable for a very specific, minimal
Conway TxBody shape and will not work for general-purpose transactions without
changes to the circuit (or adding CBOR parsing inside it).

## Implementation references (entry points)

- CBOR -> bits -> padding:
  - `wbps/lib/WBPS/Core/Session/Steps/Demonstration/Artefacts/PreparedMessage/Prepare.hs`
    (`toBitsPaddedToMaxSize`, `payloadBits`)
- TxBody map length:
  - `wbps/lib/WBPS/Core/Session/Steps/Demonstration/Artefacts/Cardano/UnsignedTx.hs`
    (`txBodyMapByteLength`)
- Circuit txId:
  - `wbps/setup/relation/relation.circom` (`ComputeTxId`)
- Haskell txId reference:
  - `wbps/lib/WBPS/Core/Session/Steps/Proving/Artefacts/Challenge.hs`
    (`txIdFromMessage`, `computeByUsingTxId`)
- Commitment limb sizing:
  - `wbps/setup/commitment/BuildCommitment.circom`
  - `wbps/lib/WBPS/Core/Setup/Circuit/Parameters.hs`
