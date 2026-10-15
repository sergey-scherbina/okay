# okay-graphql-derive: types from case classes, extensions on errors

## Derivation

Writing every `Field("name", Scalar.string)(_.name)` by hand is the boilerplate of a schema. A case class already
says its fields and their types; a Scala enum, its values.

- `ObjectType.derive[A](extra*)`: an object type of the case class `A` — its name the class's, a field per
  constructor parameter, of the output type its Scala type has (`Output[T]`): `Int`, `Double`, `String`,
  `Boolean`, `Option[T]` nullable, `List`/`Seq`/`Vector[T]` a list, an enum or another object type whose
  `Output` is given. `extra` fields are added — the fetched ones, written by hand — and replace a derived one of
  the same name. Types may refer to each other and to themselves (fields are lazy).
- `EnumType.derive[E]` / `InEnum.derive[E]`: a Scala enum of singleton cases, its values by their names.
- `InObject.derive[A]`: an input object of the case class `A`, each parameter an argument of its input type
  (`Input[T]`); `Option[T]` optional; a default parameter value is not read (Mirror has none).
- Typed throughout: a field's getter is the element of `Tuple.fromProductTyped` at its type, found by inline
  recursion over the mirror's element types — no `productElement`, no cast; an input built by `fromProduct` of a
  typed tuple.

## Extensions

A GraphQL error may carry `extensions`, a JSON object clients read (`code` above all).

- Request errors carry a code: `GRAPHQL_PARSE_FAILED`, `GRAPHQL_VALIDATION_FAILED`, `BAD_USER_INPUT` (a
  variable's value); an argument a field cannot decode, `BAD_USER_INPUT`.
- A resolver fails with its own: `FieldError("not found", "code" -> JStr("NOT_FOUND"))` — raised, or thrown —
  gives its message and its extensions; any other failure, its message alone.

## Behavior

- [ ] a case class's object type: names, types, nullables, lists, nested, an enum; extra fields added and replacing
- [ ] recursive and mutually recursive derived types
- [ ] enums derived, out and in; an input object derived and decoded
- [ ] SDL and introspection of derived types as of hand-written ones
- [ ] request errors' codes; a resolver's FieldError extensions; a plain failure without

## Decisions

- Inline recursion over `Tuple.fromProductTyped` rather than a macro: the getter's type is the compiler's, the
  code stays ordinary inline Scala. Its price: a field read builds the tuple of the object (one allocation of the
  object's arity per read); measured nowhere yet, noted.
