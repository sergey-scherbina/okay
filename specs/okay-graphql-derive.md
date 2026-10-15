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

- [x] a case class's object type: names, types, nullables, lists, nested, an enum; extra fields added and replacing
- [x] recursive and mutually recursive derived types
- [x] enums derived, out and in; an input object derived and decoded
- [x] SDL and introspection of derived types as of hand-written ones
- [x] request errors' codes; a resolver's FieldError extensions; a plain failure without

## Decisions

- A MACRO (quotes), not inline recursion over `Tuple.fromProductTyped`: the inline recursion cannot give a getter
  its element's type without a type test on the tuple — the match on `erasedValue` refines nothing at run time. The
  macro selects each field by its own symbol (`a.city`), summons `Output[T]` at the field's type, and builds the
  `Field` — every getter typed by the compiler, no allocation per read. An enum's values are its cases' own
  references; an input object is built by its `Mirror.fromProduct` from a typed tuple.
- `Output[A]` carries `type B >: A`, the type its output type is of: `ListOf` is an `OutType[Seq[T]]`, and a
  `List[T]` field's getter returns the `List` as it is, by subtyping — no conversion, no cast.
- Recursive and mutually recursive types derive: a field's `Output` is summoned inside the type's lazy field list,
  so `given userT = ObjectType.derive[User](...)` referring to `User` is read when the fields are, after the given.

## Results

- TestGraphQLDerive, 5 tests: a User of six parameters (a nested case class, an enum, a list, an optional
  recursive friend), its `orders` fetched and its `name` replaced by hand; SDL and introspection as of written
  types; an input object from a literal and from a variable; FieldError's extensions, a plain failure without,
  the four request codes. okay-graphql 43 on JVM, JS and Native.
