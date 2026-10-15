## okay-graphql-derive - types derived from case classes, extensions on errors

- `ObjectType.derive[A](extra*)`, `EnumType.derive[E]`, `InEnum.derive[E]`, `InObject.derive[A]`: a schema's types
  from case classes and enums — a macro selecting each field by its own symbol, `Output[T]` / `Input[T]` per field
  type (scalars, `Option`, `List`/`Seq`/`Vector`, given object types and enums), the fetched fields added by hand
  and replacing derived ones; recursive types derive.
- `extensions` on errors: request errors coded (`GRAPHQL_PARSE_FAILED`, `GRAPHQL_VALIDATION_FAILED`,
  `BAD_USER_INPUT`, `OPERATION_RESOLUTION_FAILURE`); a resolver's own by `FieldError` (thrown or `FieldError.raise`).
- specs/okay-graphql-derive.md, TestGraphQLDerive (5), docs/modules/okay-graphql.md.
