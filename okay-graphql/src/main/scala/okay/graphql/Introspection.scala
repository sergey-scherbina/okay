package okay.graphql

import okay.one.Cont.pure

/** INTROSPECTION (GraphQL §4): `__schema` and `__type(name:)` on the query root, answered by ordinary object types
 * over the schema value — so introspection runs on the same executor as any query */
object Introspection:
  /** GraphiQL's introspection query (graphql-js `getIntrospectionQuery()`, its defaults) */
  val query: String =
    """query IntrospectionQuery {
      |  __schema {
      |    queryType { name }
      |    mutationType { name }
      |    subscriptionType { name }
      |    types { ...FullType }
      |    directives { name description locations args { ...InputValue } }
      |  }
      |}
      |fragment FullType on __Type {
      |  kind name description
      |  fields(includeDeprecated: true) {
      |    name description args { ...InputValue } type { ...TypeRef } isDeprecated deprecationReason
      |  }
      |  inputFields { ...InputValue }
      |  interfaces { ...TypeRef }
      |  enumValues(includeDeprecated: true) { name description isDeprecated deprecationReason }
      |  possibleTypes { ...TypeRef }
      |}
      |fragment InputValue on __InputValue { name description type { ...TypeRef } defaultValue }
      |fragment TypeRef on __Type {
      |  kind name ofType { kind name ofType { kind name ofType { kind name ofType { kind name ofType { kind name
      |  ofType { kind name ofType { kind name } } } } } } }
      |}""".stripMargin

  /** a type as introspection sees it: a named one, or a list or non-null wrapping */
  enum TypeRef:
    case Of(name: String)
    case ListT(of: TypeRef)
    case NonNullT(of: TypeRef)

  /** an output type's reference. Bounded: its lists and nulls, as the schema's code writes them */
  def outRef(t: OutType[?]): TypeRef = t match
    case Nullable(of) => strip(outRef(of))
    case Fetched(of) => outRef(of)
    case ListOf(of) => TypeRef.NonNullT(TypeRef.ListT(outRef(of)))
    case n: Named => TypeRef.NonNullT(TypeRef.Of(n.name))

  /** an input type's reference. Bounded: its lists and nulls, as the schema's code writes them */
  def inRef(t: InType[?]): TypeRef = t match
    case Optional(of) => strip(inRef(of))
    case InList(of) => TypeRef.NonNullT(TypeRef.ListT(inRef(of)))
    case n: InNamed => TypeRef.NonNullT(TypeRef.Of(n.name))

  private def strip(t: TypeRef): TypeRef = t match
    case TypeRef.NonNullT(of) => of
    case other => other

  final case class FieldInfo(name: String, description: Option[String], args: List[InputInfo], tpe: TypeRef,
      deprecated: Option[String])
  final case class InputInfo(name: String, description: Option[String], tpe: TypeRef, default: Option[String])
  final case class EnumValueInfo(name: String, description: Option[String], deprecated: Option[String])
  final case class DirectiveInfo(name: String, description: Option[String], locations: List[String],
      args: List[InputInfo])

  private def input(a: Arg[?]): InputInfo = a match
    case a: Arg[t] => InputInfo(a.name, a.description, inRef(a.tpe), a.default.map(a.tpe.literal))

  private def field(f: Field[?, ?]): FieldInfo =
    FieldInfo(f.name, f.description, f.args.map(input), outRef(f.tpe), f.deprecated)

  private val kinds = List("SCALAR", "OBJECT", "INTERFACE", "UNION", "ENUM", "INPUT_OBJECT", "LIST", "NON_NULL")
  private val locations = List("QUERY", "MUTATION", "SUBSCRIPTION", "FIELD", "FRAGMENT_DEFINITION",
    "FRAGMENT_SPREAD", "INLINE_FRAGMENT", "VARIABLE_DEFINITION", "SCHEMA", "SCALAR", "OBJECT", "FIELD_DEFINITION",
    "ARGUMENT_DEFINITION", "INTERFACE", "UNION", "ENUM", "ENUM_VALUE", "INPUT_OBJECT", "INPUT_FIELD_DEFINITION")

  private val directives = List(
    DirectiveInfo("skip", Some("Directs the executor to skip this field or fragment when the `if` argument is true."),
      List("FIELD", "FRAGMENT_SPREAD", "INLINE_FRAGMENT"),
      List(InputInfo("if", Some("Skipped when true."), TypeRef.NonNullT(TypeRef.Of("Boolean")), None))),
    DirectiveInfo("include", Some("Directs the executor to include this field or fragment only when the `if` argument is true."),
      List("FIELD", "FRAGMENT_SPREAD", "INLINE_FRAGMENT"),
      List(InputInfo("if", Some("Included when true."), TypeRef.NonNullT(TypeRef.Of("Boolean")), None))),
    DirectiveInfo("deprecated", Some("Marks an element of a GraphQL schema as no longer supported."),
      List("FIELD_DEFINITION", "ARGUMENT_DEFINITION", "INPUT_FIELD_DEFINITION", "ENUM_VALUE"),
      List(InputInfo("reason", None, TypeRef.Of("String"), Some("\"No longer supported\"")))))

  /** the root fields `__schema` and `__type`, over `schema` */
  def rootFields(schema: Schema): List[Field[Unit, ?]] =
    lazy val index = schema.index
    val includeDeprecated = Arg("includeDeprecated", Optional(InScalar.boolean), Some(Some(false)))
    def live(deprecated: Option[String], all: Option[Boolean]) = all.contains(true) || deprecated.isEmpty

    def kindOf(t: TypeRef): String = t match
      case TypeRef.ListT(_) => "LIST"
      case TypeRef.NonNullT(_) => "NON_NULL"
      case TypeRef.Of(n) => (index.outputs.get(n), index.input(n)) match
        case (Some(_: ObjectType[?]), _) => "OBJECT"
        case (Some(_: InterfaceType[?]), _) => "INTERFACE"
        case (Some(_: UnionType[?]), _) => "UNION"
        case (Some(_: EnumType[?]), _) | (None, Some(_: InEnum[?])) => "ENUM"
        case (None, Some(_: InObject[?])) => "INPUT_OBJECT"
        case _ => "SCALAR"
    def named(t: TypeRef): Option[String] = t match
      case TypeRef.Of(n) => Some(n)
      case _ => None
    def description(t: TypeRef): Option[String] = named(t).flatMap(n =>
      index.outputs.get(n).map(_.description).getOrElse(index.input(n).flatMap(_.description)))
    def fields(t: TypeRef, all: Option[Boolean]): Option[List[FieldInfo]] = named(t).flatMap(index.outputs.get) match
      case Some(o: ObjectType[?]) => Some(o.fields.filterNot(_.name.startsWith("__")).filter(f => live(f.deprecated, all)).map(field))
      case Some(i: InterfaceType[?]) => Some(i.fields.filter(f => live(f.deprecated, all)).map(field))
      case _ => None
    def interfaces(t: TypeRef): Option[List[TypeRef]] = named(t).flatMap(index.outputs.get) match
      case Some(o: ObjectType[?]) => Some(index.interfacesOf(o.name).map(i => TypeRef.Of(i.name)))
      case Some(_: InterfaceType[?]) => Some(Nil)
      case _ => None
    def possibleTypes(t: TypeRef): Option[List[TypeRef]] = named(t).flatMap(index.outputs.get) match
      case Some(i: InterfaceType[?]) => Some(i.members.map(m => TypeRef.Of(m.tpe.name)))
      case Some(u: UnionType[?]) => Some(u.members.map(m => TypeRef.Of(m.tpe.name)))
      case _ => None
    def enumValues(t: TypeRef, all: Option[Boolean]): Option[List[EnumValueInfo]] =
      named(t).flatMap(n => index.outputs.get(n).orElse(index.input(n))) match
        case Some(e: EnumType[?]) => Some(e.values.map(_._1).map(v => EnumValueInfo(v, None, e.deprecated.get(v)))
          .filter(v => live(v.deprecated, all)))
        case Some(e: InEnum[?]) => Some(e.values.map(v => EnumValueInfo(v._1, None, None)))
        case _ => None
    def inputFields(t: TypeRef): Option[List[InputInfo]] = named(t).flatMap(index.input) match
      case Some(i: InObject[?]) => Some(i.fields.map(input))
      case _ => None
    def ofType(t: TypeRef): Option[TypeRef] = t match
      case TypeRef.ListT(of) => Some(of)
      case TypeRef.NonNullT(of) => Some(of)
      case _ => None

    val kindT = EnumType[String]("__TypeKind", kinds.map(k => k -> k))
    val locationT = EnumType[String]("__DirectiveLocation", locations.map(l => l -> l))
    lazy val typeT: ObjectType[TypeRef] = ObjectType[TypeRef]("__Type")(List(
      Field("kind", kindT)(kindOf),
      Field("name", Nullable(Scalar.string))(named),
      Field("description", Nullable(Scalar.string))(description),
      Field.fetch("fields", Nullable(ListOf(fieldT)), includeDeprecated)((t, a) => pure(fields(t, a(includeDeprecated)))),
      Field("interfaces", Nullable(ListOf(typeT)))(interfaces),
      Field("possibleTypes", Nullable(ListOf(typeT)))(possibleTypes),
      Field.fetch("enumValues", Nullable(ListOf(enumValueT)), includeDeprecated)((t, a) =>
        pure(enumValues(t, a(includeDeprecated)))),
      Field.fetch("inputFields", Nullable(ListOf(inputValueT)), includeDeprecated)((t, _) => pure(inputFields(t))),
      Field("ofType", Nullable(typeT))(ofType),
      Field("specifiedByURL", Nullable(Scalar.string))(_ => None),
      Field("isOneOf", Nullable(Scalar.boolean))(t => if kindOf(t) == "INPUT_OBJECT" then Some(false) else None),
    ))
    lazy val fieldT: ObjectType[FieldInfo] = ObjectType[FieldInfo]("__Field")(List(
      Field("name", Scalar.string)(_.name),
      Field("description", Nullable(Scalar.string))(_.description),
      Field.fetch("args", ListOf(inputValueT), includeDeprecated)((f, _) => pure(f.args)),
      Field("type", typeT)(_.tpe),
      Field("isDeprecated", Scalar.boolean)(_.deprecated.isDefined),
      Field("deprecationReason", Nullable(Scalar.string))(_.deprecated),
    ))
    lazy val inputValueT: ObjectType[InputInfo] = ObjectType[InputInfo]("__InputValue")(List(
      Field("name", Scalar.string)(_.name),
      Field("description", Nullable(Scalar.string))(_.description),
      Field("type", typeT)(_.tpe),
      Field("defaultValue", Nullable(Scalar.string))(_.default),
      Field("isDeprecated", Scalar.boolean)(_ => false),
      Field("deprecationReason", Nullable(Scalar.string))(_ => None),
    ))
    lazy val enumValueT: ObjectType[EnumValueInfo] = ObjectType[EnumValueInfo]("__EnumValue")(List(
      Field("name", Scalar.string)(_.name),
      Field("description", Nullable(Scalar.string))(_.description),
      Field("isDeprecated", Scalar.boolean)(_.deprecated.isDefined),
      Field("deprecationReason", Nullable(Scalar.string))(_.deprecated),
    ))
    lazy val directiveT: ObjectType[DirectiveInfo] = ObjectType[DirectiveInfo]("__Directive")(List(
      Field("name", Scalar.string)(_.name),
      Field("description", Nullable(Scalar.string))(_.description),
      Field("locations", ListOf(locationT))(_.locations),
      Field.fetch("args", ListOf(inputValueT), includeDeprecated)((d, _) => pure(d.args)),
      Field("isRepeatable", Scalar.boolean)(_ => false),
    ))
    lazy val schemaT: ObjectType[Schema] = ObjectType[Schema]("__Schema")(List(
      Field("description", Nullable(Scalar.string))(_.description),
      Field("types", ListOf(typeT))(_ => index.names.map(TypeRef.Of(_))),
      Field("queryType", typeT)(s => TypeRef.Of(s.query.name)),
      Field("mutationType", Nullable(typeT))(_.mutation.map(m => TypeRef.Of(m.name))),
      Field("subscriptionType", Nullable(typeT))(_.subscription.map(s => TypeRef.Of(s.name))),
      Field("directives", ListOf(directiveT))(_ => directives),
    ))
    val name = Arg("name", InScalar.string)
    List(
      Field[Unit, Schema]("__schema", schemaT)(_ => schema),
      Field.fetch[Unit, Option[TypeRef]]("__type", Nullable(typeT), name)((_, a) =>
        pure(Option.when(index.outputs.contains(a(name)) || index.input(a(name)).isDefined)(TypeRef.Of(a(name))))),
    )
