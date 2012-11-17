//
// Copyright (c) 2008, Brian Frank and Andy Frank
// Licensed under the Academic Free License version 3.0
//
// History:
//   17 Nov 08  Brian Frank  Creation
//

using compiler

**
** JavaBridge is the compiler plugin for bringing Java
** classes into the Fantom type system.
**
class JavaBridge : CBridge
{

//////////////////////////////////////////////////////////////////////////
// Constructor
//////////////////////////////////////////////////////////////////////////

  **
  ** Construct a JavaBridge for current environment
  **
  new make(Compiler c, ClassPath cp := ClassPath.makeForCurrent)
    : super(c)
  {
    this.cp = cp
  }

//////////////////////////////////////////////////////////////////////////
// Namespace
//////////////////////////////////////////////////////////////////////////

  **
  ** Map a FFI "podName" to a Java package.
  **
  override CPod resolvePod(Str name, Loc? loc)
  {
    // the empty package is used to represent primitives
    if (name == "") return primitives

    // look for package name in classpatch
    classes := cp.classes[name]
    if (classes == null)
      throw CompilerErr("Java package '$name' not found", loc)

    // map package to JavaPod
    return JavaPod(this, name, classes)
  }

  **
  ** Map class meta-data and Java members to Fantom slots
  ** for the specified JavaType.
  **
  virtual Void loadType(JavaType type, Str:CSlot slots)
  {
    JavaReflect.loadType(type, slots)
  }

//////////////////////////////////////////////////////////////////////////
// Call Resolution
//////////////////////////////////////////////////////////////////////////

  **
  ** Resolve a construction call to a Java constructor.
  **
  override Expr resolveConstruction(CallExpr call)
  {
    // if the last argument is an it-block, then we know
    // right away that we will not be passing it thru to Java,
    // so strip it off to be appended as call to Obj.with
    itBlock := call.args.last as ClosureExpr
    if (itBlock != null && itBlock.isItBlock)
      call.args.removeAt(-1)
    else
      itBlock = null

    // if this is an interop array like IntArray/int[] use make
    // factory otherwise look for Java constructor called <init>
    JavaType base := call.target.ctype
    if (base.isInteropArray)
      call.method = base.method("make")
    else
      call.method = base.method("<init>")

    // call resolution to deal with overloading
    call = resolveCall(call)

    // we need to create an implicit target for the Java runtime
    // to perform the new opcode to ensure it is on the stack
    // before the args (we don't do this for interop Array classes)
    if (!base.isInteropArray)
    {
      loc := call.loc
      call.target = CallExpr.makeWithMethod(loc, null, base.newMethod) { synthetic=true }
    }

    // if we stripped an it-block argument,
    // add it as trailing call to Obj.with
    if (itBlock != null) return itBlock.toWith(call)
    return call
  }

  **
  ** Resolve a construction chain call where a Fantom constructor
  ** calls the super-class constructor.  Type check the arguments
  ** and insert any conversions needed.
  **
  override Expr resolveConstructorChain(CallExpr call)
  {
    // we don't allow chaining to a this ctor for Java FFI
    if (call.target.id !== ExprId.superExpr)
      throw err("Must use super constructor call in Java FFI", call.loc)

    // route to a superclass constructor
    JavaType base := call.target.ctype.deref
    call.method = base.method("<init>")

    // call resolution to deal with overloading
    return resolveCall(call)
  }

  **
  ** Given a dot operator slot access on the given foreign
  ** base type, determine the appopriate slot to use based on
  ** whether parens were used
  **   base.name    =>  noParens = true
  **   base.name()  =>  noParens = false
  **
  ** In Java a given name could be bound to both a field and
  ** a method.  In this case we only resolve the field if
  ** no parens are used.  We also handle the special case of
  ** Java annotations here because their element methods are
  ** also mapped as Fantom fields (instance based mixin field).
  **
  override CSlot? resolveSlotAccess(CType base, Str name, Bool noParens)
  {
    // first try to resolve as a field
    field := base.field(name)
    if (field != null)
    {
      // if no () we used and this isn't an annotation field
      if (noParens && (field.isStatic || !base.isMixin))
        return field

      // if we did find a field, then make sure we use that
      // field's parent type to resolve a method (becuase the
      // base type might be a sub-class of a Java type in which
      // case it is unware of field/method overloads)
      return field.parent.method(name)
    }

    // lookup method
    return base.method(name)
  }

  **
  ** Resolve a method call: try to find the best match
  ** and apply any coercions needed.
  **
  override CallExpr resolveCall(CallExpr call)
  {
    // try to match against all the overloaded methods
    matches := CallMatch[,]
    CMethod? m := call.method
    while (m != null)
    {
      match := matchCall(call, m)
      if (match != null) matches.add(match)
      m = m is JavaMethod ? ((JavaMethod)m).next : null
    }

    // if we have exactly one match use then use that one
    if (matches.size == 1) return matches[0].apply(call)

    // if we have multiple matches; resolve to
    // most specific match according to JLS rules
    // TODO: this does not correct resolve when using Fantom implicit casting
    if (matches.size > 1)
    {
      best := resolveMostSpecific(matches)
      if (best != null) return best.apply(call)
    }

    // zero or multiple ambiguous matches is a compiler error
    s := StrBuf()
    s.add(matches.isEmpty ? "Invalid args " : "Ambiguous call ")
    s.add(call.name).add("(")
    s.add(call.args.join(", ") |Expr arg->Str| { return arg.toTypeStr })
    s.add(")")
    throw err(s.toStr, call.loc)
  }

  **
  ** Check if the call matches the specified overload method.
  ** If so return method and coerced args otherwise return null.
  **
  internal CallMatch? matchCall(CallExpr call, CMethod m)
  {
    // first check if have matching numbers of args and params
    args := call.args
    if (m.params.size < args.size) return null

    // check if each argument is ok or can be coerced
    isErr := false
    newArgs := args.dup
    m.params.each |CParam p, Int i|
    {
      if (i >= args.size)
      {
        // param has a default value, then that is ok
        if (!p.hasDefault) isErr = true
      }
      else
      {
        // ensure arg fits parameter type (or auto-cast)
        newArgs[i] = coerce(args[i], p.paramType) |->| { isErr = true }
      }
    }
    if (isErr) return null
    return CallMatch { it.method = m; it.args = newArgs }
  }

  **
  ** Given a list of overloaed methods find the most specific method
  ** according to Java Language Specification 15.11.2.2.  The "informal
  ** intuition" rule is that a method is more specific than another
  ** if the first could be could be passed onto the second one.
  **
  internal static CallMatch? resolveMostSpecific(CallMatch[] matches)
  {
    CallMatch? best := matches[0]
    for (i:=1; i<matches.size; ++i)
    {
      x := matches[i]
      if (isMoreSpecific(best, x)) { continue }
      if (isMoreSpecific(x, best)) { best = x; continue }
      return null
    }
    return best
  }

  **
  ** Is 'a' more specific than 'b' such that 'a' could be used
  ** passed to 'b' without a compile time error.
  **
  internal static Bool isMoreSpecific(CallMatch a, CallMatch b)
  {
    return a.method.params.all |CParam ap, Int i->Bool|
    {
      bp := b.method.params[i]
      return ap.paramType.fits(bp.paramType)
    }
  }

//////////////////////////////////////////////////////////////////////////
// Overrides
//////////////////////////////////////////////////////////////////////////

  **
  ** Called during Inherit step when a Fantom slot overrides a FFI slot.
  ** Log and throw compiler error if there is a problem.
  **
  override Void checkOverride(TypeDef t, CSlot base, SlotDef def)
  {
    // we don't allow Fantom to override Java methods with multiple
    // overloaded versions since the Fantom type system can't actually
    // override all the overloaded versions
    jslot := base as JavaSlot
    if (jslot?.next != null)
      throw err("Cannot override Java overloaded method: '$jslot.name'", def.loc)

    // route to method override checking
    if (base is JavaMethod && def is MethodDef)
      checkMethodOverride(t, base, def)
  }

  **
  ** Called on method/method overrides in the checkOverride callback.
  **
  private Void checkMethodOverride(TypeDef t, JavaMethod base, MethodDef def)
  {
    // bail early if we know things aren't going to work out
    if (base.params.size != def.params.size) return

    // if the return type is primitive or Java array and the
    // Fantom declaration matches how it is inferred into the Fan
    // type system, then just change the return type - the compiler
    // will impliclty do all the return coercions
    if (isOverrideInferredType(base.returnType, def.returnType))
    {
      def.ret = def.inheritedRet = base.returnType
    }

    // if any of the parameters is a primitive or Java array
    // and the Fantom declaration matches how it is inferred into
    // the Fantom type type, then change the parameter type to
    // the Java override type and make the Fantom type a local
    // variable:
    //   Java:   void foo(int a) { ... }
    //   Fantom: Void foo(Int a) { ... }
    //   Result: Void foo(int a_$J) { Int a := a_$J; ... }
    //
    base.params.eachr |CParam bp, Int i|
    {
      dp := def.paramDefs[i]
      if (!isOverrideInferredType(bp.paramType, dp.paramType)) return

      // add local variable: Int bar := bar_$J
      local := LocalDefStmt(def.loc)
      local.ctype = dp.paramType
      local.name  = dp.name
      local.init  = UnknownVarExpr(def.loc, null, dp.name + "_\$J")
      def.code.stmts.insert(0, local)

      // rename parameter Int bar -> int bar_$J
      dp.name = dp.name + "_\$J"
      dp.paramType = bp.paramType
    }
  }

  **
  ** When overriding a Java method check if the base type is
  ** is a Java primitive or array and the override definition is
  ** matches how the Java type is inferred in the Fantom type system.
  ** If we have a match return true and we'll swizzle things in
  ** checkMethodOverride.
  **
  static private Bool isOverrideInferredType(CType base, CType def)
  {
    // check if base class slot is a JavaType
    java := base.toNonNullable as JavaType
    if (java != null)
    {
      // allow primitives is it matches the inferred type
      if (java.isPrimitive) return java.inferredAs == def

      // allow arrays if mapped as Foo[] -> Foo?[]?
      if (java.isArray) return java.inferredAs == def.toNonNullable && def.isNullable
    }
    return false
  }

//////////////////////////////////////////////////////////////////////////
// CheckErrors
//////////////////////////////////////////////////////////////////////////

  **
  ** Called during CheckErrors step for a type which extends
  ** a FFI class or implements any FFI mixins.
  **
  override Void checkType(TypeDef def)
  {
    // can't subclass a primitive array like ByteArray/byte[]
    if (def.base.deref is JavaType && def.base.deref->isInteropArray)
    {
      err("Cannot subclass from Java interop array: $def.base", def.loc)
      return
    }

    // we don't allow deep inheritance of Java classes because
    // the Fantom constructor and Java constructor model don't match
    // up past one level of inheritance
    // NOTE: that that when we remove this restriction we need to
    // test how field initialization works because instance$init
    // is almost certain to break with the current emit design
    javaBase := def.base
    while (javaBase != null && !javaBase.isForeign) javaBase = javaBase.base
    if (javaBase != null && javaBase !== def.base)
    {
      err("Cannot subclass Java class more than one level: $javaBase", def.loc)
      return
    }

    // ensure that when we map Fantom constructors to Java
    // constructors that we don't have duplicate signatures
    ctors := def.ctorDefs
    ctors.each |MethodDef a, Int i|
    {
      ctors.each |MethodDef b, Int j|
      {
        if (i > j && areParamsSame(a, b))
          err("Duplicate Java FFI constructor signatures: '$b.name' and '$a.name'", a.loc)
      }
    }
  }

  **
  ** Do the two methods have the exact same parameter types.
  **
  static Bool areParamsSame(CMethod a, CMethod b)
  {
    if (a.params.size != b.params.size) return false
    for (i:=0; i<a.params.size; ++i)
    {
      if (a.params[i].paramType != b.params[i].paramType)
        return false
    }
    return true
  }

//////////////////////////////////////////////////////////////////////////
// Coercion
//////////////////////////////////////////////////////////////////////////

  **
  ** Return if we can make the actual type fit the expected
  ** type, potentially using a coercion.
  **
  Bool fits(CType actual, CType expected)
  {
    // use dummy expression and route to coerce code
    dummy := UnknownVarExpr(Loc("dummy"), null, "dummy") { ctype = actual }
    fits := true
    coerce(dummy, expected) |->| { fits=false }
    return fits
  }

  **
  ** Coerce expression to expected type.  If not a type match
  ** then run the onErr function.
  **
  override Expr coerce(Expr expr, CType expected, |->| onErr)
  {
    // handle easy case
    actual := expr.ctype
    expected = expected.deref
    if (actual == expected) return expr

    // handle null literal
    if (expr.id === ExprId.nullLiteral && expected.isNullable)
      return expr

    // handle Fantom to Java primitives
    if (expected.pod == primitives)
      return coerceToPrimitive(expr, expected, onErr)

    // handle Java primitives to Fan
    if (actual.pod == primitives)
      return coerceFromPrimitive(expr, expected, onErr)

    // handle Java array to Fantom list
    if (actual.name[0] == '[')
      return coerceFromArray(expr, expected, onErr)

    // handle Fantom list to Java array
    if (expected.name[0] == '[')
      return coerceToArray(expr, expected, onErr)

    // handle sys::Func -> Java interface
    if (actual is FuncType && expected.isMixin && expected.toNonNullable is JavaType)
      return coerceFuncToInterface(expr, expected.toNonNullable, onErr)

    // handle special classes and interfaces for built-in Fantom
    // classes which actually map directly to Java built-in types
    if (actual.isBool    && boolTypes.contains(expected.toNonNullable.signature)) return box(expr)
    if (actual.isInt     && intTypes.contains(expected.toNonNullable.signature)) return box(expr)
    if (actual.isFloat   && floatTypes.contains(expected.toNonNullable.signature)) return box(expr)
    if (actual.isDecimal && decimalTypes.contains(expected.toNonNullable.signature)) return expr
    if (actual.isStr     && strTypes.contains(expected.toNonNullable.signature)) return expr

     // use normal Fantom coercion behavior
    return super.coerce(expr, expected, onErr)
  }

  **
  ** Ensure value type is boxed.
  **
  private Expr box(Expr expr)
  {
    if (expr.ctype.isVal)
      return TypeCheckExpr.coerce(expr, expr.ctype.toNullable)
    else
      return expr
  }

  **
  ** Coerce a fan expression to a Java primitive (other
  ** than the ones we support natively)
  **
  Expr coerceToPrimitive(Expr expr, JavaType expected, |->| onErr)
  {
    actual := expr.ctype

    // sys::Int (long) -> int, short, byte
    if (actual.isInt && expected.isPrimitiveIntLike)
      return TypeCheckExpr.coerce(expr, expected)

    // sys::Float (double) -> float
    if (actual.isFloat && expected.isPrimitiveFloat)
      return TypeCheckExpr.coerce(expr, expected)

    // no coercion - type error
    onErr()
    return expr
  }

  **
  ** Coerce a Java primitive to a Fantom type.
  **
  Expr coerceFromPrimitive(Expr expr, CType expected, |->| onErr)
  {
    actual := (JavaType)expr.ctype

    // int, short, byte -> sys::Int (long)
    if (actual.isPrimitiveIntLike)
    {
      if (expected.isInt || expected.isObj)
        return TypeCheckExpr.coerce(expr, expected)
    }

    // float -> sys::Float (float)
    if (actual.isPrimitiveFloat)
    {
      if (expected.isFloat || expected.isObj)
        return TypeCheckExpr.coerce(expr, expected)
    }

    // no coercion - type error
    onErr()
    return expr
  }

  **
  ** Coerce a Java array to a Fantom list.
  **
  Expr coerceFromArray(Expr expr, CType expected, |->| onErr)
  {
    actual := (JavaType)expr.ctype.toNonNullable

    // if expected is array type
    if (expected is JavaType && ((JavaType)expected).isArray)
      if (actual.arrayOf.fits(((JavaType)expected).arrayOf)) return expr

    // if expected is Obj
    if (expected.isObj) return arrayToList(expr, actual.inferredArrayOf)

    // if expected is list type
    if (expected.toNonNullable is ListType)
    {
      expectedOf := ((ListType)expected.toNonNullable).v
      if (actual.inferredArrayOf.fits(expectedOf)) return arrayToList(expr, expectedOf)
    }

    // no coercion available
    onErr()
    return expr
  }

  **
  ** Generate List.make(of, expr) where expr is Object[]
  **
  private Expr arrayToList(Expr expr, CType of)
  {
    loc := expr.loc
    ofExpr := LiteralExpr(loc, ExprId.typeLiteral, ns.typeType, of)
    call := CallExpr.makeWithMethod(loc, null, listMakeFromArray, [ofExpr, expr])
    call.synthetic = true
    return call
  }

  **
  ** Coerce a Fantom list to Java array.
  **
  Expr coerceToArray(Expr expr, CType expected, |->| onErr)
  {
    loc := expr.loc
    expectedOf := ((JavaType)expected.toNonNullable).inferredArrayOf
    actual := expr.ctype

    // if actual is list type
    if (actual.toNonNullable is ListType)
    {
      actualOf := ((ListType)actual.toNonNullable).v
      if (actualOf.fits(expectedOf))
      {
        // (Foo[])list.asArray(cls)
        clsLiteral := CallExpr.makeWithMethod(loc, null, JavaType.classLiteral(this, expectedOf))
        asArray := CallExpr.makeWithMethod(loc, expr, listAsArray, [clsLiteral])
        return TypeCheckExpr.coerce(asArray, expected)
      }
    }

    // no coercion available
    onErr()
    return expr
  }

  **
  ** Attempt to coerce a parameterized sys::Func expr to a Java
  ** interface if the interface supports exactly one matching method.
  **
  Expr coerceFuncToInterface(Expr expr, JavaType expected, |->| onErr)
  {
    // check if we have exactly one abstract method in the expected type
    loc := expr.loc
    abstracts := expected.methods.findAll |CMethod m->Bool| { return m.isAbstract }
    if (abstracts.size != 1) { onErr(); return expr }
    method := abstracts.first

    // check if we have a match
    FuncType funcType := (FuncType)expr.ctype
    if (!isFuncToInterfaceMatch(funcType, method)) { onErr(); return expr }

    // check if we've already generated a wrapper for this combo
    key := "${funcType.signature}+${method.qname}"
    ctor := funcWrappers[key]
    if (ctor == null)
    {
      ctor = generateFuncToInterfaceWrapper(expr.loc, funcType, expected, method)
      funcWrappers[key] = ctor
    }

    // replace expr with FuncWrapperX(expr)
    call := CallExpr.makeWithMethod(loc, null, ctor, [expr])
    call.synthetic = true
    return call
  }

  **
  ** Return if the specified function type can be used to implement
  ** the specified interface method.
  **
  Bool isFuncToInterfaceMatch(FuncType funcType, CMethod method)
  {
    // sanity check to map to callX method - can't handle more than 8 args
    if (method.params.size > 8) return false

    // check if method is match for function; first check is that
    // method must supply all the arguments required by the function
    if (funcType.params.size > method.params.size) return false

    // check that func return type fits method return
    retOk := method.returnType.isVoid || fits(funcType.ret, method.returnType)
    if (!retOk) return false

    // check all the method parameters fit the function parameters
    paramsOk := funcType.params.all |CType f, Int i->Bool| { return fits(f, method.params[i].paramType) }
    if (!paramsOk) return false

    return true
  }

  **
  ** Generate the wrapper which implements the specified expected interface
  ** and overrides the specified method which calls the function.
  **
  CMethod generateFuncToInterfaceWrapper(Loc loc, FuncType funcType, CType expected, CMethod method)
  {
    //   Fantom: func typed as |Str|
    //   Java:   interface Foo { void bar(String) }
    //   Result: FuncWrapperX(func)
    //
    //   class FuncWrapperX : Foo
    //   {
    //     new make(Func f) { _func = f }
    //     override Void bar(Str a) { _func.call(a) }
    //     Func _func
    //   }

    // generate FuncWrapper class
    name := "FuncWrapper" + funcWrappers.size
    cls := TypeDef(ns, loc, compiler.types[0].unit, name, FConst.Internal + FConst.Synthetic)
    cls.base = ns.objType
    cls.mixins = [expected]
    addTypeDef(cls)

    // generate FuncWrapper._func field
    field := FieldDef(loc, cls)
    ((SlotDef)field).name = "_func"
    ((DefNode)field).flags = FConst.Private + FConst.Storage + FConst.Synthetic
    field.fieldType = funcType
    cls.addSlot(field)

    // generate FuncWrapper.make constructor
    ctor := MethodDef(loc, cls, "make", FConst.Internal + FConst.Ctor + FConst.Synthetic)
    ctor.ret  = ns.voidType
    ctor.paramDefs = [ParamDef(loc, funcType, "f")]
    ctor.code = Block.make(loc)
    ctor.code.stmts.add(BinaryExpr.makeAssign(
      FieldExpr(loc, ThisExpr(loc), field),
      UnknownVarExpr(loc, null, "f")).toStmt)
    ctor.code.stmts.add(ReturnStmt.make(loc))
    cls.addSlot(ctor)

    // generate FuncWrapper override of abstract method
    over := MethodDef(loc, cls, method.name, FConst.Public + FConst.Override + FConst.Synthetic)
    over.ret = method.returnType
    over.paramDefs = ParamDef[,]
    over.code = Block.make(loc)
    callArity := "call"
    call := CallExpr.makeWithMethod(loc, FieldExpr(loc, ThisExpr(loc), field), funcType.method(callArity))
    method.params.each |CParam param, Int i|
    {
      paramName := "p$i"
      over.params.add(ParamDef(loc, param.paramType, paramName))
      if (i < funcType.params.size)
        call.args.add(UnknownVarExpr(loc, null, paramName))
    }
    if (method.returnType.isVoid)
      over.code.stmts.add(call.toStmt).add(ReturnStmt(loc))
    else
      over.code.stmts.add(ReturnStmt(loc, call))
    cls.addSlot(over)

    // return the ctor which we use for coercion
    return ctor
  }

//////////////////////////////////////////////////////////////////////////
// Reflection
//////////////////////////////////////////////////////////////////////////

  **
  ** Get a CMethod representation for 'List.make(Type, Object[])'
  **
  once CMethod listMakeFromArray()
  {
    return JavaMethod(
      this.ns.listType,
      "make",
      FConst.Public + FConst.Static,
      this.ns.listType.toNullable,
      [
        JavaParam("of", this.ns.typeType),
        JavaParam("array", objectArrayType)
      ])
  }

  **
  ** Get a CMethod representation for 'Object[] List.asArray()'
  **
  once CMethod listAsArray()
  {
    return JavaMethod(
      this.ns.listType,
      "asArray",
      FConst.Public,
      objectArrayType,
      [JavaParam("cls", classType)])
  }

  **
  ** Get a CType representation for 'java.lang.Class'
  **
  once JavaType classType()
  {
    return ns.resolveType("[java]java.lang::Class")
  }

  **
  ** Get a CType representation for 'java.lang.Object[]'
  **
  once JavaType objectArrayType()
  {
    return ns.resolveType("[java]java.lang::[Object")
  }

//////////////////////////////////////////////////////////////////////////
// Fields
//////////////////////////////////////////////////////////////////////////

  const static Str[] boolTypes := Str[
    "[java]java.io::Serializable",
    "[java]java.lang::Comparable",
  ]

  const static Str[] intTypes := Str[
    "[java]java.lang::Number",
    "[java]java.io::Serializable",
    "[java]java.lang::Comparable",
  ]

  const static Str[] floatTypes := Str[
    "[java]java.lang::Number",
    "[java]java.io::Serializable",
    "[java]java.lang::Comparable",
  ]

  const static Str[] decimalTypes := Str[
    "[java]java.lang::Number",
    "[java]java.io::Serializable",
    "[java]java.lang::Comparable",
  ]

  const static Str[] strTypes := Str[
    "[java]java.io::Serializable",
    "[java]java.lang::CharSequence",
    "[java]java.lang::Comparable",
  ]

  JavaPrimitives primitives := JavaPrimitives(this)
  ClassPath cp

  private Str:CMethod funcWrappers := Str:CMethod[:]  // funcType+method:ctor

}

**************************************************************************
** CallMatch
**************************************************************************

internal class CallMatch
{
  CallExpr apply(CallExpr call)
  {
    call.args   = args
    call.method = method
    call.ctype  = method.isCtor ? method.parent : method.returnType
    return call
  }

  override Str toStr() { return method.signature }

  CMethod? method    // matched method
  Expr[]? args       // coerced arguments
}