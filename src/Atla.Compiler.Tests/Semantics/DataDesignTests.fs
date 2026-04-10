namespace Atla.Compiler.Tests.Semantics

open Xunit
open Atla.Compiler.Semantics.Data

module DataDesignTests =
    [<Fact>]
    let ``SymbolId should use value equality`` () =
        let table = SymbolTable()
        let sid1 = table.NextId()
        let sid2 = SymbolId sid1.id
        Assert.Equal(sid1, sid2)

        let info = { name = "x"; typ = TypeId.Int; kind = SymbolKind.Local() }
        table.Add(sid1, info)

        match table.Get(sid2) with
        | Some resolved -> Assert.Equal("x", resolved.name)
        | None -> Assert.True(false, "value-equal SymbolId で取得できるべきです。")

    [<Fact>]
    let ``Type unify should return Result.Error for incompatible function arity`` () =
        let subst = TypeSubst()
        let left = TypeId.Fn([ TypeId.Int ], TypeId.Int)
        let right = TypeId.Fn([ TypeId.Int; TypeId.Int ], TypeId.Int)

        match Type.unify subst left right with
        | Result.Error (DifferentFunctionArity(1, 2)) -> Assert.True(true)
        | Result.Error other -> Assert.True(false, $"unexpected error: {other}")
        | Result.Ok _ -> Assert.True(false, "arity mismatch は失敗すべきです。")

    [<Fact>]
    let ``TypeMetaFactory should be local to each factory instance`` () =
        let f1 = TypeMetaFactory()
        let f2 = TypeMetaFactory()

        let m1 = f1.Fresh()
        let m2 = f2.Fresh()

        Assert.Equal(m1, m2)
