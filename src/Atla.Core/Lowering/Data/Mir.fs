namespace Atla.Core.Lowering.Data

open System
open System.Reflection
open System.Reflection.Emit
open Atla.Core.Semantics.Data

// MIRでは
// - 型はTypeMetaを除去済み
// - 変数名をインデックスに変換済み
module Mir =
    // Immediate values in MIR
    type Imm =
        | Bool of bool
        | Int of int
        | Float of float
        | String of string
        | Null
        override this.ToString() =
            match this with
            | Bool v -> sprintf "Bool(%b)" v
            | Int v -> sprintf "Int(%d)" v
            | Float v -> sprintf "Float(%f)" v
            | String s -> sprintf "String(%s)" s
            | Null -> "Null"

    type Reg =
        | Loc of int
        | Arg of int
        override this.ToString() =
            match this with
            | Loc index -> sprintf "Loc(%d)" index
            | Arg index -> sprintf "Arg(%d)" index

    /// MIR メソッドフレームのレジスタ割り当てを保持する不変レコード。
    /// 変数シンボル ID からレジスタ（引数/ローカル変数）および型へのマッピングを管理する。
    type Frame = {
        args: Map<SymbolId, Reg>
        locs: Map<SymbolId, Reg>
        argTypes: Map<SymbolId, TypeId>
        locTypes: Map<SymbolId, TypeId>
    }

    /// Frame の構築・検索を行う純粋関数群。
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Frame =
        /// 空フレームを返す。
        let empty: Frame = {
            args = Map.empty
            locs = Map.empty
            argTypes = Map.empty
            locTypes = Map.empty
        }

        /// 引数シンボルを次の引数インデックスに割り当て、登録済みレジスタと更新フレームを返す。
        let addArg (sid: SymbolId) (tid: TypeId) (frame: Frame) : Reg * Frame =
            let reg = Reg.Arg(frame.args.Count)
            let newFrame = {
                frame with
                    args = frame.args |> Map.add sid reg
                    argTypes = frame.argTypes |> Map.add sid tid
            }
            reg, newFrame

        /// ローカル変数シンボルを次のローカルインデックスに割り当て、登録済みレジスタと更新フレームを返す。
        let addLoc (sid: SymbolId) (tid: TypeId) (frame: Frame) : Reg * Frame =
            let reg = Reg.Loc(frame.locs.Count)
            let newFrame = {
                frame with
                    locs = frame.locs |> Map.add sid reg
                    locTypes = frame.locTypes |> Map.add sid tid
            }
            reg, newFrame

        /// シンボル ID に対応するレジスタを検索する（引数を優先）。
        let get (sid: SymbolId) (frame: Frame) : Reg option =
            match frame.args |> Map.tryFind sid with
            | Some reg -> Some reg
            | None -> frame.locs |> Map.tryFind sid

    // Values in MIR
    type Value =
        | ImmVal of Imm
        | RegVal of Reg
        | FieldVal of field: FieldInfo
        | MethodVal of method: MethodInfo
        // 関数シンボルを .NET デリゲートとして生成する値。
        // targetReg=None:   ldnull;            ldftn <sid>; newobj <delegateType>::.ctor
        // targetReg=Some r: ldarg/ldloc <r>;   ldftn <sid>; newobj <delegateType>::.ctor
        | FnDelegate of sid: SymbolId * delegateType: System.Type * targetReg: Reg option
        override this.ToString() =
            match this with
            | ImmVal v -> sprintf "Imm(%s)" (v.ToString())
            | RegVal v -> sprintf "Reg(%s)" (v.ToString())
            | FieldVal (fi) -> sprintf "Field(%A)" fi
            | MethodVal (mi) -> sprintf "Method(%A)" mi
            | FnDelegate (sid, dt, targetReg) ->
                let targetText =
                    targetReg
                    |> Option.map (fun reg -> reg.ToString())
                    |> Option.defaultValue "null"
                sprintf "FnDelegate(sid=%d, type=%s, target=%s)" sid.id dt.FullName targetText

    type OpCode =
        | Add
        | Sub
        | Mul
        | Div
        | Mod
        | Or
        | And
        | Eq

    /// 分岐先ラベルの識別子。Layout フェーズで連番で払い出し、Gen フェーズで ILGenerator.Label へ解決する。
    [<Struct>]
    type LabelId = LabelId of int
        with override this.ToString() = let (LabelId id) = this in sprintf "Label(%d)" id

    // Instructions
    type Ins =
        | Assign of dest: Reg * value: Value
        | AssignField of inst: Reg * field: FieldInfo * value: Value
        | TAC of dest: Reg * lhs: Value * op: OpCode * rhs: Value
        | Call of method: Choice<MethodInfo, ConstructorInfo> * args: Value list
        | CallSym of sid: SymbolId * args: Value list
        | CallAssign of dst: Reg * method: MethodInfo * args: Value list
        | CallAssignSym of dst: Reg * sid: SymbolId * args: Value list
        | New of dst: Reg * ctor: ConstructorInfo * args: Value list
        // env-class インスタンスを新規生成する（デフォルトコンストラクタ使用。typeSid で型を SymbolId 解決する）。
        | NewEnv of dst: Reg * typeSid: SymbolId
        // env-class インスタンスのフィールドへ値を書き込む（typeSid・fieldSid で解決）。
        | StoreEnvField of inst: Reg * typeSid: SymbolId * fieldSid: SymbolId * value: Value
        // env-class インスタンスのフィールドから値を読み込む（typeSid・fieldSid で解決）。
        | LoadEnvField of dst: Reg * inst: Reg * typeSid: SymbolId * fieldSid: SymbolId
        | Ret
        | RetValue of value: Value
        | Jump of label: LabelId
        | JumpTrue of value: Value * label: LabelId
        | JumpFalse of value: Value * label: LabelId
        | MarkLabel of label: LabelId
        | Try of body: Ins list * finallyBody: Ins list
        override this.ToString() =
            match this with
            | Assign(name, value) -> sprintf "%A = %s" name (value.ToString())
            | AssignField(inst, field, value) -> sprintf "%A.%s = %s" inst field.Name (value.ToString())
            | TAC(dest, lhs, op, rhs) -> sprintf "%A = %s %A %s" dest (lhs.ToString()) op (rhs.ToString())
            | Call(method, args) -> sprintf "%A(%s)" method (String.Join(", ", args |> List.map (fun a -> a.ToString())))
            | CallSym(sid, args) -> sprintf "call sid=%d (%s)" sid.id (String.Join(", ", args |> List.map (fun a -> a.ToString())))
            | CallAssign(dst, method, args) -> sprintf "%A = %A(%s)" dst method (String.Join(", ", args |> List.map (fun a -> a.ToString())))
            | CallAssignSym(dst, sid, args) -> sprintf "%A = sid:%d(%s)" dst sid.id (String.Join(", ", args |> List.map (fun a -> a.ToString())))
            | New(dst, ctor, args) -> sprintf "%A = %A(%s)" dst ctor (String.Join(", ", args |> List.map (fun a -> a.ToString())))
            | NewEnv(dst, typeSid) -> sprintf "%A = new_env(typeSid=%d)" dst typeSid.id
            | StoreEnvField(inst, typeSid, fieldSid, value) -> sprintf "%A.field_%d(typeSid=%d) = %s" inst fieldSid.id typeSid.id (value.ToString())
            | LoadEnvField(dst, inst, typeSid, fieldSid) -> sprintf "%A = %A.field_%d(typeSid=%d)" dst inst fieldSid.id typeSid.id
            | Ret -> "Ret"
            | RetValue v -> sprintf "return %s" (v.ToString())
            | Jump label -> sprintf "Jump %s" (label.ToString())
            | JumpTrue(v, label) -> sprintf "JumpTrue %s %s" (v.ToString()) (label.ToString())
            | JumpFalse(v, label) -> sprintf "JumpFalse %s %s" (v.ToString()) (label.ToString())
            | MarkLabel label -> sprintf "MarkLabel %s" (label.ToString())
            | Try(body, finallyBody) -> sprintf "Try(body=%d, finally=%d)" (List.length body) (List.length finallyBody)

    // Convenience wrapper for fields in generated types
    type Field(sid: SymbolId, tid: TypeId) =
        let mutable _builder: FieldBuilder option = None
        member this.sym = sid
        member this.typ = tid
        member this.builder
            with get() = _builder.Value
            and set(v) = _builder <- Some v

    type Constructor(args: TypeId list, body: Ins list, frame: Frame) =
        let mutable _builder: ConstructorBuilder option = None
        member this.args = args
        member this.body = body
        member this.frame = frame
        member this.builder
            with get() = _builder.Value
            and set(v) = _builder <- Some v

    type Method(name: string, sid: SymbolId, args: TypeId list, ret: TypeId, body: Ins list, frame: Frame) =
        let mutable _builder: MethodBuilder option = None
        member this.name = name
        member this.sym = sid
        member this.args = args
        member this.ret = ret
        member this.body = body
        member this.frame = frame
        member this.builder
            with get() = _builder.Value
            and set(v) = _builder <- Some v

    type Type(name: string, sid: SymbolId, baseType: TypeId option, fields: Field list, ctors: Constructor list, methods: Method list) =
        let mutable _builder: TypeBuilder option = None
        member this.name = name
        member this.sym = sid
        member this.baseType = baseType
        member this.fields = fields
        member this.ctors = ctors
        member this.methods = methods
        member this.builder
            with get() = _builder.Value
            and set(v) = _builder <- Some v

    type Module(name: string, types: Type list, methods: Method list) =
        let mutable _builder: ModuleBuilder option = None
        member this.name = name
        member this.types = types
        member this.methods = methods
        member this.builder
            with get() = _builder.Value
            and set(v) = _builder <- Some v

    type Assembly(name: string, modules: Module list) =
        let mutable _builder: PersistedAssemblyBuilder option = None
        member this.name = name
        member this.modules = modules
        member this.builder
            with get() = _builder.Value
            and set(v) = _builder <- Some v
