namespace Atla.Compiler.Hir

[<CustomEquality; NoComparison>]
type Value =
    | Unit
    | Int of int
    | Float of float
    | String of string
    | Function of (Value list -> Value)
    | Data of Map<string, Value>
    | Native of obj // for interop with .NET objects

    interface System.IEquatable<Value> with
        member this.Equals(other: Value) : bool =
            let rec eq (v1: Value) (v2: Value) : bool =
                match v1, v2 with
                | Unit, Unit -> true
                | Int a, Int b -> a = b
                | Float a, Float b -> a = b
                | String a, String b -> a = b
                | Function _, Function _ -> failwith "Cannot compare function values for equality"
                | Data a, Data b ->
                    if (Map.count a) <> (Map.count b) then false
                    else
                        // ensure every key in a exists in b and values are equal
                        Map.forall (fun k va ->
                            match Map.tryFind k b with
                            | Some vb -> eq va vb
                            | None -> false
                        ) a
                | _ -> false

            eq this other

    override this.Equals(obj: obj) =
        match obj with
        | :? Value as other -> (this :> System.IEquatable<Value>).Equals(other)
        | _ -> false

    override this.GetHashCode() =
        match this with
        | Unit -> 0
        | Int v -> hash v
        | Float v -> hash v
        | String s -> hash s
        | Function _ -> raise (System.InvalidOperationException("Cannot compute hash code for function values"))
        | Data m ->
            // combine hashes of entries (order-independent)
            m |> Map.toSeq |> Seq.map (fun (k, v) -> hash k ^^^ (v.GetHashCode())) |> Seq.fold (fun acc h -> acc ^^^ h) 0
