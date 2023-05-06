namespace Atla.Parser

type SourceInput(Lines: string list, Line: int, Col: int) = 
    inherit Input<SourceChar>()

    override this.Get() =
        if ((Line < 0 || Lines.Length - 1 < Line) || Lines[Line].Length < Col) then 
            None
        else
            if Col = Lines[Line].Length then
                Some({ SourceChar.Char='\n'; Pos={ Line=Line; Col=Col }})
            else
                Some({ SourceChar.Char=Lines[Line][Col]; Pos={ Line=Line; Col=Col }})

    override this.Next() =
        if Col < Lines[Line].Length - 1 then
            SourceInput(Lines, Line, Col + 1)
        elif Col = Lines[Line].Length - 1 then
            SourceInput(Lines, Line, Col + 1)
        else
            SourceInput(Lines, Line + 1, 0)

    override this.WhereIs() =
        { Position.Line=Line; Col=Col }