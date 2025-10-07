module Parser

open AST
open FParsec
open System

let trim (s:string) = s.Trim()

// Forward reference for statement parser
let statement, statementRef = createParserForwardedToRef<Statement, unit>()

// Helper parsers
let comment = pstring "호호" >>. skipRestOfLine true
let ws = spaces >>. optional comment >>. spaces
let pnumber = pint32 .>> ws

// Identifier parser
let identifierIndependent =
    many1Satisfy (fun c -> c <> '?' && c <> '~' && not (Char.IsWhiteSpace c))

// Variable declaration parser: "알로하~ 원준님"
let varDeclare =
    parse {
        let! _ = pstring "알로하" .>> ws
        let! tildes = many (pchar '~')
        let! _ = ws
        let! var = identifierIndependent
        let! _ = ws
        // Count the number of tildes but ignore it (default is 0)
        return VariableAssignment(var, Number(0))
    }

// Variable assignment negative: "원준님 너무한거 아니에요?"
let varAssignNegative =
    parse {
        let! var = identifierIndependent
        let! _ = ws
        let! _ = pstring "너무한거 아니에요?" .>> ws
        // This assigns -variable to variable
        return VariableAssignment(var, BinaryOp(Number(0), Ops.Sub, Variable(var)))
    }

// Calculation statement: "저 필요하신분~~~ 원준님?"
let calculation =
    parse {
        let! _ = pstring "저 필요하신분" .>> ws
        let! tildes = many (pchar '~')
        let! _ = spaces
        let! var = identifierIndependent
        let! questions = many (pchar '?')
        let! _ = ws
        
        // Calculate: var += (number of ~) - (number of ?)
        let addValue = tildes.Length
        let subValue = questions.Length
        
        return VariableAssignment(var, 
                BinaryOp(
                    Variable(var),
                    Ops.Add,
                    BinaryOp(
                        Number(addValue),
                        Ops.Sub,
                        Number(subValue)
                    )
                )
            )
    }

// Output statement: "원준님 요즘 어떠신가요?" (number) or "원준님 요즘 어떠신가요??" (ASCII)
let output =
    parse {
        let! var = identifierIndependent
        let! _ = ws
        let! _ = pstring "요즘 어떠신가요" .>> ws
        let! questionMarks = many (pchar '?')
        let! _ = ws
        
        return Output(var, questionMarks.Length >= 2)
    }

// Return statement: "원준님 오~ 너무 좋습니다."
let returnValue =
    parse {
        let! var = identifierIndependent
        let! _ = ws
        let! _ = pstring "오" .>> ws
        let! _ = many (pchar '~') .>> ws
        let! _ = pstring "너무 좋습니다." .>> ws
        return Return(var)
    }

// Break statement: "여러분~, 먼저 들어가보겠습니다"
let breakSign = 
    (pstring "여러분" .>> many (pchar '~') .>> pstring ", 먼저 들어가보겠습니다" .>> ws )
    <|>
    (pstring "제가 가서 대가리 박을게요" .>> ws )
    >>% Break

// IF: "원준님 말이 되나요?" ... "ㅎㅎㅎㅎ 개웃김."
let ifBlock =
    parse {
        let! var = identifierIndependent
        let! _ = ws
        let! _ = pstring "말이 되나요?" .>> ws

        let statementWithBreakSign = attempt breakSign <|> statement

        let! block = many (ws >>. statementWithBreakSign .>> optional newline)
        let! _ = pstring "ㅎㅎㅎㅎ 개웃김." .>> ws
        return IfStatement (var, block)
    }


let whileBlock =
    parse {
        let! _ = pstring "집가서 더 볼게요" .>> ws
        let! _ = many (pchar '~') .>> ws
        
        let statementWithBreakSign = attempt breakSign <|> statement

        let! block = many (ws >>. statementWithBreakSign .>> optional newline)
        let! _ = pstring "ㅎㅎㅎㅎ 개웃김." .>> ws
        
        return WhileStatement(block)
    }


let sleep =
    parse {
        let! counts = many1 (pstring "스탠덥")
        do! ws
        let! _ = pstring "스탠덥하시죠~" .>> ws
        return Sleep(counts.Length)
    }

// Register all statement types
do statementRef := choice [
        attempt ifBlock
        attempt whileBlock
        attempt varDeclare
        attempt calculation
        attempt varAssignNegative
        attempt sleep
        attempt output
        attempt returnValue
        ]

// Parser for the entire program
let program =
    ws >>. many (statement .>> optional newline) .>> eof



