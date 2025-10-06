module AST

type Ops = Add | Sub | Mul | Div

type Expr =
    | Number of int
    | Variable of string
    | BinaryOp of Expr * Ops * Expr


type isAsciiOutput = bool


type Statement =
    | VariableAssignment of string * Expr
    | Sleep of int
    | WhileStatement of Block
    | IfStatement of string * Block
    | Output of string * isAsciiOutput
    | Break
    | Return of string
and Block = Statement list