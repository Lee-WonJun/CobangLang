module Tests

open System
open Xunit
open FParsec
open Parser
open Interpreter

let parse code =
    match run program code with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg

let run code =
    let ast = parse code
    Async.RunSynchronously (interpretAsync ast (fun _ -> ()))

[<Fact>]
let ``변수 선언 - 알로하~ 변수명`` () =
    let state = run "알로하~ 원준님"
    Assert.Equal(0, state.Variables.["원준님"])

[<Fact>]
let ``변수 증감 - 저 필요하신분~~~ 변수명?`` () =
    let state = run "알로하~ 원준님\n저 필요하신분~~~ 원준님?"
    Assert.Equal(2, state.Variables.["원준님"])

[<Fact>]
let ``변수 부호 반전 - 변수명 너무한거 아니에요?`` () =
    let state = run "알로하~ 원준님\n저 필요하신분~~~~~ 원준님\n원준님 너무한거 아니에요?"
    Assert.Equal(-5, state.Variables.["원준님"])

[<Fact>]
let ``숫자 출력 - 변수명 요즘 어떠신가요?`` () =
    let state = run "알로하~ 원준님\n저 필요하신분~~~~~~~~~~ 원준님\n원준님 요즘 어떠신가요?"
    Assert.Equal("10", state.StandardOutput)

[<Fact>]
let ``ASCII 출력 - 변수명 요즘 어떠신가요??`` () =
    let state = run "알로하~ 원준님\n저 필요하신분~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 원준님\n원준님 요즘 어떠신가요??"
    Assert.Equal("A", state.StandardOutput)

[<Fact>]
let ``if문 - 변수명 말이 되나요?`` () =
    let state = run "알로하~ 원준님\n저 필요하신분~~~ 원준님\n원준님 말이 되나요?\n저 필요하신분~~~~~ 원준님\nㅎㅎㅎㅎ 개웃김."
    Assert.Equal(8, state.Variables.["원준님"])

[<Fact>]
let ``if문 조건 거짓`` () =
    let state = run "알로하~ 원준님\n원준님 말이 되나요?\n저 필요하신분~~~~~ 원준님\nㅎㅎㅎㅎ 개웃김."
    Assert.Equal(0, state.Variables.["원준님"])

[<Fact>]
let ``while문과 break - 집가서 더 볼게요~~~`` () =
    let state = run "알로하~ 카운터\n집가서 더 볼게요~~~\n저 필요하신분~ 카운터\n카운터 말이 되나요?\n여러분~, 먼저 들어가보겠습니다\nㅎㅎㅎㅎ 개웃김.\nㅎㅎㅎㅎ 개웃김."
    Assert.Equal(1, state.Variables.["카운터"])

[<Fact>]
let ``주석 - 호호`` () =
    let state = run "알로하~ 원준님\n호호 이건 주석입니다\n저 필요하신분~~~ 원준님"
    Assert.Equal(3, state.Variables.["원준님"])

