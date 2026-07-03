open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open ColorPuzzle

type SolveRequest = { tubes: string[] }
type MoveDto = { count: int; color: string; fromTube: int; toTube: int }
type SolveResponse = { moveCount: int; moves: MoveDto[] }
type HintResponse = { hint: MoveDto; remainingMoves: int }

let validColorLetters = set ['B'; 'C'; 'G'; 'L'; 'M'; 'O'; 'P'; 'R'; 'W'; 'Y']

let validateRequest (request: SolveRequest) : Result<string[], string> =
    if isNull (box request) || isNull request.tubes then
        Error "Request body must contain a 'tubes' array."
    elif request.tubes.Length = 0 || request.tubes.Length > INITIAL_NON_EMPTY_COUNT then
        Error (sprintf "Expected between 1 and %d tubes; got %d." INITIAL_NON_EMPTY_COUNT request.tubes.Length)
    else
        let tubes = request.tubes |> Array.map (fun t -> (if isNull t then "" else t).Trim().ToUpperInvariant())
        let badTube = tubes |> Array.tryFind (fun t -> t.Length <> 4 || not (t |> Seq.forall validColorLetters.Contains))
        match badTube with
            | Some bad ->
                Error (sprintf "Tube '%s' is invalid: each tube must be exactly 4 letters from BCGLMOPRWY." bad)
            | None ->
                let unbalanced = tubes
                                    |> Seq.collect id
                                    |> Seq.countBy id
                                    |> Seq.tryFind (fun (_, occurrences) -> occurrences <> 4)
                match unbalanced with
                    | Some (letter, occurrences) ->
                        Error (sprintf "Each color must appear exactly 4 times; '%c' appears %d times." letter occurrences)
                    | None ->
                        Ok tubes

// A mid-game board: tubes may hold 0-4 letters, order is preserved (hint
// indices refer to the request's positions), and used colors still appear
// exactly 4 times each.
let validateHintRequest (request: SolveRequest) : Result<string[], string> =
    if isNull (box request) || isNull request.tubes then
        Error "Request body must contain a 'tubes' array."
    elif request.tubes.Length = 0 || request.tubes.Length > TUBE_COUNT then
        Error (sprintf "Expected between 1 and %d tubes; got %d." TUBE_COUNT request.tubes.Length)
    else
        let tubes = request.tubes |> Array.map (fun t -> (if isNull t then "" else t).Trim().ToUpperInvariant())
        let badTube = tubes |> Array.tryFind (fun t -> t.Length > 4 || not (t |> Seq.forall validColorLetters.Contains))
        match badTube with
            | Some bad ->
                Error (sprintf "Tube '%s' is invalid: each tube must be 0 to 4 letters from BCGLMOPRWY." bad)
            | None ->
                let unbalanced = tubes
                                    |> Seq.collect id
                                    |> Seq.countBy id
                                    |> Seq.tryFind (fun (_, occurrences) -> occurrences <> 4)
                match unbalanced with
                    | Some (letter, occurrences) ->
                        Error (sprintf "Each color must appear exactly 4 times; '%c' appears %d times." letter occurrences)
                    | None ->
                        Ok tubes

let hintHandler (request: SolveRequest) : IResult =
    match validateHintRequest request with
        | Error message ->
            Results.BadRequest {| error = message |}
        | Ok tubes ->
            match solveFromState tubes with
                | None ->
                    Results.UnprocessableEntity {| error = "No hint available — this position cannot be solved." |}
                | Some solved ->
                    match List.rev solved.moveList with
                        | [] ->
                            Results.UnprocessableEntity {| error = "The puzzle is already solved — no hint needed." |}
                        | MoveTubes (count, color, fromIndex, toIndex) :: _ ->
                            let hint = { count = count; color = string color; fromTube = fromIndex; toTube = toIndex }
                            Results.Ok { hint = hint; remainingMoves = solved.moveCount }

let solveHandler (request: SolveRequest) : IResult =
    match validateRequest request with
        | Error message ->
            Results.BadRequest {| error = message |}
        | Ok tubes ->
            match solvePuzzle tubes with
                | Some solved ->
                    let moves = solved.moveList
                                    |> List.rev
                                    |> List.map (fun (MoveTubes (count, color, fromIndex, toIndex)) ->
                                                    { count = count; color = string color; fromTube = fromIndex; toTube = toIndex })
                                    |> Array.ofList
                    Results.Ok { moveCount = solved.moveCount; moves = moves }
                | None ->
                    Results.UnprocessableEntity {| error = "This puzzle has no solution." |}

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)

    builder.Services.AddCors() |> ignore
    builder.Services.AddOpenApi() |> ignore

    let app = builder.Build()

    // Permissive for now; tighten to the React app's origin when it exists.
    app.UseCors(fun policy -> policy.AllowAnyOrigin().AllowAnyMethod().AllowAnyHeader() |> ignore) |> ignore

    app.MapOpenApi() |> ignore
    app.UseSwaggerUI(fun options -> options.SwaggerEndpoint("/openapi/v1.json", "ColorPuzzle API v1")) |> ignore

    app.MapPost("/api/solve", Func<SolveRequest, IResult>(solveHandler))
        .WithSummary("Solve a color-sort puzzle in the fewest moves")
        .WithDescription("Each tube is 4 letters (B, C, G, L, M, O, P, R, W, Y — one per color) \
                          reading top to bottom; colors pour from the last letter. Send only the \
                          filled tubes; empty tubes are added automatically to reach 12 total.")
        .Produces<SolveResponse>(StatusCodes.Status200OK)
        .Produces(StatusCodes.Status400BadRequest)
        .Produces(StatusCodes.Status422UnprocessableEntity)
        |> ignore

    app.MapPost("/api/hint", Func<SolveRequest, IResult>(hintHandler))
        .WithSummary("Get the next optimal move from the current position")
        .WithDescription("Pass the current board: all tubes in order, each 0-4 letters (empty string \
                          for an empty tube). Returns the next move of an optimal continuation and how \
                          many moves remain, or 422 when the position cannot be solved.")
        .Produces<HintResponse>(StatusCodes.Status200OK)
        .Produces(StatusCodes.Status400BadRequest)
        .Produces(StatusCodes.Status422UnprocessableEntity)
        |> ignore

    app.Run()

    0
