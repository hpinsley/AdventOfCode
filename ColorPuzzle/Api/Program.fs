open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open ColorPuzzle

type SolveRequest = { tubes: string[] }
type MoveDto = { count: int; color: string; fromTube: int; toTube: int }
type SolveResponse = { moveCount: int; moves: MoveDto[] }

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

    app.Run()

    0
