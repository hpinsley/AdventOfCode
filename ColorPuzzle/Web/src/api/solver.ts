import { NAME_TO_LETTER } from '../game/colors'
import type { HintResponse, Move, MoveDto, SolveResponse } from '../game/types'

export class SolveError extends Error {
  constructor(
    message: string,
    readonly status: number,
  ) {
    super(message)
  }
}

function dtoToMove(dto: MoveDto): Move {
  return {
    count: dto.count,
    letter: NAME_TO_LETTER[dto.color],
    from: dto.fromTube,
    to: dto.toTube,
  }
}

async function post<T>(path: string, tubes: string[], failVerb: string): Promise<T> {
  let response: Response
  try {
    response = await fetch(path, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ tubes }),
    })
  } catch {
    throw new SolveError('Could not reach the solver API. Is it running?', 0)
  }

  const body = (await response.json().catch(() => null)) as (T & { error?: string }) | null

  if (!response.ok) {
    throw new SolveError(body?.error ?? `${failVerb} failed (HTTP ${response.status}).`, response.status)
  }

  return body as T
}

/** Ask the API for an optimal solution. Tubes are the filled tubes in file format. */
export async function solvePuzzle(tubes: string[]): Promise<Move[]> {
  const body = await post<SolveResponse>('/api/solve', tubes, 'Solve')
  return body.moves.map(dtoToMove)
}

/**
 * Ask the API for the next optimal move from the current position. Send the
 * whole board in order (empty strings for empty tubes) — the returned move's
 * indices refer to these positions. Throws SolveError with the server's
 * message when no hint is available (unsolvable or already solved).
 */
export async function getHint(tubes: string[]): Promise<{ move: Move; remainingMoves: number }> {
  const body = await post<HintResponse>('/api/hint', tubes, 'Hint')
  return { move: dtoToMove(body.hint), remainingMoves: body.remainingMoves }
}
