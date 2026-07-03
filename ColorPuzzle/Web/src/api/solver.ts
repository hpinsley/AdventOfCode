import { NAME_TO_LETTER } from '../game/colors'
import type { Move, SolveResponse } from '../game/types'

export class SolveError extends Error {
  constructor(
    message: string,
    readonly status: number,
  ) {
    super(message)
  }
}

/** Ask the API for an optimal solution. Tubes are the filled tubes in file format. */
export async function solvePuzzle(tubes: string[]): Promise<Move[]> {
  let response: Response
  try {
    response = await fetch('/api/solve', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ tubes }),
    })
  } catch {
    throw new SolveError('Could not reach the solver API. Is it running?', 0)
  }

  const body = (await response.json().catch(() => null)) as
    | (SolveResponse & { error?: string })
    | null

  if (!response.ok) {
    throw new SolveError(body?.error ?? `Solve failed (HTTP ${response.status}).`, response.status)
  }

  return (body as SolveResponse).moves.map((m) => ({
    count: m.count,
    letter: NAME_TO_LETTER[m.color],
    from: m.fromTube,
    to: m.toTube,
  }))
}
