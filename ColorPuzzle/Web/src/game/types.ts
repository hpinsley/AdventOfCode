export const COLOR_LETTERS = ['B', 'C', 'G', 'L', 'M', 'O', 'P', 'R', 'W', 'Y'] as const
export type ColorLetter = (typeof COLOR_LETTERS)[number]

export const TUBE_COUNT = 12
export const TUBE_CAPACITY = 4
export const MAX_FILLED_TUBES = 10

/**
 * A tube is a string of 0-4 color letters (file/API format). Colors display
 * top-to-bottom in string order; the LAST character is the pour end (the
 * lowest filled slot on screen).
 */
export type Tube = string

export interface Move {
  count: number
  letter: ColorLetter
  from: number
  to: number
}

export interface SavedGame {
  id: string
  name: string
  /** Filled tubes only, file/API format (e.g. "YWCB"). */
  tubes: Tube[]
  createdAt: string
  bestMoveCount?: number
  optimalMoveCount?: number
}

/** Wire format of POST /api/solve. */
export interface MoveDto {
  count: number
  color: string
  fromTube: number
  toTube: number
}

export interface SolveResponse {
  moveCount: number
  moves: MoveDto[]
}

/** Wire format of POST /api/hint. */
export interface HintResponse {
  hint: MoveDto
  remainingMoves: number
}
