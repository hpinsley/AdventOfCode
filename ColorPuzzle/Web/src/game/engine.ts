import { COLOR_LETTERS, MAX_FILLED_TUBES, TUBE_CAPACITY, TUBE_COUNT } from './types'
import type { ColorLetter, Move, Tube } from './types'

/** The pourable run of matching colors at the top of a tube (null if empty). */
export function topStreak(tube: Tube): { letter: ColorLetter; count: number } | null {
  if (tube.length === 0) return null
  const letter = tube[tube.length - 1] as ColorLetter
  let count = 0
  for (let i = tube.length - 1; i >= 0 && tube[i] === letter; i--) count++
  return { letter, count }
}

export function freeSpace(tube: Tube): number {
  return TUBE_CAPACITY - tube.length
}

/**
 * The move produced by pouring `from` onto `to`, or null if illegal.
 * Matching colors move together: count = min(top streak, free space).
 */
export function pourMove(tubes: Tube[], from: number, to: number): Move | null {
  if (from === to) return null
  const streak = topStreak(tubes[from])
  if (!streak) return null
  const target = tubes[to]
  const space = freeSpace(target)
  if (space === 0) return null
  if (target.length > 0 && target[target.length - 1] !== streak.letter) return null
  return { count: Math.min(streak.count, space), letter: streak.letter, from, to }
}

export function canPour(tubes: Tube[], from: number, to: number): boolean {
  return pourMove(tubes, from, to) !== null
}

export function applyMove(tubes: Tube[], move: Move): Tube[] {
  const next = tubes.slice()
  next[move.from] = tubes[move.from].slice(0, tubes[move.from].length - move.count)
  next[move.to] = tubes[move.to] + move.letter.repeat(move.count)
  return next
}

export function undoMove(tubes: Tube[], move: Move): Tube[] {
  const next = tubes.slice()
  next[move.to] = tubes[move.to].slice(0, tubes[move.to].length - move.count)
  next[move.from] = tubes[move.from] + move.letter.repeat(move.count)
  return next
}

export function isTubeComplete(tube: Tube): boolean {
  return tube.length === TUBE_CAPACITY && [...tube].every((c) => c === tube[0])
}

export function isSolved(tubes: Tube[]): boolean {
  return tubes.every((t) => t.length === 0 || isTubeComplete(t))
}

/** Pad the filled tubes out to the full 12-tube board with empties. */
export function padToBoard(filledTubes: Tube[]): Tube[] {
  const board = filledTubes.slice()
  while (board.length < TUBE_COUNT) board.push('')
  return board
}

/**
 * Same rules as the API's validation: 1-10 tubes, exactly 4 letters each
 * from the palette, and every used color appears exactly 4 times.
 * Returns null when valid, otherwise a message.
 */
export function validateBoard(tubes: string[]): string | null {
  if (tubes.length === 0 || tubes.length > MAX_FILLED_TUBES) {
    return `Expected between 1 and ${MAX_FILLED_TUBES} tubes; got ${tubes.length}.`
  }
  const palette = new Set<string>(COLOR_LETTERS)
  for (const tube of tubes) {
    if (tube.length !== TUBE_CAPACITY || [...tube].some((c) => !palette.has(c))) {
      return `Tube "${tube}" is invalid: each tube must be exactly 4 letters from ${COLOR_LETTERS.join('')}.`
    }
  }
  const counts = new Map<string, number>()
  for (const tube of tubes) {
    for (const c of tube) counts.set(c, (counts.get(c) ?? 0) + 1)
  }
  for (const [letter, occurrences] of counts) {
    if (occurrences !== TUBE_CAPACITY) {
      return `Each color must appear exactly 4 times; "${letter}" appears ${occurrences} times.`
    }
  }
  return null
}
