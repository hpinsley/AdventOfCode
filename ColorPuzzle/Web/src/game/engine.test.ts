import { describe, expect, it } from 'vitest'
import {
  applyMove,
  canPour,
  isSolved,
  padToBoard,
  pourMove,
  topStreak,
  undoMove,
  validateBoard,
} from './engine'

describe('topStreak', () => {
  it('is null for an empty tube', () => {
    expect(topStreak('')).toBeNull()
  })

  it('counts the run of matching colors at the top (end of string)', () => {
    expect(topStreak('YWCB')).toEqual({ letter: 'B', count: 1 })
    expect(topStreak('YBBB')).toEqual({ letter: 'B', count: 3 })
    expect(topStreak('BBBB')).toEqual({ letter: 'B', count: 4 })
  })
})

describe('pourMove', () => {
  const tubes = ['YBB', 'RB', 'RRRR', '', 'YYY']

  it('pours the whole streak when it fits', () => {
    expect(pourMove(tubes, 0, 1)).toEqual({ count: 2, letter: 'B', from: 0, to: 1 })
  })

  it('pours only what fits when the streak is bigger than the space', () => {
    expect(pourMove(tubes, 4, 1)).toBeNull() // colors do not match
    expect(pourMove(['YYY', 'RY'], 0, 1)).toEqual({ count: 2, letter: 'Y', from: 0, to: 1 })
  })

  it('allows any color into an empty tube', () => {
    expect(pourMove(tubes, 0, 3)).toEqual({ count: 2, letter: 'B', from: 0, to: 3 })
  })

  it('rejects empty sources, full targets, mismatches, and self-pours', () => {
    expect(canPour(tubes, 3, 0)).toBe(false) // empty source
    expect(canPour(tubes, 0, 2)).toBe(false) // full target
    expect(canPour(tubes, 1, 4)).toBe(false) // B onto Y
    expect(canPour(tubes, 0, 0)).toBe(false) // self
  })
})

describe('applyMove / undoMove', () => {
  it('moves the pills and undo restores the original board', () => {
    const tubes = ['YBB', 'RB', '']
    const move = pourMove(tubes, 0, 1)!
    const after = applyMove(tubes, move)
    expect(after).toEqual(['Y', 'RBBB', ''])
    expect(undoMove(after, move)).toEqual(tubes)
  })
})

describe('isSolved', () => {
  it('is true when every tube is empty or one full color', () => {
    expect(isSolved(['BBBB', 'YYYY', '', ''])).toBe(true)
  })

  it('is false for partial or mixed tubes', () => {
    expect(isSolved(['BBB', 'YYYY', 'B', ''])).toBe(false)
    expect(isSolved(['BBBY', 'YYYB', '', ''])).toBe(false)
  })
})

describe('padToBoard', () => {
  it('pads the filled tubes to a 12-tube board', () => {
    const board = padToBoard(['YWCB', 'PLMR'])
    expect(board).toHaveLength(12)
    expect(board.slice(2).every((t) => t === '')).toBe(true)
  })
})

describe('validateBoard', () => {
  const sample = ['YWCB', 'PLMR', 'WGMO', 'OPML', 'RCGC', 'GOCW', 'BLBB', 'YRYP', 'PLYM', 'ORGW']

  it('accepts the sample puzzle', () => {
    expect(validateBoard(sample)).toBeNull()
  })

  it('rejects unknown letters and wrong lengths', () => {
    expect(validateBoard(['XXXX'])).toMatch(/invalid/)
    expect(validateBoard(['BBB'])).toMatch(/invalid/)
  })

  it('rejects color counts other than 4', () => {
    expect(validateBoard(['BBBG', 'GGGB', 'RRRR', 'YYYB'])).toMatch(/appears 5 times/)
  })

  it('rejects more than 10 tubes', () => {
    expect(validateBoard(Array(11).fill('BBBB'))).toMatch(/between 1 and 10/)
  })

  it('accepts small balanced boards', () => {
    expect(validateBoard(['BBBR', 'RRRB', 'GGGG'])).toBeNull()
  })
})
