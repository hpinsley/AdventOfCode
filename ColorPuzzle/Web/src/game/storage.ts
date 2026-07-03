import type { SavedGame } from './types'

const KEY = 'colorpuzzle.games'

// Mirrors ColorPuzzle/samples/cp-2026-06-26-input.json
const SAMPLE_GAME = {
  name: 'Sample 2026-06-26',
  tubes: ['YWCB', 'PLMR', 'WGMO', 'OPML', 'RCGC', 'GOCW', 'BLBB', 'YRYP', 'PLYM', 'ORGW'],
}

function readAll(): SavedGame[] | null {
  const raw = localStorage.getItem(KEY)
  if (raw === null) return null
  try {
    const parsed = JSON.parse(raw)
    return Array.isArray(parsed) ? (parsed as SavedGame[]) : []
  } catch {
    return []
  }
}

function writeAll(games: SavedGame[]): void {
  localStorage.setItem(KEY, JSON.stringify(games))
}

function newId(): string {
  return typeof crypto.randomUUID === 'function'
    ? crypto.randomUUID()
    : `${Date.now()}-${Math.random().toString(36).slice(2)}`
}

/** All saved games; seeds the library with the sample puzzle on first run. */
export function listGames(): SavedGame[] {
  const games = readAll()
  if (games !== null) return games
  const seeded = [makeGame(SAMPLE_GAME.name, SAMPLE_GAME.tubes)]
  writeAll(seeded)
  return seeded
}

export function getGame(id: string): SavedGame | undefined {
  return listGames().find((g) => g.id === id)
}

function makeGame(name: string, tubes: string[]): SavedGame {
  return { id: newId(), name, tubes, createdAt: new Date().toISOString() }
}

export function saveGame(name: string, tubes: string[]): SavedGame {
  const game = makeGame(name, tubes)
  writeAll([...listGames(), game])
  return game
}

export function updateGame(id: string, patch: Partial<Omit<SavedGame, 'id'>>): SavedGame | undefined {
  const games = listGames()
  const index = games.findIndex((g) => g.id === id)
  if (index < 0) return undefined
  games[index] = { ...games[index], ...patch }
  writeAll(games)
  return games[index]
}

export function deleteGame(id: string): void {
  writeAll(listGames().filter((g) => g.id !== id))
}
