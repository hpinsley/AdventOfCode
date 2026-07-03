import { useEffect, useMemo, useRef, useState } from 'react'
import { Link, useParams } from 'react-router-dom'
import { SolveError, solvePuzzle } from '../api/solver'
import Board from '../components/Board'
import type { BoardHandle } from '../components/Board'
import ControlBar from '../components/ControlBar'
import type { PlayMode, Status } from '../components/ControlBar'
import HeaderBar from '../components/HeaderBar'
import { applyMove, isSolved, padToBoard, pourMove, undoMove } from '../game/engine'
import { getGame, updateGame } from '../game/storage'
import type { Move, SavedGame } from '../game/types'

const sleep = (ms: number) => new Promise<void>((resolve) => setTimeout(resolve, ms))

export default function GamePageRoute() {
  const { gameId } = useParams()
  const game = gameId ? getGame(gameId) : undefined
  if (!game) {
    return (
      <div className="page">
        <p>Game not found.</p>
        <Link to="/">← Back to the game library</Link>
      </div>
    )
  }
  // Key by id so navigating between games resets all play state.
  return <GamePage key={game.id} initialGame={game} />
}

function GamePage({ initialGame }: { initialGame: SavedGame }) {
  const [game, setGame] = useState(initialGame)
  const initialTubes = useMemo(() => padToBoard(game.tubes), [game.tubes])

  const [tubes, setTubes] = useState(initialTubes)
  const [moves, setMoves] = useState<Move[]>([])
  const [selected, setSelected] = useState<number | null>(null)
  const [mode, setMode] = useState<PlayMode>('manual')
  const [animating, setAnimating] = useState(false)
  const [error, setError] = useState<string | null>(null)

  const boardRef = useRef<BoardHandle>(null)
  const solved = isSolved(tubes)

  // Record a personal best once a manual game settles in a solved state.
  useEffect(() => {
    if (!solved || animating || moves.length === 0 || mode !== 'manual') return
    if (game.bestMoveCount === undefined || moves.length < game.bestMoveCount) {
      const updated = updateGame(game.id, { bestMoveCount: moves.length })
      if (updated) setGame(updated)
    }
  }, [solved, animating, moves.length, mode, game])

  async function performMove(move: Move) {
    setAnimating(true)
    const current = tubes
    try {
      const board = boardRef.current
      if (board) {
        await board.animateMove(current, move, () => setTubes(applyMove(current, move)))
      } else {
        setTubes(applyMove(current, move))
      }
      setMoves((m) => [...m, move])
    } finally {
      setAnimating(false)
    }
  }

  /** Reset to the initial board and animate `sequence` move by move. */
  async function runSequence(sequence: Move[]) {
    setSelected(null)
    let current = initialTubes
    setTubes(current)
    await sleep(300)
    for (const move of sequence) {
      const board = boardRef.current
      if (board) {
        await board.animateMove(current, move, () => {
          current = applyMove(current, move)
          setTubes(current)
        })
      } else {
        current = applyMove(current, move)
        setTubes(current)
      }
      await sleep(70)
    }
  }

  function handleTubeClick(i: number) {
    if (animating || mode !== 'manual') return
    setError(null)
    if (selected === null) {
      if (tubes[i].length > 0) setSelected(i)
      return
    }
    if (selected === i) {
      setSelected(null)
      return
    }
    const move = pourMove(tubes, selected, i)
    if (move) {
      setSelected(null)
      void performMove(move)
    } else {
      // Invalid target: switch the selection instead (empty tubes deselect).
      setSelected(tubes[i].length > 0 ? i : null)
    }
  }

  function handleRestart() {
    setTubes(initialTubes)
    setMoves([])
    setSelected(null)
    setError(null)
  }

  function handleUndo() {
    if (moves.length === 0) return
    const last = moves[moves.length - 1]
    setTubes((t) => undoMove(t, last))
    setMoves((m) => m.slice(0, -1))
    setSelected(null)
  }

  async function handleReanimate() {
    if (moves.length === 0) return
    setAnimating(true)
    try {
      await runSequence(moves)
    } finally {
      setAnimating(false)
    }
  }

  async function handleSolve() {
    setError(null)
    setAnimating(true)
    try {
      const solution = await solvePuzzle(game.tubes)
      setMoves(solution)
      if (game.optimalMoveCount !== solution.length) {
        const updated = updateGame(game.id, { optimalMoveCount: solution.length })
        if (updated) setGame(updated)
      }
      await runSequence(solution)
    } catch (e) {
      setError(e instanceof SolveError ? e.message : 'Solve failed unexpectedly.')
    } finally {
      setAnimating(false)
    }
  }

  let status: Status
  if (error) {
    status = { kind: 'error', text: error }
  } else if (animating) {
    status = { kind: 'info', text: mode === 'auto' ? 'Solving…' : 'Pouring…' }
  } else if (solved && moves.length > 0) {
    const pb = game.bestMoveCount !== undefined ? ` · PB: ${game.bestMoveCount}` : ''
    const optimal = game.optimalMoveCount !== undefined ? ` · Optimal: ${game.optimalMoveCount}` : ''
    status = { kind: 'success', text: `✓ Solved in ${moves.length} moves!${pb}${optimal}` }
  } else {
    status = { kind: 'info', text: game.name }
  }

  return (
    <div className="page game-page">
      <HeaderBar moves={moves.length} selected={selected} tubes={tubes} />
      <Board
        ref={boardRef}
        tubes={tubes}
        selected={selected}
        disabled={animating || mode !== 'manual'}
        onTubeClick={handleTubeClick}
      />
      <ControlBar
        mode={mode}
        status={status}
        busy={animating}
        canUndo={moves.length > 0}
        canReanimate={moves.length > 0}
        onModeChange={setMode}
        onRestart={handleRestart}
        onUndo={handleUndo}
        onReanimate={handleReanimate}
        onSolve={handleSolve}
      />
      <Link className="back-link" to="/">
        ← Game library
      </Link>
    </div>
  )
}
