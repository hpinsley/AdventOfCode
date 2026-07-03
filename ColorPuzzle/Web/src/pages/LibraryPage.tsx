import { useState } from 'react'
import { Link, useNavigate } from 'react-router-dom'
import { deleteGame, listGames } from '../game/storage'

export default function LibraryPage() {
  const navigate = useNavigate()
  const [games, setGames] = useState(() => listGames())

  function handleDelete(id: string, name: string) {
    if (!window.confirm(`Delete "${name}" from the library?`)) return
    deleteGame(id)
    setGames(listGames())
  }

  return (
    <div className="page library-page">
      <header className="header-bar">
        <span className="title">Color Puzzle</span>
        <span className="stat">Game library</span>
        <Link className="action new-game" to="/new">
          + New game
        </Link>
      </header>

      {games.length === 0 ? (
        <p className="hint">No saved games yet — create one!</p>
      ) : (
        <ul className="game-list">
          {games.map((game) => (
            <li key={game.id} className="game-card">
              <button type="button" className="game-main" onClick={() => navigate(`/play/${game.id}`)}>
                <span className="game-name">{game.name}</span>
                <span className="game-meta">
                  {game.tubes.length} tubes · added {new Date(game.createdAt).toLocaleDateString()}
                  {game.bestMoveCount !== undefined && ` · PB: ${game.bestMoveCount}`}
                  {game.optimalMoveCount !== undefined && ` · Optimal: ${game.optimalMoveCount}`}
                </span>
              </button>
              <button
                type="button"
                className="delete"
                onClick={() => handleDelete(game.id, game.name)}
                aria-label={`Delete ${game.name}`}
              >
                ✕
              </button>
            </li>
          ))}
        </ul>
      )}
    </div>
  )
}
