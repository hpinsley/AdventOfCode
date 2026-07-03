import { useMemo, useState } from 'react'
import { Link, useNavigate } from 'react-router-dom'
import { COLOR_CSS, COLOR_NAMES } from '../game/colors'
import { validateBoard } from '../game/engine'
import { saveGame } from '../game/storage'
import { COLOR_LETTERS, MAX_FILLED_TUBES, TUBE_CAPACITY } from '../game/types'
import type { ColorLetter } from '../game/types'

type Brush = ColorLetter | 'erase'

/** The 10 painter tubes serialized as text, one per line, trailing empties dropped. */
function toText(tubes: string[]): string {
  let last = tubes.length - 1
  while (last >= 0 && tubes[last] === '') last--
  return tubes.slice(0, last + 1).join('\n')
}

/** Parse textarea lines back into exactly MAX_FILLED_TUBES painter tubes. */
function fromText(text: string): string[] {
  const lines = text
    .split(/\r?\n/)
    .map((line) => line.trim().toUpperCase())
    .slice(0, MAX_FILLED_TUBES)
  while (lines.length < MAX_FILLED_TUBES) lines.push('')
  return lines
}

export default function NewGamePage() {
  const navigate = useNavigate()
  const [name, setName] = useState('')
  const [text, setText] = useState('')
  const [brush, setBrush] = useState<Brush>('B')

  const tubes = useMemo(() => fromText(text), [text])
  const filled = tubes.filter((t) => t.length > 0)

  const usage = useMemo(() => {
    const counts = {} as Record<ColorLetter, number>
    for (const letter of COLOR_LETTERS) counts[letter] = 0
    for (const tube of tubes) {
      for (const c of tube) {
        if (c in counts) counts[c as ColorLetter]++
      }
    }
    return counts
  }, [tubes])

  const boardError = filled.length === 0 ? 'Enter at least one tube.' : validateBoard(filled)
  const canSave = boardError === null && name.trim().length > 0

  function paintTube(index: number) {
    const next = tubes.slice()
    if (brush === 'erase') {
      if (next[index].length === 0) return
      next[index] = next[index].slice(0, -1)
    } else {
      if (next[index].length >= TUBE_CAPACITY || usage[brush] >= TUBE_CAPACITY) return
      next[index] = next[index] + brush
    }
    setText(toText(next))
  }

  function handleSave() {
    if (!canSave) return
    const game = saveGame(name.trim(), filled)
    navigate(`/play/${game.id}`)
  }

  return (
    <div className="page new-game-page">
      <header className="header-bar">
        <span className="title">New Game</span>
        <span className="goal-chip">Up to {MAX_FILLED_TUBES} tubes · 4 letters each · each color used 4 times</span>
      </header>

      <div className="entry-grid">
        <section className="entry-text">
          <h2>Type it</h2>
          <p className="hint">
            One tube per line, letters {COLOR_LETTERS.join(' ')} reading top to bottom (first letter is
            the top).
          </p>
          <textarea
            rows={MAX_FILLED_TUBES}
            value={text}
            onChange={(e) => setText(e.target.value.toUpperCase())}
            placeholder={'YWCB\nPLMR\n…'}
            spellCheck={false}
          />
        </section>

        <section className="entry-painter">
          <h2>Or paint it</h2>
          <div className="palette">
            {COLOR_LETTERS.map((letter) => (
              <button
                key={letter}
                type="button"
                className={`swatch${brush === letter ? ' on' : ''}`}
                style={{ background: COLOR_CSS[letter] }}
                onClick={() => setBrush(letter)}
                disabled={usage[letter] >= TUBE_CAPACITY}
                title={COLOR_NAMES[letter]}
              >
                {usage[letter]}/4
              </button>
            ))}
            <button
              type="button"
              className={`swatch erase${brush === 'erase' ? ' on' : ''}`}
              onClick={() => setBrush('erase')}
              title="Remove the top color of a tube"
            >
              ⌫
            </button>
          </div>
          <p className="hint">Click a tube to add the selected color (fills top-down).</p>
          <div className="painter-board">
            {tubes.map((tube, i) => (
              <button key={i} type="button" className="tube mini" onClick={() => paintTube(i)}>
                {Array.from({ length: TUBE_CAPACITY }, (_, s) => s).map((s) => (
                  <div key={s} className="slot">
                    {s < tube.length && (
                      <div
                        className="pill"
                        style={{
                          background: COLOR_CSS[tube[s] as ColorLetter] ?? '#555',
                        }}
                      />
                    )}
                  </div>
                ))}
              </button>
            ))}
          </div>
        </section>
      </div>

      <div className="save-row">
        <input
          type="text"
          value={name}
          onChange={(e) => setName(e.target.value)}
          placeholder="Name this game (e.g. 2026-06-27)"
        />
        <button type="button" className="action" onClick={handleSave} disabled={!canSave}>
          Save &amp; play
        </button>
      </div>
      {boardError !== null && filled.length > 0 && <p className="validation-error">{boardError}</p>}

      <Link className="back-link" to="/">
        ← Game library
      </Link>
    </div>
  )
}
