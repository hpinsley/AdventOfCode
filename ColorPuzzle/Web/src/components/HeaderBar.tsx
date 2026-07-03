import { COLOR_NAMES } from '../game/colors'
import { topStreak } from '../game/engine'
import type { Tube } from '../game/types'

interface HeaderBarProps {
  moves: number
  selected: number | null
  tubes: Tube[]
}

export default function HeaderBar({ moves, selected, tubes }: HeaderBarProps) {
  let selectedLabel = 'None'
  if (selected !== null) {
    const streak = topStreak(tubes[selected])
    selectedLabel = streak ? `Tube ${selected + 1} (${COLOR_NAMES[streak.letter]})` : `Tube ${selected + 1}`
  }

  return (
    <header className="header-bar">
      <span className="title">Color Puzzle</span>
      <span className="stat">
        Moves: <strong>{moves}</strong>
      </span>
      <span className="stat">
        Selected: <strong>{selectedLabel}</strong>
      </span>
      <span className="goal-chip">Goal: Group each color</span>
    </header>
  )
}
