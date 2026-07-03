import { COLOR_CSS } from '../game/colors'
import { TUBE_CAPACITY } from '../game/types'
import type { ColorLetter, Tube } from '../game/types'

interface TubeViewProps {
  index: number
  tube: Tube
  selected: boolean
  disabled: boolean
  /** Number of top pills currently mid-flight (hidden while the overlay animates them). */
  hiddenTopCount: number
  onClick: (index: number) => void
}

/**
 * Renders one tube as 4 slots. Colors read top-to-bottom in string order
 * (tube[0] at the top), so the pour end (last character) is the lowest
 * filled slot and empty slots sit at the bottom — the reddit convention.
 */
export default function TubeView({ index, tube, selected, disabled, hiddenTopCount, onClick }: TubeViewProps) {
  const slots = []
  for (let s = 0; s < TUBE_CAPACITY; s++) {
    const letter = s < tube.length ? (tube[s] as ColorLetter) : null
    const hidden = letter !== null && s >= tube.length - hiddenTopCount
    slots.push(
      <div key={s} className="slot" data-tube={index} data-slot={s}>
        {letter !== null && (
          <div
            className="pill"
            style={{ background: COLOR_CSS[letter], visibility: hidden ? 'hidden' : 'visible' }}
          />
        )}
      </div>,
    )
  }

  return (
    <button
      type="button"
      className={`tube${selected ? ' selected' : ''}`}
      disabled={disabled}
      onClick={() => onClick(index)}
      aria-label={`Tube ${index + 1}`}
    >
      {slots}
    </button>
  )
}
