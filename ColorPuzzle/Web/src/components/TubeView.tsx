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
 * Renders one tube as 4 slots, visually top-to-bottom. Slot `s` (0 = bottom)
 * holds the pill for tube[s]; the last character of the tube string is the top.
 */
export default function TubeView({ index, tube, selected, disabled, hiddenTopCount, onClick }: TubeViewProps) {
  const slots = []
  for (let s = TUBE_CAPACITY - 1; s >= 0; s--) {
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
