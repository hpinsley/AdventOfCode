import { forwardRef, useImperativeHandle, useRef, useState } from 'react'
import { COLOR_CSS } from '../game/colors'
import type { Move, Tube } from '../game/types'
import TubeView from './TubeView'

export interface BoardHandle {
  /**
   * Animate `move` on top of the `tubes` state (which must not yet include it),
   * then invoke `commit` to apply the move to the owner's state. The source
   * pills are hidden while their overlay clones fly, and `commit` runs in the
   * same tick as the flight teardown so React batches them into one render.
   */
  animateMove(tubes: Tube[], move: Move, commit: () => void): Promise<void>
}

interface BoardProps {
  tubes: Tube[]
  selected: number | null
  disabled: boolean
  onTubeClick: (index: number) => void
}

const FLIGHT_MS = 380
const STAGGER_MS = 55
const SLOT_INSET = 3 // matches .slot padding in index.css

const Board = forwardRef<BoardHandle, BoardProps>(function Board(
  { tubes, selected, disabled, onTubeClick },
  ref,
) {
  const boardRef = useRef<HTMLDivElement>(null)
  const overlayRef = useRef<HTMLDivElement>(null)
  const [flight, setFlight] = useState<{ tube: number; count: number } | null>(null)

  useImperativeHandle(ref, () => ({
    async animateMove(current, move, commit) {
      setFlight({ tube: move.from, count: move.count })
      try {
        await runFlight(boardRef.current, overlayRef.current, current, move)
      } finally {
        commit()
        setFlight(null)
      }
    },
  }))

  return (
    <div className="board" ref={boardRef}>
      {tubes.map((tube, i) => (
        <TubeView
          key={i}
          index={i}
          tube={tube}
          selected={selected === i}
          disabled={disabled}
          hiddenTopCount={flight?.tube === i ? flight.count : 0}
          onClick={onTubeClick}
        />
      ))}
      <div className="board-overlay" ref={overlayRef} />
    </div>
  )
})

async function runFlight(
  boardEl: HTMLDivElement | null,
  overlayEl: HTMLDivElement | null,
  tubes: Tube[],
  move: Move,
): Promise<void> {
  if (!boardEl || !overlayEl || typeof Element.prototype.animate !== 'function') return
  const boardRect = boardEl.getBoundingClientRect()

  const slotRect = (tube: number, slot: number): DOMRect | null => {
    const el = boardEl.querySelector<HTMLElement>(`[data-tube="${tube}"][data-slot="${slot}"]`)
    return el ? el.getBoundingClientRect() : null
  }
  const tubeTopY = (tube: number): number => {
    const slot = boardEl.querySelector<HTMLElement>(`[data-tube="${tube}"]`)
    const el = slot?.parentElement
    return el ? el.getBoundingClientRect().top - boardRect.top : 0
  }

  const sourceLen = tubes[move.from].length
  const targetLen = tubes[move.to].length
  const flights: Promise<unknown>[] = []

  for (let i = 0; i < move.count; i++) {
    const src = slotRect(move.from, sourceLen - move.count + i)
    const dst = slotRect(move.to, targetLen + i)
    if (!src || !dst) continue

    const w = src.width - SLOT_INSET * 2
    const h = src.height - SLOT_INSET * 2
    const sx = src.left - boardRect.left + SLOT_INSET
    const sy = src.top - boardRect.top + SLOT_INSET
    const tx = dst.left - boardRect.left + SLOT_INSET
    const ty = dst.top - boardRect.top + SLOT_INSET
    const liftSourceY = tubeTopY(move.from) - h - 8
    const liftTargetY = tubeTopY(move.to) - h - 8

    const clone = document.createElement('div')
    clone.className = 'pill flying'
    clone.style.width = `${w}px`
    clone.style.height = `${h}px`
    clone.style.background = COLOR_CSS[move.letter]
    clone.style.transform = `translate(${sx}px, ${sy}px)`
    overlayEl.appendChild(clone)

    const animation = clone.animate(
      [
        { transform: `translate(${sx}px, ${sy}px)`, offset: 0 },
        { transform: `translate(${sx}px, ${liftSourceY}px)`, offset: 0.3 },
        { transform: `translate(${tx}px, ${liftTargetY}px)`, offset: 0.7 },
        { transform: `translate(${tx}px, ${ty}px)`, offset: 1 },
      ],
      { duration: FLIGHT_MS, delay: i * STAGGER_MS, easing: 'ease-in-out', fill: 'both' },
    )
    flights.push(
      animation.finished.catch(() => undefined).then(() => clone.remove()),
    )
  }

  await Promise.all(flights)
}

export default Board
