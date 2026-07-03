export type PlayMode = 'manual' | 'auto'

export interface Status {
  kind: 'info' | 'success' | 'error'
  text: string
}

interface ControlBarProps {
  mode: PlayMode
  status: Status
  busy: boolean
  canUndo: boolean
  canReanimate: boolean
  canHint: boolean
  onModeChange: (mode: PlayMode) => void
  onRestart: () => void
  onUndo: () => void
  onReanimate: () => void
  onSolve: () => void
  onHint: () => void
}

export default function ControlBar({
  mode,
  status,
  busy,
  canUndo,
  canReanimate,
  canHint,
  onModeChange,
  onRestart,
  onUndo,
  onReanimate,
  onSolve,
  onHint,
}: ControlBarProps) {
  return (
    <footer className="control-bar">
      <button type="button" className="action" onClick={onRestart} disabled={busy}>
        Restart
      </button>

      <div className="mode-toggle" role="group" aria-label="Play mode">
        <button
          type="button"
          className={mode === 'manual' ? 'on' : ''}
          onClick={() => onModeChange('manual')}
          disabled={busy}
        >
          Manual
        </button>
        <button
          type="button"
          className={mode === 'auto' ? 'on' : ''}
          onClick={() => onModeChange('auto')}
          disabled={busy}
        >
          Auto
        </button>
      </div>

      <span className={`status-pill ${status.kind}`}>{status.text}</span>

      <button type="button" className="action" onClick={onReanimate} disabled={busy || !canReanimate}>
        Reanimate
      </button>
      {mode === 'manual' && (
        <button type="button" className="action hint-action" onClick={onHint} disabled={busy || !canHint}>
          Hint
        </button>
      )}
      {mode === 'auto' && (
        <button type="button" className="action solve" onClick={onSolve} disabled={busy}>
          Solve
        </button>
      )}
      <button type="button" className="action" onClick={onUndo} disabled={busy || !canUndo || mode === 'auto'}>
        Undo
      </button>
    </footer>
  )
}
