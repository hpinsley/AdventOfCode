import type { ColorLetter } from './types'

export const COLOR_CSS: Record<ColorLetter, string> = {
  B: '#2c4ef2',
  C: '#35e6f2',
  G: '#30e63c',
  L: '#8b8bf2',
  M: '#f230d8',
  O: '#ffa21a',
  P: '#9b30d9',
  R: '#f23434',
  W: '#e6e6ea',
  Y: '#ffe81a',
}

export const COLOR_NAMES: Record<ColorLetter, string> = {
  B: 'Blue',
  C: 'Cyan',
  G: 'Green',
  L: 'Lavender',
  M: 'Magenta',
  O: 'Orange',
  P: 'Purple',
  R: 'Red',
  W: 'White',
  Y: 'Yellow',
}

/** Inverse of COLOR_NAMES, for mapping API responses ("Blue") back to letters. */
export const NAME_TO_LETTER: Record<string, ColorLetter> = Object.fromEntries(
  (Object.entries(COLOR_NAMES) as [ColorLetter, string][]).map(([letter, name]) => [name, letter]),
)
