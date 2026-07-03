import { BrowserRouter, Route, Routes } from 'react-router-dom'
import GamePageRoute from './pages/GamePage'
import LibraryPage from './pages/LibraryPage'
import NewGamePage from './pages/NewGamePage'

export default function App() {
  return (
    <BrowserRouter>
      <Routes>
        <Route path="/" element={<LibraryPage />} />
        <Route path="/new" element={<NewGamePage />} />
        <Route path="/play/:gameId" element={<GamePageRoute />} />
      </Routes>
    </BrowserRouter>
  )
}
