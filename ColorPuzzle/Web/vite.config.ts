import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

// https://vite.dev/config/
export default defineConfig({
  plugins: [react()],
  server: {
    host: true, // listen on 0.0.0.0 so the devcontainer port forward works
    proxy: {
      // The ColorPuzzle solver API (ColorPuzzle/Api, `dotnet run`)
      '/api': 'http://localhost:5197',
    },
  },
})
