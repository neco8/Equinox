import { defineConfig } from 'vite';
import elmPlugin from 'vite-plugin-elm';
import sassPlugin from 'vite-plugin-sass';

export default defineConfig({
  base: './', // GitHubページでの相対パスを正しく解決するため
  plugins: [
    elmPlugin(),
    sassPlugin()
  ],
  build: {
    outDir: 'dist',
    emptyOutDir: true
  }
});
