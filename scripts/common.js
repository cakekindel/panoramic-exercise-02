import Fs from 'fs/promises'
import Path from 'path'

export const rootDir = Path.resolve(__dirname, '..')

export const packageSources = async () => {
  const sources = []
  const rootDir = './src';
  const files = await Fs.readdir(rootDir, { recursive: true, withFileTypes: true })
  sources.push(
    ...files.flatMap(e =>
      e.isFile() ? [Path.resolve(rootDir, e.path, e.name)] : [],
    ),
  )
  return sources
}
