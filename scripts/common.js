import Fs from 'fs/promises'
import Path from 'path'

export const rootDir = Path.resolve(__dirname, '..')

export const packageSources = async () => {
  const sources = []
  const dirs = ['./src', './static']

  for (const dir of dirs) {
    const files = await Fs.readdir(dir, {
      recursive: true,
      withFileTypes: true,
    })

    sources.push(
      ...files.flatMap(e =>
        e.isFile() ? [Path.resolve(rootDir, e.path, e.name)] : [],
      ),
    )
  }
  return sources
}
