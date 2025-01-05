import File from 'fs/promises'
import Path from 'path'
import Process from 'process'

/** @type {(o: {prod: boolean, modules: string[]}) => Promise<string[]>} */
export const prepare = async ({ modules }) => {
  const indexs = modules.map(m =>
    Path.resolve(Process.cwd(), 'output', m, 'index.js'),
  )

  const mains = modules.map(m =>
    Path.resolve(Process.cwd(), 'output', m, 'main.js'),
  )

  await File.mkdir('./dist').catch(() => {})
  for (let ix = 0; ix < modules.length; ix++) {
    const index = indexs[ix]
    const main = mains[ix]
    await File.writeFile(main, `import {main} from '${index}'; main();`)
  }

  return mains
}
