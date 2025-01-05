import Bun from 'bun'
import { $ } from 'bun'
import Path from 'path'
import File from 'fs/promises'
import * as Bundle from './bundle.common.js'

const prod = (() => {
  const scriptIx = process.argv.findIndex(a => a.endsWith('bundle.ui.js'))
  return process.argv[scriptIx + 1] === '--prod'
})()

const [mainJs] = await Bundle.prepare({ prod, modules: ['Core.UI.Main'] })

await $`mkdir -p public`
await $`cp -R ui/static/* public/`

// index.html
await (async () => {
  const indexPath = 'public/index.html'
  const index = await File.readFile(indexPath, 'utf8')
  const index_ = index.replaceAll(
    '{{main}}',
    Path.relative('public/index.html', mainJs),
  )
  await File.writeFile(indexPath, index_, 'utf8')
})()

const result = await Bun.build({
  entrypoints: [mainJs],
  minify: prod,
  outdir: 'public',
  target: 'browser',
  format: 'esm',
  sourcemap: !prod ? 'inline' : 'none',
})

if (!result.success) {
  throw new AggregateError(result.logs)
}

// index.css
await (async () => {
  await File.rm('public/index.css')
  await $`bun x tailwindcss -c ui/tailwind.config.js -i ui/static/index.css -o public/index.css 1>&2`
})()
