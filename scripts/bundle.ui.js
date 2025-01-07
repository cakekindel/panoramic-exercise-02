import Bun from 'bun'
import { $ } from 'bun'
import Path from 'path'
import File from 'fs/promises'
import * as Bundle from './bundle.common.js'

const [mainJs] = await Bundle.prepare({
  modules: ['ACMECorp.Dog.Web.Main'],
})

await $`mkdir -p public`
await $`cp -R ./static/* public/`

const result = await Bun.build({
  entrypoints: [mainJs],
  minify: true,
  outdir: 'public',
  target: 'browser',
  format: 'esm',
  sourcemap: 'inline',
})

if (!result.success) {
  throw new AggregateError(result.logs)
}

// index.css
await (async () => {
  await File.rm('public/index.css')
  await $`bun x tailwindcss -c ./tailwind.config.js -i ./static/index.css -o public/index.css 1>&2`
})()
