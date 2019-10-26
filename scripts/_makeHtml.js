const { writeFile } = require('fs')
const { promisify } = require('util')
const writeFileP = promisify(writeFile)

module.exports = ({ path, css = '', js = '', body = '' }) => async () => {
  const html = `<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>REPL for The Untyped λ Calculus</title>
    <link href="${css}" rel="stylesheet" />
  </head>
  <body class="bg-gray-900 text-white text-base font-sans">
    <script src="${js}"></script>${body}
  </body>
</html>`
  console.log(`Write HTML to ${path}...`)
  try {
    await writeFileP(path, html, 'utf8')
    console.log('Done.')
    return true
  } catch (e) {
    console.log(e)
    return false
  }
}
