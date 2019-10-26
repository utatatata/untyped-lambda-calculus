const { version } = require('../package.json')
const makeHtml = require('./_makeHtml')

const js = `public/untyped-lambda-calculus-${version}.js`
const css = `public/untyped-lambda-calculus-${version}.css`
const html = 'public/index.html'

const deployHTML = makeHtml({ path: html, css, js })

deployHTML()
