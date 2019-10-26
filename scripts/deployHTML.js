const { version } = require("../package.json");
const makeHtml = require("./_makeHtml");

const js = `./untyped-lambda-calculus-${version}.js`;
const css = `./untyped-lambda-calculus-${version}.css`;
const html = "public/index.html";

const deployHTML = makeHtml({ path: html, css, js });

deployHTML();
