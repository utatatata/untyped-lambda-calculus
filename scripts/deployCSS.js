const { version } = require("../package.json");
const makeCommand = require("./_makeCommand");

const css = `public/untyped-lambda-calculus-${version}.css`;

const deployCSS = makeCommand(
  `npx tailwind build assets/css/styles.css -o ${css}`
);

deployCSS();
