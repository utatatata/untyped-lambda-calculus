const { version } = require("../package.json");
const makeCommand = require("./_makeCommand");

const js = `public/untyped-lambda-calculus-${version}.js`;

const deployPureScript = makeCommand(`spago bundle-app --to ${js}`);

deployPureScript();
