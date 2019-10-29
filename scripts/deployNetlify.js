const { join } = require("path");
const { copyFile } = require("fs");
const { promisify } = require("util");
const copyFileP = promisify(copyFile);

const name = "_redirects";
const from = join("assets/netlify", name);
const to = join("public", name);

const deployNetlify = async () => {
  console.log(`Copy '${name}' from '${from}' to '${to}'...`);
  try {
    await copyFileP(from, to);
    console.log("Done.");
    return true;
  } catch (e) {
    console.log(e);
    return false;
  }
};

deployNetlify();
