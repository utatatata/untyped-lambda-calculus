const { exec } = require('child_process')
const { promisify } = require('util')
const execP = promisify(exec)
const style = require('./_style')

module.exports = command => async () => {
  console.log('$', command)
  try {
    const result = await execP(command)
    console.log(result.stdout)
    return true
  } catch (e) {
    console.log(
      `${style.start(style.bold, style.red)}error${
        style.end
      } Command failed with exit code ${e.code}.`
    )
    console.log(e.stderr)
    return false
  }
}
