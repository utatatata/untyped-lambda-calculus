'use strict'

exports.addEventListenerOnce = function(type) {
  return function(listener) {
    return function(useCapture) {
      return function(target) {
        return function() {
          return target.addEventListener(type, listener, {
            capture: useCapture,
            once: true,
          })
        }
      }
    }
  }
}
