'use strict'

exports.getComputedStyle = function(elem) {
  return function(window) {
    return function() {
      return window.getComputedStyle(elem)
    }
  }
}

exports.lineHeight = function(style) {
  return function() {
    return Number(style.lineHeight.replace(/px/, ''))
  }
}
