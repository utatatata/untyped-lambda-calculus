"use strict";

exports.execCommand = function(commandName) {
  return function(document) {
    return function() {
      return document.execCommand(commandName);
    };
  };
};
