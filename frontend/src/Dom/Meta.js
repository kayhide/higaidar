"use strict";

exports._get = function(name) {
  return function () {
    return document.head.querySelector("[name=" + name + "]").content;
  };
};

