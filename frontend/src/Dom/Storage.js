"use strict";

exports._get = function(key) {
  return function () {
    return window.localStorage[key];
  };
};

exports._set = function(key) {
  return function(value) {
    return function () {
      window.localStorage[key] = value;
    };
  };
};
