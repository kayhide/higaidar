'use strict';

const fs = require('fs');
const path = require('path');
const co = require('co');

module.exports = {
  read(name) {
    return JSON.parse(fs.readFileSync(path.join(__dirname, 'fixtures', `${name}.json`), 'utf8'));
  },

  join(name) {
    return path.join(__dirname, 'fixtures', name);
  }
}
