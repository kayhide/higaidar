'use strict';

const _ = require('lodash');

module.exports = class {
  constructor(options) {
    this.options = options;
  }

  apply(compiler) {
    compiler.plugin('emit', (compilation, callback) => {
      const chunks = _.pick(compilation.namedChunks, this.options.chunks);
      const files = _.flatMap(_.mapValues(chunks, 'files'));
      compilation.assets = _.omit(compilation.assets, files);

      console.log("Omitting:");
      files.forEach(f => console.log(`  ${f}`));

      callback();
    });
  }
};
