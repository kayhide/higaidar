'use strict';

const _ = require('lodash');

const factory = {
  user: function *() {
    for (let i = 1;; ++i) {
      yield {
        code: 100 + i,
        name: `User ${i}`,
        tel: `03-1234-${8900 + i}`
      }
    };
  }
};

const cur = {
  user: factory.user()
};

const build = (model, opts = {}) => {
  return Object.assign(cur[model.name].next().value, opts);
};

const buildList = (model, n, opts = {}) => {
  return _.times(n, () => build(model, opts));
};


module.exports.build = build;

module.exports.buildList = buildList;

module.exports.create = (model, opts = {}) => {
  return model.create(build(model, opts));
};

module.exports.createList = (model, n, opts = {}) => {
  return model.bulkCreate(buildList(model, n, opts));
};
