const _ = require('lodash');

const factory = {
  * user() {
    for (let i = 1; ; ++i) {
      yield {
        code: `${100 + i}`,
        name: `User ${i}`,
        tel: `03-1234-${8900 + i}`,
        is_admin: false,
      };
    }
  },
  * photo() {
    for (let i = 1; ; ++i) {
      yield {
        key: `${100 + i}/photo-${i}.jpg`,
        original_url: `http://higaidar.test/${100 + i}/photo-${i}.jpg`,
        thumbnail_url: null,
        crop: null,
        pest: null,
      };
    }
  },
  * pest() {
    for (let i = 1; ; ++i) {
      yield {
        label: `Pest ${i}`,
        crop: `Crop ${i}`,
      };
    }
  },
  * crop() {
    for (let i = 1; ; ++i) {
      yield {
        label: `Crop ${i}`,
      };
    }
  },
};

const cur = _.mapValues(factory, (f) => f());

const build = (model, opts = {}) => Object.assign(cur[model.name].next().value, opts);

const buildList = (model, n, opts = {}) => _.times(n, () => build(model, opts));

module.exports.build = build;

module.exports.buildList = buildList;

module.exports.create = (model, opts = {}) => model.create(build(model, opts));

module.exports.createList = (model, n, opts = {}) => model.bulkCreate(buildList(model, n, opts));
