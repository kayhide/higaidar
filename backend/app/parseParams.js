'use strict';

const _ = require('lodash');

const parseFilter = s => {
  const m = s.match(/(in)\((.+)\)/);
  return m[2].split(',');
};

module.exports = e => {
  const event = e;

  return {
    filter: (...keys) => {
      return _.mapValues(_.pick(event.queryStringParameters, ...keys), parseFilter);
    },

    pager: obj => {
      return Object.assign(
        _.pick(obj, 'offset', 'limit'),
        _.mapValues(_.pick(event.queryStringParameters, 'offset', 'limit'), parseInt)
      );
    }
  };
};
