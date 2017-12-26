'use strict';

const _ = require('lodash');
const Sequelize = require('sequelize');
const Op = Sequelize.Op;

const parse = s => {
  const m = s.match(/(in)\((.+)\)/);
  return m[2].split(',');
};

module.exports = obj => {
  return _.mapValues(obj, parse);
};
