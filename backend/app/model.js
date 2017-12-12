'use strict';

const _ = require('lodash');

const conn = require('app/model/conn');


let sequelize;

module.exports.with = (f) => {
  const attempt = (s) => {
    sequelize = s;
    const m = load(sequelize);
    return f(m);
  };

  const retry = (err) => {
    if (err.name === 'SequelizeAccessDeniedError') {
      console.log('authentication retrying...');
      sequelize = null;
      return conn.initSequelize().then(attempt);
    }
    return Promise.reject(err);
  }

  return (sequelize ? Promise.resolve(sequelize) : conn.initSequelize())
    .then(attempt).catch(retry)
};


const load = (sequelize) => {
  const defs = {
    User: require('app/model/user'),
    Photo: require('app/model/photo')
  };

  const m = _.mapValues(defs, d => d.define(sequelize));
  _.mapValues(defs, d => d.relate(m));

  return m;
};
