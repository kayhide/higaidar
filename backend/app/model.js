'use strict';

const conn = require('app/model/conn');


let sequelize;

module.exports.with = (models, f) => {
  const attempt = (s) => {
    sequelize = s;
    const args = models.map(m => require(`app/model/${m}`).define(sequelize));
    return f(...args);
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
