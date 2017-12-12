'use strict';

const fs = require('fs-extra');
const path = require('path');
const yaml = require('js-yaml');
const Sequelize = require('sequelize');

let sequelize;
const initSequelize = () => {
  if (sequelize) {
    return Promise.resolve(sequelize);
  }
  const db_config = yaml.safeLoad(fs.readFileSync(path.join(__dirname, '../../db/config.yml')));
  const c = db_config[process.env.STAGE];
  sequelize = new Sequelize(c.database, c.username, c.password, {
    host: 'localhost',
    dialect: { mysql2: 'mysql' }[c.adapter] || c.adapter
  })

  return Promise.resolve(sequelize);
};

module.exports.stub = {
  'app/model/conn': {
    initSequelize,
    '@global': true
  }
};
