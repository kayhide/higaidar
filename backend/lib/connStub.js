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

  if (process.env.ON_REMOTE) {
    const host = process.env.AWS_RDS_HOST;
    const port = parseInt(process.env.AWS_RDS_PORT);
    const database = process.env.AWS_RDS_DATABASE;
    const username = process.env.AWS_RDS_MASTER_USERNAME;
    const password = process.env.AWS_RDS_MASTER_USER_PASSWORD;
    sequelize = new Sequelize(database, username, password, {
      host, port, dialect: 'mysql'
    })
  }
  else {
    const db_config = yaml.safeLoad(fs.readFileSync(path.join(__dirname, '../../db/config.yml')));
    const c = db_config[process.env.STAGE];
    sequelize = new Sequelize(c.database, c.username, c.password, {
      host: 'localhost',
      dialect: { mysql2: 'mysql' }[c.adapter] || c.adapter
    })
  }

  return Promise.resolve(sequelize);
};

module.exports.stub = {
  'app/model/conn': {
    initSequelize,
    '@global': true
  }
};
