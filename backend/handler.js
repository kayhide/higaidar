'use strict';

const co = require('co');
const promisify = require('util.promisify');
const AWS = require('aws-sdk');
const Sequelize = require('sequelize');

const signer = new AWS.RDS.Signer({
  region: process.env.AWS_RDS_REGION,
  hostname: process.env.AWS_RDS_HOST,
  port: parseInt(process.env.AWS_RDS_PORT),
  username: process.env.AWS_RDS_USER
});

let token;
let sequelize;

module.exports.hello = (event, context, callback) => {
  context.callbackWaitsForEmptyEventLoop = false;

  co(function *() {
    if (!token) {
      token = yield promisify(signer.getAuthToken.bind(signer))();
      const options = {
        host: process.env.AWS_RDS_HOST,
        port: parseInt(process.env.AWS_RDS_PORT),
        ssl: true,
        dialect: 'mysql',
        dialectOptions: {
          ssl: 'Amazon RDS',
          authSwitchHandler: (data, callback) => {
            if (data.pluginName === 'mysql_clear_password') {
              callback(null, Buffer.from(token + '\0'));
            }
          }
        }
      };
      sequelize = new Sequelize('mysql', process.env.AWS_RDS_USER, token, options);
    }
    const model = sequelize.define('user', {
      'Host': { type: Sequelize.STRING },
      'User': { type: Sequelize.STRING, primaryKey: true }
    }, {
      tableName: 'user',
      timestamps: false
    });

    const data = yield model.findAll();

    console.log(data);
    const response = {
      statusCode: 200,
      body: JSON.stringify({
        message: 'Fine!',
        data: data
      })
    };

    callback(null, response);
  }).catch(err => {
    token = null;
    callback(err);
  });
};
