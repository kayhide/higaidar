'use strict';

const co = require('co');
const promisify = require('util.promisify');
const AWS = require('aws-sdk');
const Sequelize = require('sequelize');

require('tls').DEFAULT_MIN_VERSION = "TLSv1";


module.exports.initSequelize = () => {
  const region = process.env.AWS_RDS_REGION;
  const host = process.env.AWS_RDS_HOST;
  const port = parseInt(process.env.AWS_RDS_PORT);
  const username = process.env.AWS_RDS_USERNAME;
  const database = process.env.AWS_RDS_DATABASE;

  return co(function *() {
    const signer = new AWS.RDS.Signer({
      region: region,
      hostname: host,
      port: port,
      username: username
    });
    const token = yield promisify(signer.getAuthToken.bind(signer))();
    const options = {
      host, port,
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
    return new Sequelize(database, username, token, options);
  });
};
