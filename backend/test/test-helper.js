'use strict';

process.env.STAGE = 'test';

const fs = require('fs-extra');
const path = require('path');
const _ = require('lodash');
const yaml = require('js-yaml');
const co = require('co');
const nock = require('nock');
const Sequelize = require('sequelize');

const boot = require('lib/boot');
const fixture = require('test/fixture');


function ensureTestVars() {
  const file = path.join(__dirname, '../../.rake/rds/higaidar-test/vars.yml');
  if (fs.existsSync(file)) {
    return;
  }

  const vars = {
    HOST: 'higaidar.test',
    PORT: 3306,
    DBI_RESOURCE_ID: 'db-XXXXXXXXXXXXXXXXXXXXXXXXXX',
    USERNAME: 'lambda',
    DATABASE: 'higaidar'
  };
  fs.ensureDirSync(path.dirname(file));
  fs.writeFileSync(file, yaml.safeDump(vars));
}

before((done) => {
  nock.enableNetConnect('localhost');
  // nock.recorder.rec();

  co(function *() {
    ensureTestVars();
    yield boot();
    done();
  }).catch(done);
});

module.exports.stub = Object.assign(
  {},
  require('lib/connStub').stub
);


const comparablify = (x) => {
  const x_ = x.dataValues ? x.dataValues : x;
  return _.mapValues(x_, v => {
    if (v instanceof Date) {
      return new Date(Math.floor(v.getTime() / 1000) * 1000).toJSON();
    }
    return v;
  });
};

module.exports.isEqualModel = (x, y) => {
  return _.isEqual(comparablify(x), comparablify(y));
};
