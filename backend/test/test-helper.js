'use strict';

process.env.STAGE = 'test';

const fs = require('fs-extra');
const path = require('path');
const _ = require('lodash');
const yaml = require('js-yaml');
const mime = require('mime-types');
const co = require('co');
const promisify = require('util.promisify');
const stream = require('stream');
const nock = require('nock');
const Sequelize = require('sequelize');
const sinon = require('sinon');

const boot = require('lib/boot');
const fixture = require('test/fixture');


function ensureTestVars() {
  const file = path.join(__dirname, '../../.rake/rds/higaidar-test/vars.yml');
  if (fs.existsSync(file)) {
    fs.removeSync(file);
  }

  const vars = {
    HOST: 'higaidar.test',
    PORT: process.env.DB_PORT,
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
  require('lib/connStub').stub,
  require('lib/awsStub').stub
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

module.exports.setS3File = (s3, bucket, key, file) => {
  return co(function *() {
    const b = yield exists(s3, bucket, key);
    if (!b) {
      yield upload(s3, bucket, key, file);
    }
  });
}

module.exports.unsetS3File = (s3, bucket, key) => {
  return co(function *() {
    const b = yield exists(s3, bucket, key);
    if (b) {
      yield unload(s3, bucket, key);
    }
  });
}

const upload = (s3, bucket, key, file) => {
  const pass = stream.PassThrough();
  const params = {
    Bucket: bucket,
    Key: key,
    Body: pass,
    ContentType: mime.lookup(file),
    ACL: 'public-read'
  };
  fs.createReadStream(fixture.join(file)).pipe(pass);
  return promisify(s3.upload.bind(s3))(params)
};
module.exports.upload = upload;

const unload = (s3, bucket, key) => {
  const params = {
    Bucket: bucket,
    Key: key
  };
  return promisify(s3.deleteObject.bind(s3))(params)
};
module.exports.unload = unload;

const exists = (s3, bucket, key) => {
  const params = {
    Bucket: bucket,
    Key: key
  };
  return promisify(s3.headObject.bind(s3))(params)
    .then(() => Promise.resolve(true), () => Promise.resolve(false));
};
module.exports.exists = exists;
