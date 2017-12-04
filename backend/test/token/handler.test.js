'use strict';

const _ = require('lodash');
const co = require('co');
const promisify = require('util.promisify');
const assert = require('power-assert');
const proxyquire = require('proxyquire');
const jwt = require('jsonwebtoken');

const helper = require('test/test-helper');
const fixture = require('test/fixture');
const factory = require('test/factory');


let model;
let User;
before(() => co(function *(){
  model = proxyquire('app/model', helper.stub);
  User = yield model.with(['user'], _.identity);
}));

beforeEach(() => co(function *(){
  yield User.destroy({ where: {} });
}));


describe('#create', () => {
  let event;
  let handle;
  let user;
  const attrs = { code: 1703, tel: '03-3624-4700' };

  beforeEach(() => co(function *() {
    const handler = proxyquire('app/token/handler', helper.stub);
    handle = promisify(handler.create.bind(handler));

    user = yield factory.create(User, attrs);
    event = {};
    event.body = JSON.stringify(attrs);
  }));

  context('with valid attrs', () => {
    it('returns a token with user', () => {
      return co(function *() {
        const res = yield handle(event, {});
        assert(res.statusCode === 200);

        const token = JSON.parse(res.body).token;
        const decoded = jwt.verify(token, process.env.JWT_SECRET);
        assert(helper.isEqualModel(decoded.user, user));
      });
    });
  });

  context('with wrong attrs', () => {
    beforeEach(() => {
      event.body = JSON.stringify({ code: 9999, tel: '00-0000-0000' });
    });

    it('returns 401', () => {
      return co(function *() {
        const res = yield handle(event, {});
        assert(res.statusCode === 401);
      });
    });
  });

  context('with partially correct attrs', () => {
    beforeEach(() => {
      event.body = JSON.stringify({ code: 1703 });
    });

    it('returns 401', () => {
      return co(function *() {
        const res = yield handle(event, {});
        assert(res.statusCode === 401);
      });
    });
  });
});

describe('#authorize', () => {
  let event;
  let handle;
  let user;
  let token;

  beforeEach(() => co(function *() {
    const handler = proxyquire('app/token/handler', helper.stub);
    handle = promisify(handler.authorize.bind(handler));

    user = yield factory.create(User);
    token = jwt.sign(
      { user: user.dataValues },
      process.env.JWT_SECRET,
      { expiresIn: process.env.JWT_EXPIRES_IN }
    )
    event = {};
    event.methodArn = 'arn:aws:execute-api:ap-northheast-1::api-id/test/GET/something';
    event.authorizationToken = `Bearer ${token}`;
  }));

  context('with valid token', () => {
    it('authorizes', () => {
      return co(function *() {
        const res = yield handle(event, {});
        assert(res.policyDocument.Statement[0].Action === 'execute-api:Invoke');
        assert(res.policyDocument.Statement[0].Effect === 'Allow');
        assert(res.policyDocument.Statement[0].Resource === event.methodArn);
      });
    });
  });

  context('with invalid token', () => {
    it('unauthorizes', () => {
      event.authorizationToken = 'invalid';
      return co(function *() {
        yield handle(event, {});
      }).then(() => assert(false), err => {
        assert(err === 'Unauthorized');
      });
    });
  });

  context('with malformed token', () => {
    it('unauthorizes', () => {
      event.authorizationToken = token;
      return co(function *() {
        yield handle(event, {});
      }).then(() => assert(false), err => {
        assert(err === 'Unauthorized');
      });
    });
  });
});
