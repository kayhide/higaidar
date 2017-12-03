'use strict';

const _ = require('lodash');
const co = require('co');
const promisify = require('util.promisify');
const assert = require('power-assert');
const sinon = require('sinon');
const proxyquire = require('proxyquire');

const helper = require('test/test-helper');
const fixture = require('test/fixture');

const identity = x => x;

let model;
let User;
before(() => co(function *(){
  model = proxyquire('app/model', helper.stub);
  User = yield model.with(['user'], identity);
}));

beforeEach(() => co(function *(){
  yield User.destroy({ where: {} });
}));


describe('#index', () => {
  let event;
  let handle;

  beforeEach(() => {
    const handler = proxyquire('app/users/handler', helper.stub);
    handle = promisify(handler.index.bind(handler));
  });

  context('without params', () => {
    beforeEach(() => {
      event = fixture.read('users/event_index');
    });

    it('returns users', () => {
      return co(function *() {
        yield User.create({ code: 123, name: 'Yamada Taro', tel: '03-1234-5678' });
        yield User.create({ code: 124, name: 'Tanaka Jiro', tel: '03-9012-3456' });

        const res = yield handle(event, {})
        assert(res.statusCode === 200);

        const body = JSON.parse(res.body);
        assert(_.isEqual(body.map(x => x.code), [123, 124]));
      });
    });
  });
});

describe('#create', () => {
  let event;
  let handle;

  beforeEach(() => {
    const handler = proxyquire('app/users/handler', helper.stub);
    handle = promisify(handler.create.bind(handler));
  });

  context('with valid attrs', () => {
    const attrs = {
      code: 100,
      name: 'Taro',
      tel: '0123-4567-8900'
    };

    beforeEach(() => {
      event = fixture.read('users/event_create');
      event.body = JSON.stringify(attrs);
    });

    it('creates a new user', () => {
      return co(function *() {
        const org = yield User.count();
        yield handle(event, {});
        const cur = yield User.count();
        assert(org === 0);
        assert(cur === 1);

        const user = yield User.findOne();
        assert(_.isEqual(_.pick(user.dataValues, _.keys(attrs)), attrs));
      });
    });

    it('returns attributes of the new user', () => {
      return co(function *() {
        const res = yield handle(event, {});
        const body = JSON.parse(res.body);
        assert(_.isEqual(_.pick(body, _.keys(attrs)), attrs));
      });
    });
  });
});
