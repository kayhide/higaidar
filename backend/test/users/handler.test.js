'use strict';

const _ = require('lodash');
const co = require('co');
const promisify = require('util.promisify');
const assert = require('power-assert');
const sinon = require('sinon');
const proxyquire = require('proxyquire');

const helper = require('test/test-helper');
const fixture = require('test/fixture');
const factory = require('test/factory');

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
      event = fixture.read('users/event_get');
    });

    it('returns users', () => {
      return co(function *() {
        const users = yield factory.createList(User, 2);

        const res = yield handle(event, {})
        assert(res.statusCode === 200);

        const body = JSON.parse(res.body);
        assert(_.isEqualWith(body, users.map(x => x.dataValues), (x, y) => x.id === y.id));
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
      event = fixture.read('users/event_post');
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
        assert(helper.isIncluding(user.dataValues, attrs));
      });
    });

    it('returns attributes of the new user', () => {
      return co(function *() {
        const res = yield handle(event, {});
        const body = JSON.parse(res.body);
        assert(helper.isIncluding(body, attrs));
      });
    });
  });
});

describe('#show', () => {
  let event;
  let handle;
  let user;

  beforeEach(() => {
    const handler = proxyquire('app/users/handler', helper.stub);
    handle = promisify(handler.show.bind(handler));
  });

  context('with valid attrs', () => {
    beforeEach(() => co(function *() {
      user = yield factory.create(User);
      event = fixture.read('users/event_get');
      event.pathParameters = { id: user.id };
    }));

    it('returns attributes of the user', () => {
      return co(function *() {
        const res = yield handle(event, {});
        const body = JSON.parse(res.body);
        assert(helper.isEqualModel(body, user));
      });
    });
  });
});

describe('#update', () => {
  let event;
  let handle;
  let user;

  beforeEach(() => {
    const handler = proxyquire('app/users/handler', helper.stub);
    handle = promisify(handler.update.bind(handler));
  });

  context('with valid attrs', () => {
    const attrs = {
      code: 200,
      name: 'Jiro',
      tel: '0123-4567-9000'
    };

    beforeEach(() => co(function *() {
      user = yield factory.create(User);
      event = fixture.read('users/event_post');
      event.httpMethod = 'PATCH';
      event.requestContext.httpMethod = 'PATCH';
      event.pathParameters = { id: user.id };
      event.body = JSON.stringify(attrs);
    }));

    it('returns attributes of the user', () => {
      return co(function *() {
        const res = yield handle(event, {});
        const body = JSON.parse(res.body);
        assert(helper.isIncluding(body, attrs));
      });
    });
  });
});

describe('#destroy', () => {
  let event;
  let handle;
  let user;

  beforeEach(() => {
    const handler = proxyquire('app/users/handler', helper.stub);
    handle = promisify(handler.destroy.bind(handler));
  });

  context('with valid id', () => {
    beforeEach(() => co(function *() {
      user = yield factory.create(User);
      event = fixture.read('users/event_get');
      event.httpMethod = 'DELETE';
      event.requestContext.httpMethod = 'DELETE';
      event.pathParameters = { id: user.id };
    }));

    it('deletes the user', () => {
      return co(function *() {
        const org = yield User.count();
        yield handle(event, {});
        const cur = yield User.count();
        assert(org === 1);
        assert(cur === 0);
      });
    });
  });
});
