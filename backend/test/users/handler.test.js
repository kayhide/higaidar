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


let model;
let User;
before(() => co(function *(){
  model = proxyquire('app/model', helper.stub);
  User = yield model.with(['user'], _.identity);
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

    event = fixture.read('users/event_get');
  });

  context('without params', () => {
    it('returns users', () => {
      return co(function *() {
        const users = yield factory.createList(User, 2);

        const res = yield handle(event, {})
        assert(res.statusCode === 200);

        const body = JSON.parse(res.body);
        assert(helper.isEqualModel(body[0], users[0]));
        assert(helper.isEqualModel(body[1], users[1]));
      });
    });

    it('orders by code', () => {
      return co(function *() {
        const user1 = yield factory.create(User, { code: 5 });
        const user2 = yield factory.create(User, { code: 2 });
        const users = [user2, user1];

        const res = yield handle(event, {})
        assert(res.statusCode === 200);

        const body = JSON.parse(res.body);
        assert(helper.isEqualModel(body[0], users[0]));
        assert(helper.isEqualModel(body[1], users[1]));
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

    event = fixture.read('users/event_post');
  });

  context('with valid attrs', () => {
    const attrs = {
      code: 100,
      name: 'Taro',
      tel: '0123-4567-8900'
    };

    beforeEach(() => {
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
        assert(_.matches(user.dataValues, attrs));
      });
    });

    it('returns attributes of the new user', () => {
      return co(function *() {
        const res = yield handle(event, {});
        assert(res.statusCode === 200);

        const body = JSON.parse(res.body);
        assert(_.matches(body, attrs));
      });
    });
  });

  context('with invalid attrs', () => {
    const attrs = {
      code: null,
      name: 'Taro',
      tel: '0123-4567-8900'
    };

    beforeEach(() => {
      event.body = JSON.stringify(attrs);
    });

    it('fails to creates a new user', () => {
      return co(function *() {
        yield handle(event, {});
        const cur = yield User.count();
        assert(cur === 0);
      });
    });

    it('returns 400', () => {
      return co(function *() {
        const res = yield handle(event, {});
        assert(res.statusCode === 400);
      });
    });
  });
});

describe('#show', () => {
  let event;
  let handle;
  let user;

  beforeEach(() => co(function *() {
    const handler = proxyquire('app/users/handler', helper.stub);
    handle = promisify(handler.show.bind(handler));

    user = yield factory.create(User);
    event = fixture.read('users/event_get');
    event.pathParameters = { id: user.id };
  }));


  context('with valid attrs', () => {
    it('returns attributes of the user', () => {
      return co(function *() {
        const res = yield handle(event, {});
        assert(res.statusCode === 200);

        const body = JSON.parse(res.body);
        assert(helper.isEqualModel(body, user));
      });
    });
  });

  context('with invalid id', () => {
    beforeEach(() => {
      event.pathParameters = { id: 0 };
    });

    it('returns 404', () => {
      return co(function *() {
        const res = yield handle(event, {});
        assert(res.statusCode === 404);
      });
    });
  });
});

describe('#update', () => {
  let event;
  let handle;
  let user;

  beforeEach(() => co(function *() {
    const handler = proxyquire('app/users/handler', helper.stub);
    handle = promisify(handler.update.bind(handler));

    user = yield factory.create(User);
    event = fixture.read('users/event_post');
    event.httpMethod = 'PATCH';
    event.requestContext.httpMethod = 'PATCH';
    event.pathParameters = { id: user.id };
  }));

  context('with valid attrs', () => {
    const attrs = {
      code: 200,
      name: 'Jiro',
      tel: '0123-4567-9000'
    };

    beforeEach(() => {
      event.body = JSON.stringify(attrs);
    });

    it('returns attributes of the user', () => {
      return co(function *() {
        const res = yield handle(event, {});
        assert(res.statusCode === 200);

        const body = JSON.parse(res.body);
        assert(_.matches(body, attrs));
      });
    });
  });

  context('with invalid id', () => {
    beforeEach(() => {
      event.pathParameters = { id: 0 };
    });

    it('returns 404', () => {
      return co(function *() {
        const res = yield handle(event, {});
        assert(res.statusCode === 404);
      });
    });
  });

  context('with invalid attrs', () => {
    const attrs = {
      code: null,
      name: 'Jiro',
      tel: '0123-4567-9000'
    };

    beforeEach(() => {
      event.body = JSON.stringify(attrs);
    });

    it('returns 400', () => {
      return co(function *() {
        const res = yield handle(event, {});
        assert(res.statusCode === 400);
      });
    });
  });
});

describe('#destroy', () => {
  let event;
  let handle;
  let user;

  beforeEach(() => co(function *() {
    const handler = proxyquire('app/users/handler', helper.stub);
    handle = promisify(handler.destroy.bind(handler));

    user = yield factory.create(User);
    event = fixture.read('users/event_get');
    event.httpMethod = 'DELETE';
    event.requestContext.httpMethod = 'DELETE';
    event.pathParameters = { id: user.id };
  }));

  context('with valid id', () => {
    it('deletes the user', () => {
      return co(function *() {
        const org = yield User.count();
        yield handle(event, {});
        const cur = yield User.count();
        assert(org === 1);
        assert(cur === 0);
      });
    });

    it('returns 204', () => {
      return co(function *() {
        const res = yield handle(event, {});
        assert(res.statusCode === 204);
      });
    });
  });

  context('with invalid id', () => {
    beforeEach(() => {
      event.pathParameters = { id: 0 };
    });

    it('returns 404', () => {
      return co(function *() {
        const res = yield handle(event, {});
        assert(res.statusCode === 404);
      });
    });
  });
});
