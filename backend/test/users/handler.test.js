const _ = require('lodash');
const co = require('co');
const promisify = require('util.promisify');
const assert = require('power-assert');
const sinon = require('sinon');
const proxyquire = require('proxyquire');

const helper = require('test/test-helper');
const fixture = require('test/fixture');
const factory = require('test/factory');

describe('users', () => {
  let User;
  before(() => co(function* () {
    const model = proxyquire('app/model', helper.stub);
    const m = yield model.with(_.identity);
    User = m.User;
  }));

  afterEach(() => co(function* () {
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
      it('returns users', () => co(function* () {
        const users = yield factory.createList(User, 2);

        const res = yield handle(event, {});
        assert(res.statusCode === 200);

        const body = JSON.parse(res.body);
        assert(helper.isEqualModel(body[0], users[0]));
        assert(helper.isEqualModel(body[1], users[1]));
      }));

      it('orders by code', () => co(function* () {
        const user1 = yield factory.create(User, { code: '5' });
        const user2 = yield factory.create(User, { code: '2' });
        const users = [user2, user1];

        const res = yield handle(event, {});
        assert(res.statusCode === 200);

        const body = JSON.parse(res.body);
        assert(helper.isEqualModel(body[0], users[0]));
        assert(helper.isEqualModel(body[1], users[1]));
      }));

      it('returns content range', () => co(function* () {
        const users = yield factory.createList(User, 7);
        const res = yield handle(event, {});
        assert(res.headers['Content-Range'] === '0-6/7');
      }));
    });

    context('with paging params', () => {
      it('offsets and limits', () => co(function* () {
        event.queryStringParameters = {
          offset: '2',
          limit: '3',
        };
        const users = yield factory.createList(User, 7);
        const res = yield handle(event, {});
        assert(res.headers['Content-Range'] === '2-4/7');

        const body = JSON.parse(res.body);
        assert(body.length === 3);
        assert(helper.isEqualModel(body[0], users[2]));
        assert(helper.isEqualModel(body[2], users[4]));
      }));
    });

    context('with id param', () => {
      it('filters by id', () => co(function* () {
        const users = yield factory.createList(User, 3);
        event.queryStringParameters = {
          id: `in(${users[0].id},${users[2].id})`,
        };
        const res = yield handle(event, {});
        const body = JSON.parse(res.body);
        assert(body.length === 2);
        assert(helper.isEqualModel(body[0], users[0]));
        assert(helper.isEqualModel(body[1], users[2]));
      }));
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
        code: '100',
        name: 'Taro',
        tel: '0123-4567-8900',
      };

      beforeEach(() => {
        event.body = JSON.stringify(attrs);
      });

      it('creates a new user', () => co(function* () {
        const org = yield User.count();
        yield handle(event, {});
        const cur = yield User.count();
        assert(org === 0);
        assert(cur === 1);

        const user = yield User.findOne();
        assert(_.isMatch(user.dataValues, attrs));
      }));

      it('returns attributes of the new user', () => co(function* () {
        const res = yield handle(event, {});
        assert(res.statusCode === 200);

        const body = JSON.parse(res.body);
        assert(_.isMatch(body, attrs));
        assert(_.isMatch(body, { is_admin: false, is_editor: false }));
      }));
    });

    context('with invalid attrs', () => {
      const attrs = {
        code: null,
        name: 'Taro',
        tel: '0123-4567-8900',
      };

      beforeEach(() => {
        event.body = JSON.stringify(attrs);
      });

      it('fails to creates a new user', () => co(function* () {
        yield handle(event, {});
        const cur = yield User.count();
        assert(cur === 0);
      }));

      it('returns 400', () => co(function* () {
        const res = yield handle(event, {});
        assert(res.statusCode === 400);
      }));
    });
  });

  describe('#show', () => {
    let event;
    let handle;
    let user;

    beforeEach(() => co(function* () {
      const handler = proxyquire('app/users/handler', helper.stub);
      handle = promisify(handler.show.bind(handler));

      user = yield factory.create(User);
      event = fixture.read('users/event_get');
      event.pathParameters = { id: user.id };
    }));

    context('with valid attrs', () => {
      it('returns attributes of the user', () => co(function* () {
        const res = yield handle(event, {});
        assert(res.statusCode === 200);

        const body = JSON.parse(res.body);
        assert(helper.isEqualModel(body, user));
      }));
    });

    context('with invalid id', () => {
      beforeEach(() => {
        event.pathParameters = { id: 0 };
      });

      it('returns 404', () => co(function* () {
        const res = yield handle(event, {});
        assert(res.statusCode === 404);
      }));
    });
  });

  describe('#update', () => {
    let event;
    let handle;
    let user;

    beforeEach(() => co(function* () {
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
        code: '200',
        name: 'Jiro',
        tel: '0123-4567-9000',
      };

      beforeEach(() => {
        event.body = JSON.stringify(attrs);
      });

      it('returns attributes of the user', () => co(function* () {
        const res = yield handle(event, {});
        assert(res.statusCode === 200);

        const body = JSON.parse(res.body);
        assert(_.isMatch(body, attrs));
      }));
    });

    context('with invalid id', () => {
      beforeEach(() => {
        event.pathParameters = { id: 0 };
      });

      it('returns 404', () => co(function* () {
        const res = yield handle(event, {});
        assert(res.statusCode === 404);
      }));
    });

    context('with invalid attrs', () => {
      const attrs = {
        code: null,
        name: 'Jiro',
        tel: '0123-4567-9000',
      };

      beforeEach(() => {
        event.body = JSON.stringify(attrs);
      });

      it('returns 400', () => co(function* () {
        const res = yield handle(event, {});
        assert(res.statusCode === 400);
      }));
    });
  });

  describe('#destroy', () => {
    let event;
    let handle;
    let user;

    beforeEach(() => co(function* () {
      const handler = proxyquire('app/users/handler', helper.stub);
      handle = promisify(handler.destroy.bind(handler));

      user = yield factory.create(User);
      event = fixture.read('users/event_get');
      event.httpMethod = 'DELETE';
      event.requestContext.httpMethod = 'DELETE';
      event.pathParameters = { id: user.id };
    }));

    context('with valid id', () => {
      it('deletes the user', () => co(function* () {
        const org = yield User.count();
        yield handle(event, {});
        const cur = yield User.count();
        assert(org === 1);
        assert(cur === 0);
      }));

      it('returns 204', () => co(function* () {
        const res = yield handle(event, {});
        assert(res.statusCode === 204);
      }));
    });

    context('with invalid id', () => {
      beforeEach(() => {
        event.pathParameters = { id: 0 };
      });

      it('returns 404', () => co(function* () {
        const res = yield handle(event, {});
        assert(res.statusCode === 404);
      }));
    });
  });
});
