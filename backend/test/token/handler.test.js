const _ = require('lodash');
const co = require('co');
const promisify = require('util.promisify');
const assert = require('power-assert');
const proxyquire = require('proxyquire');
const jwt = require('jsonwebtoken');

const helper = require('test/test-helper');
const fixture = require('test/fixture');
const factory = require('test/factory');

describe('token', () => {
  let User;
  before(() => co(function* () {
    const model = proxyquire('app/model', helper.stub);
    const m = yield model.with(_.identity);
    User = m.User;
  }));

  afterEach(() => co(function* () {
    yield User.destroy({ where: {} });
  }));

  describe('#create', () => {
    let event;
    let handle;
    let user;
    const attrs = { code: '1703', tel: '03-3624-4700' };

    beforeEach(() => co(function* () {
      const handler = proxyquire('app/token/handler', helper.stub);
      handle = promisify(handler.create.bind(handler));

      user = yield factory.create(User, attrs);
      event = {};
      event.body = JSON.stringify(attrs);
    }));

    context('with valid attrs', () => {
      it('returns a token with user', () => co(function* () {
        const res = yield handle(event, {});
        assert(res.statusCode === 200);

        const body = JSON.parse(res.body);
        const user_ = body.user;
        assert(helper.isEqualModel(user_, user));

        const { token } = body;
        const decoded = jwt.verify(token, process.env.JWT_SECRET);
        assert(helper.isEqualModel(decoded.user, user));
      }));
    });

    context('with wrong attrs', () => {
      beforeEach(() => {
        event.body = JSON.stringify({ code: '9999', tel: '00-0000-0000' });
      });

      it('returns 401', () => co(function* () {
        const res = yield handle(event, {});
        assert(res.statusCode === 401);
      }));
    });

    context('with partially correct attrs', () => {
      beforeEach(() => {
        event.body = JSON.stringify({ code: '1703' });
      });

      it('returns 401', () => co(function* () {
        const res = yield handle(event, {});
        assert(res.statusCode === 401);
      }));
    });
  });

  describe('#authorize', () => {
    let event;
    let handle;
    let user;
    let token;

    beforeEach(() => co(function* () {
      const handler = proxyquire('app/token/handler', helper.stub);
      handle = promisify(handler.authorize.bind(handler));

      event = {};
      event.methodArn = 'arn:aws:execute-api:ap-northheast-1::api-id/test/GET/something';
    }));

    context('with valid token of ordinary user', () => {
      beforeEach(() => co(function* () {
        user = yield factory.create(User);
        token = jwt.sign(
          { user: user.dataValues },
          process.env.JWT_SECRET,
          { expiresIn: process.env.JWT_EXPIRES_IN },
        );
        event.authorizationToken = `Bearer ${token}`;
      }));

      it('generates policy', () => co(function* () {
        const res = yield handle(event, {});
        const statements = res.policyDocument.Statement;
        assert(statements.length === 3);
        let i;

        i = 0;
        assert(statements[i].Action === 'execute-api:Invoke');
        assert(statements[i].Effect === 'Allow');
        assert(_.isEqual(statements[i].Resource, [
          'arn:aws:execute-api:ap-northheast-1::api-id/test/GET/crops',
        ]));

        i = 1;
        assert(statements[i].Action === 'execute-api:Invoke');
        assert(statements[i].Effect === 'Allow');
        assert(_.isEqual(statements[i].Resource, [
          'arn:aws:execute-api:ap-northheast-1::api-id/test/GET/pests',
        ]));

        i = 2;
        assert(statements[i].Action === 'execute-api:Invoke');
        assert(statements[i].Effect === 'Allow');
        assert(_.isEqual(statements[i].Resource, [
          'arn:aws:execute-api:ap-northheast-1::api-id/test/*/my/*',
        ]));
      }));
    });

    context('with valid token of editor user', () => {
      beforeEach(() => co(function* () {
        user = yield factory.create(User, { is_editor: true });
        token = jwt.sign(
          { user: user.dataValues },
          process.env.JWT_SECRET,
          { expiresIn: process.env.JWT_EXPIRES_IN },
        );
        event.authorizationToken = `Bearer ${token}`;
      }));

      it('generates policy', () => co(function* () {
        const res = yield handle(event, {});
        const statements = res.policyDocument.Statement;
        assert(statements.length === 4);
        let i;

        i = 0;
        assert(statements[i].Action === 'execute-api:Invoke');
        assert(statements[i].Effect === 'Allow');
        assert(_.isEqual(statements[i].Resource, [
          'arn:aws:execute-api:ap-northheast-1::api-id/test/*/photos',
          'arn:aws:execute-api:ap-northheast-1::api-id/test/*/photos/*',
        ]));

        i = 1;
        assert(statements[i].Action === 'execute-api:Invoke');
        assert(statements[i].Effect === 'Allow');
        assert(_.isEqual(statements[i].Resource, [
          'arn:aws:execute-api:ap-northheast-1::api-id/test/*/crops',
          'arn:aws:execute-api:ap-northheast-1::api-id/test/*/crops/*',
        ]));

        i = 2;
        assert(statements[i].Action === 'execute-api:Invoke');
        assert(statements[i].Effect === 'Allow');
        assert(_.isEqual(statements[i].Resource, [
          'arn:aws:execute-api:ap-northheast-1::api-id/test/*/pests',
          'arn:aws:execute-api:ap-northheast-1::api-id/test/*/pests/*',
        ]));

        i = 3;
        assert(statements[i].Action === 'execute-api:Invoke');
        assert(statements[i].Effect === 'Allow');
        assert(_.isEqual(statements[i].Resource, [
          'arn:aws:execute-api:ap-northheast-1::api-id/test/*/my/*',
        ]));
      }));
    });

    context('with valid token of admin user', () => {
      beforeEach(() => co(function* () {
        user = yield factory.create(User, { is_admin: true });
        token = jwt.sign(
          { user: user.dataValues },
          process.env.JWT_SECRET,
          { expiresIn: process.env.JWT_EXPIRES_IN },
        );
        event.authorizationToken = `Bearer ${token}`;
      }));

      it('generates policy', () => co(function* () {
        const res = yield handle(event, {});
        const statements = res.policyDocument.Statement;
        assert(statements.length === 5);
        let i;

        i = 0;
        assert(statements[i].Action === 'execute-api:Invoke');
        assert(statements[i].Effect === 'Allow');
        assert(_.isEqual(statements[i].Resource, [
          'arn:aws:execute-api:ap-northheast-1::api-id/test/*/users',
          'arn:aws:execute-api:ap-northheast-1::api-id/test/*/users/*',
        ]));

        i = 1;
        assert(statements[i].Action === 'execute-api:Invoke');
        assert(statements[i].Effect === 'Allow');
        assert(_.isEqual(statements[i].Resource, [
          'arn:aws:execute-api:ap-northheast-1::api-id/test/*/photos',
          'arn:aws:execute-api:ap-northheast-1::api-id/test/*/photos/*',
        ]));

        i = 2;
        assert(statements[i].Action === 'execute-api:Invoke');
        assert(statements[i].Effect === 'Allow');
        assert(_.isEqual(statements[i].Resource, [
          'arn:aws:execute-api:ap-northheast-1::api-id/test/*/crops',
          'arn:aws:execute-api:ap-northheast-1::api-id/test/*/crops/*',
        ]));

        i = 3;
        assert(statements[i].Action === 'execute-api:Invoke');
        assert(statements[i].Effect === 'Allow');
        assert(_.isEqual(statements[i].Resource, [
          'arn:aws:execute-api:ap-northheast-1::api-id/test/*/pests',
          'arn:aws:execute-api:ap-northheast-1::api-id/test/*/pests/*',
        ]));

        i = 4;
        assert(statements[i].Action === 'execute-api:Invoke');
        assert(statements[i].Effect === 'Allow');
        assert(_.isEqual(statements[i].Resource, [
          'arn:aws:execute-api:ap-northheast-1::api-id/test/*/my/*',
        ]));
      }));
    });

    context('with invalid token', () => {
      it('unauthorizes', () => {
        event.authorizationToken = 'invalid';
        return co(function* () {
          yield handle(event, {});
        }).then(() => assert(false), (err) => {
          assert(err === 'Unauthorized');
        });
      });
    });

    context('with malformed token', () => {
      it('unauthorizes', () => {
        event.authorizationToken = token;
        return co(function* () {
          yield handle(event, {});
        }).then(() => assert(false), (err) => {
          assert(err === 'Unauthorized');
        });
      });
    });
  });
});
