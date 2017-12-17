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


describe('pests', () => {
  let Pest;
  before(() => co(function *(){
    const model = proxyquire('app/model', helper.stub);
    const m = yield model.with(_.identity);
    Pest = m.Pest;
  }));

  let handler;
  before(() => {
    handler = proxyquire('app/pests/handler', helper.stub);
  });

  let event;
  beforeEach(() => co(function *(){
    event = {
      body: '{}'
    };
  }));

  afterEach(() => co(function *(){
    yield Pest.destroy({ where: {} });
  }));


  describe('#index', () => {
    let handle;
    beforeEach(() => {
      handle = promisify(handler.index.bind(handler));
    });

    context('without params', () => {
      it('returns pests ordered by id asc', () => {
        return co(function *() {
          const pests = yield factory.createList(Pest, 2);
          const n = yield Pest.count();
          assert(n === 2);

          const res = yield handle(event, {})
          assert(res.statusCode === 200);

          const body = JSON.parse(res.body);
          assert(helper.isEqualModel(body[0], pests[0]));
          assert(helper.isEqualModel(body[1], pests[1]));
        });
      });

      it('returns content range', () => {
        return co(function *() {
          const pests = yield factory.createList(Pest, 7);
          const res = yield handle(event, {})
          assert(res.headers['Content-Range'] === '0-6/7');
        });
      });
    });

    context('with paging params', () => {
      it('offsets and limits', () => {
        return co(function *() {
          event.queryStringParameters = {
            offset: 2,
            limit: 3
          };
          const pests = yield factory.createList(Pest, 7);
          const res = yield handle(event, {})
          assert(res.headers['Content-Range'] === '2-4/7');

          const body = JSON.parse(res.body);
          assert(body.length === 3);
          assert(helper.isEqualModel(body[0], pests[2]));
          assert(helper.isEqualModel(body[2], pests[4]));
        });
      });
    });
  });

  describe('#create', () => {
    let handle;
    beforeEach(() => {
      handle = promisify(handler.create.bind(handler));
    });

    context('with valid attrs', () => {
      const attrs = {};

      beforeEach(() => {
        event.body = JSON.stringify(attrs);
      });

      it('creates a new pest', () => {
        return co(function *() {
          const org = yield Pest.count();
          yield handle(event, {});
          const cur = yield Pest.count();
          assert(org === 0);
          assert(cur === 1);

          const pest = yield Pest.findOne();
          assert(_.matches(pest.dataValues, attrs));
        });
      });

      it('returns attributes of the new pest', () => {
        return co(function *() {
          const res = yield handle(event, {});
          assert(res.statusCode === 200);

          const body = JSON.parse(res.body);
          assert(_.matches(body, attrs));
        });
      });
    });
  });

  describe('#destroy', () => {
    let handle;
    let pest;
    beforeEach(() => co(function *() {
      handle = promisify(handler.destroy.bind(handler));

      pest = yield factory.create(Pest);
      event.httpMethod = 'DELETE';
      event.pathParameters = { id: pest.id };
    }));

    context('with valid id', () => {
      it('deletes the pest', () => {
        return co(function *() {
          const org = yield Pest.count();
          yield handle(event, {});
          const cur = yield Pest.count();
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
});
