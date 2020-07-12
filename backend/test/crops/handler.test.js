const _ = require('lodash');
const co = require('co');
const promisify = require('util.promisify');
const assert = require('power-assert');
const sinon = require('sinon');
const proxyquire = require('proxyquire');

const helper = require('test/test-helper');
const fixture = require('test/fixture');
const factory = require('test/factory');

describe('crops', () => {
  let Crop;
  before(() => co(function* () {
    const model = proxyquire('app/model', helper.stub);
    const m = yield model.with(_.identity);
    Crop = m.Crop;
  }));

  let handler;
  before(() => {
    handler = proxyquire('app/crops/handler', helper.stub);
  });

  let event;
  beforeEach(() => co(function* () {
    event = {
      body: '{}',
    };
  }));

  afterEach(() => co(function* () {
    yield Crop.destroy({ where: {} });
  }));

  describe('#index', () => {
    let handle;
    beforeEach(() => {
      handle = promisify(handler.index.bind(handler));
    });

    context('without params', () => {
      it('returns crops ordered by id asc', () => co(function* () {
        const crops = yield factory.createList(Crop, 2);
        const n = yield Crop.count();
        assert(n === 2);

        const res = yield handle(event, {});
        assert(res.statusCode === 200);

        const body = JSON.parse(res.body);
        assert(helper.isEqualModel(body[0], crops[0]));
        assert(helper.isEqualModel(body[1], crops[1]));
      }));

      it('returns content range', () => co(function* () {
        const crops = yield factory.createList(Crop, 7);
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
        const crops = yield factory.createList(Crop, 7);
        const res = yield handle(event, {});
        assert(res.headers['Content-Range'] === '2-4/7');

        const body = JSON.parse(res.body);
        assert(body.length === 3);
        assert(helper.isEqualModel(body[0], crops[2]));
        assert(helper.isEqualModel(body[2], crops[4]));
      }));
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

      it('creates a new crop', () => co(function* () {
        const org = yield Crop.count();
        yield handle(event, {});
        const cur = yield Crop.count();
        assert(org === 0);
        assert(cur === 1);

        const crop = yield Crop.findOne();
        assert(_.isMatch(crop.dataValues, attrs));
      }));

      it('returns attributes of the new crop', () => co(function* () {
        const res = yield handle(event, {});
        assert(res.statusCode === 200);

        const body = JSON.parse(res.body);
        assert(_.isMatch(body, attrs));
      }));
    });
  });

  describe('#destroy', () => {
    let handle;
    let crop;
    beforeEach(() => co(function* () {
      handle = promisify(handler.destroy.bind(handler));

      crop = yield factory.create(Crop);
      event.httpMethod = 'DELETE';
      event.pathParameters = { id: crop.id };
    }));

    context('with valid id', () => {
      it('deletes the crop', () => co(function* () {
        const org = yield Crop.count();
        yield handle(event, {});
        const cur = yield Crop.count();
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
