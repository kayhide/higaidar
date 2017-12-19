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
const Localstack = require('lib/localstack');

const s3 = new Localstack.S3();

describe('my/photos', () => {
  let User;
  let Photo;
  before(() => co(function *(){
    const model = proxyquire('app/model', helper.stub);
    const m = yield model.with(_.identity);
    User = m.User;
    Photo = m.Photo;
  }));

  let handler;
  before(() => {
    handler = proxyquire('app/my/photos/handler', helper.stub);
  });

  let user;
  let event;
  beforeEach(() => co(function *(){
    user = yield factory.create(User);
    event = {
      requestContext: {
        authorizer: {
          userId: user.id.toString()
        }
      },
      body: '{}'
    };
  }));

  afterEach(() => co(function *(){
    yield Photo.destroy({ where: {} });
    yield User.destroy({ where: {} });
  }));


  describe('#index', () => {
    let handle;
    beforeEach(() => {
      handle = promisify(handler.index.bind(handler));
    });

    context('without params', () => {
      it('returns photos ordered by id desc', () => {
        return co(function *() {
          const photos = yield factory.createList(Photo, 2, { user_id: user.id });
          const photos_ = _.reverse(photos)
          const n = yield Photo.count();
          assert(n === 2);

          const res = yield handle(event, {})
          assert(res.statusCode === 200);

          const body = JSON.parse(res.body);
          assert(helper.isEqualModel(body[0], photos_[0]));
          assert(helper.isEqualModel(body[1], photos_[1]));
        });
      });

      it('returns content range', () => {
        return co(function *() {
          const photos = yield factory.createList(Photo, 7, { user_id: user.id });
          const res = yield handle(event, {})
          assert(res.headers['Content-Range'] === '0-6/7');
        });
      });

      it('returns only my photos', () => {
        return co(function *() {
          yield factory.createList(Photo, 2, { user_id: user.id });

          const somebody = yield factory.createList(User);
          yield factory.createList(Photo, 2, { user_id: somebody.id });

          const n = yield Photo.count();
          assert(n === 4);

          const res = yield handle(event, {})
          assert(res.headers['Content-Range'] === '0-1/2');
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
          const photos = yield factory.createList(Photo, 7, { user_id: user.id });
          const photos_ = _.reverse(photos);
          const res = yield handle(event, {})
          assert(res.headers['Content-Range'] === '2-4/7');

          const body = JSON.parse(res.body);
          assert(body.length === 3);
          assert(helper.isEqualModel(body[0], photos_[2]));
          assert(helper.isEqualModel(body[2], photos_[4]));
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

      it('creates a new photo', () => {
        return co(function *() {
          const org = yield Photo.count();
          yield handle(event, {});
          const cur = yield Photo.count();
          assert(org === 0);
          assert(cur === 1);

          const photo = yield Photo.findOne();
          assert(_.matches(photo.dataValues, attrs));
        });
      });

      it('returns attributes of the new photo', () => {
        return co(function *() {
          const res = yield handle(event, {});
          assert(res.statusCode === 200);

          const body = JSON.parse(res.body);
          assert(_.matches(body, attrs));
        });
      });
    });
  });

  describe('#show', () => {
    let handle;
    let photo;
    beforeEach(() => co(function *() {
      handle = promisify(handler.show.bind(handler));

      photo = yield factory.create(Photo, { user_id: user.id });
      event.pathParameters = { id: photo.id };
    }));


    context('with valid attrs', () => {
      it('returns attributes of the photo', () => {
        return co(function *() {
          const res = yield handle(event, {});
          assert(res.statusCode === 200);

          const body = JSON.parse(res.body);
          assert(helper.isEqualModel(body, photo));
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
    let handle;
    let photo;
    beforeEach(() => co(function *() {
      handle = promisify(handler.update.bind(handler));

      photo = yield factory.create(Photo, { user_id: user.id });
      event.httpMethod = 'PATCH';
      event.pathParameters = { id: photo.id };
    }));

    context('with valid attrs', () => {
      const attrs = {
      };

      beforeEach(() => {
        event.body = JSON.stringify(attrs);
      });

      it('returns attributes of the photo', () => {
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
  });

  describe('#destroy', () => {
    let handle;
    let photo;
    beforeEach(() => co(function *() {
      handle = promisify(handler.destroy.bind(handler));

      photo = yield factory.create(Photo, { user_id: user.id });
      event.httpMethod = 'DELETE';
      event.pathParameters = { id: photo.id };
    }));

    context('with valid id', () => {
      it('deletes the photo', () => {
        return co(function *() {
          const org = yield Photo.count();
          yield handle(event, {});
          const cur = yield Photo.count();
          assert(org === 1);
          assert(cur === 0);
        });
      });

      it('returns 204', () => {
        return co(function *() {
          const res = yield handle(event, {});
          console.log(res);
          assert(res.statusCode === 204);
        });
      });

      it('deletes uploaded files', () => {
        const srcBucket = 'higaidar-test-photos';
        const dstBucket = `${srcBucket}-thumbnail`;
        const file = 'cute_cat.jpg';
        const key = `${user.id}/japan/tokyo/nyappori/${file}`;
        return co(function *() {
          yield helper.setS3File(s3, srcBucket, key, file);
          yield helper.setS3File(s3, dstBucket, key, file);
          const original = yield helper.exists(s3, srcBucket, key);
          assert(original);
          const thumbnail = yield helper.exists(s3, dstBucket, key);
          assert(thumbnail);
          yield photo.update({ key });
          yield handle(event, {});
          const original_ = yield helper.exists(s3, srcBucket, key);
          assert(!original_);
          const thumbnail_ = yield helper.exists(s3, dstBucket, key);
          assert(!thumbnail_);
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
