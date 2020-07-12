const _ = require('lodash');
const fs = require('fs');
const co = require('co');
const promisify = require('util.promisify');
const assert = require('power-assert');
const sinon = require('sinon');
const proxyquire = require('proxyquire');
const nock = require('nock');

const helper = require('test/test-helper');
const fixture = require('test/fixture');
const factory = require('test/factory');
const Localstack = require('lib/localstack');

const s3 = new Localstack.S3();

describe('photos/thumbnail', () => {
  let User;
  let Photo;
  before(() => co(function* () {
    const model = proxyquire('app/model', helper.stub);
    const m = yield model.with(_.identity);
    User = m.User;
    Photo = m.Photo;
  }));

  afterEach(() => co(function* () {
    yield Photo.destroy({ where: {} });
    yield User.destroy({ where: {} });
  }));

  let handler;
  beforeEach(() => {
    handler = proxyquire('app/photos/thumbnail/handler', helper.stub);
  });

  let user;
  let event;
  beforeEach(() => co(function* () {
    user = yield factory.create(User);
    event = {
      body: '{}',
    };
  }));

  describe('#create', () => {
    const srcBucket = 'higaidar-test-photos';
    const dstBucket = `${srcBucket}-thumbnail`;
    const file = 'cute_cat.jpg';

    let handle;
    let photo;
    let key;
    beforeEach(() => co(function* () {
      handle = promisify(handler.create.bind(handler));

      key = `${user.id}/japan/tokyo/nyappori/${file}`;
      photo = yield factory.create(Photo, {
        user_id: user.id,
        key,
        original_url: `https://higaidar-test-photos.s3.ap-northeast-1.amazonaws.com/${key}`,
        thumbnail_url: `https://higaidar-test-photos-thumbnail.s3.ap-northeast-1.amazonaws.com/${key}`,
      });
      event.pathParameters = { id: photo.id };

      yield helper.setS3File(s3, srcBucket, key, file);
      yield helper.unsetS3File(s3, dstBucket, key);
    }));

    it('creates thumbnail on s3', () => co(function* () {
      const org = yield helper.exists(s3, dstBucket, key);
      assert(!org);
      yield handle(event, {});
      const cur = yield helper.exists(s3, dstBucket, key);
      assert(cur);
    }));
  });
});
