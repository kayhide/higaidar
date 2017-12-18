'use strict';

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

describe('photos', () => {
  let User;
  let Photo;
  before(() => co(function *(){
    const model = proxyquire('app/model', helper.stub);
    const m = yield model.with(_.identity);
    User = m.User;
    Photo = m.Photo;
  }));

  afterEach(() => co(function *(){
    yield Photo.destroy({ where: {} });
    yield User.destroy({ where: {} });
  }));

  let handler;
  beforeEach(() => {
    handler = proxyquire('app/photos/handler', helper.stub);
  });

  let user;
  let event;
  beforeEach(() => co(function *(){
    user = yield factory.create(User);
    event = {};
  }));

  describe('#accept', () => {
    const srcBucket = 'higaidar-test-photos';
    const dstBucket = `${srcBucket}-thumbnail`;

    let handle;
    beforeEach(() => {
      handle = promisify(handler.accept.bind(handler));
      event.Records = [{
        s3: {
          bucket: {
            name: srcBucket
          }
        }
      }];
    });

    context('with image', () => {
      const file = 'cute_cat.jpg';
      let key;
      beforeEach(() => co(function *() {
        key = `${user.id}/japan/tokyo/nyappori/${file}`;
        event.Records[0].s3.object = { key };

        yield helper.setS3File(s3, srcBucket, key, file);
        yield helper.unsetS3File(s3, dstBucket, key);
      }));

      it('creates thumbnail on s3', () => {
        return co(function *() {
          const org = yield helper.exists(s3, dstBucket, key);
          assert(!org);
          yield handle(event, {});
          const cur = yield helper.exists(s3, dstBucket, key);
          assert(cur);
        });
      });

      it('creates a new Photo', () => {
        return co(function *() {
          const org = yield Photo.count();
          yield handle(event, {});
          const cur = yield Photo.count();
          assert(cur - org === 1);

          const photos = yield user.getPhotos();
          const photo = photos[0];
          assert(photo.user_id === user.id);
          assert(photo.key === key);
          assert(photo.original_url === `https://higaidar-test-photos.s3.ap-northeast-1.amazonaws.com/${key}`);
          assert(photo.thumbnail_url === `https://higaidar-test-photos-thumbnail.s3.ap-northeast-1.amazonaws.com/${key}`);
        });
      });
    });

    context('with jpeg image', () => {
      const file = 'sleepy_cat.jpeg';
      let key;
      beforeEach(() => co(function *() {
        key = `${user.id}/japan/tokyo/nyappori/${file}`;
        event.Records[0].s3.object = { key };

        yield helper.setS3File(s3, srcBucket, key, file);
        yield helper.unsetS3File(s3, dstBucket, key);
      }));

      it('works', () => {
        return handle(event, {});
      });
    });

    context('with png image', () => {
      const file = 'boss.png';
      let key;
      beforeEach(() => co(function *() {
        key = `${user.id}/japan/tokyo/nyappori/${file}`;
        event.Records[0].s3.object = { key };

        yield helper.setS3File(s3, srcBucket, key, file);
        yield helper.unsetS3File(s3, dstBucket, key);
      }));

      it('works', () => {
        return handle(event, {});
      });
    });
  });
});
