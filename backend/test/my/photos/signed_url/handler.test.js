'use strict';

const _ = require('lodash');
const co = require('co');
const promisify = require('util.promisify');
const assert = require('power-assert');
const sinon = require('sinon');
const proxyquire = require('proxyquire');

const URL = require('url').URL;

const helper = require('test/test-helper');
const fixture = require('test/fixture');
const factory = require('test/factory');


describe('my/photos/signed_url', () => {
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
    handler = proxyquire('app/my/photos/signed_url/handler', helper.stub);
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


  describe('#create', () => {
    let handle;
    beforeEach(() => {
      handle = promisify(handler.create.bind(handler));
    });

    context('with valid attrs', () => {
      const attrs = {
        filename: 'image.jpg'
      };

      beforeEach(() => {
        event.body = JSON.stringify(attrs);
      });

      it('returns signed url', () => {
        return co(function *() {
          const res = yield handle(event, {});
          assert(res.statusCode === 200);

          const body = JSON.parse(res.body);
          const url = new URL(body.url);
          assert(url.host === 'localhost:4572');
          assert(url.pathname.match(new RegExp(`^/higaidar-test-photos/${user.id}/.*/image\\.jpg$`)));

          assert(url.searchParams.get('AWSAccessKeyId') === '');
          assert(url.searchParams.get('Expires').match(/^\d+$/));
          assert(url.searchParams.get('Signature').match(/\w+=$/));
        });
      });
    });
  });
});
