const _ = require('lodash');
const co = require('co');
const promisify = require('util.promisify');
const assert = require('power-assert');
const sinon = require('sinon');
const proxyquire = require('proxyquire');

const { URL } = require('url');

const helper = require('test/test-helper');
const fixture = require('test/fixture');
const factory = require('test/factory');

describe('my/photos/presigned_post', () => {
  let User;
  let Photo;
  before(() => co(function* () {
    const model = proxyquire('app/model', helper.stub);
    const m = yield model.with(_.identity);
    User = m.User;
    Photo = m.Photo;
  }));

  let handler;
  before(() => {
    handler = proxyquire('app/my/photos/presigned_post/handler', helper.stub);
  });

  let user;
  let event;
  beforeEach(() => co(function* () {
    user = yield factory.create(User);
    event = {
      requestContext: {
        authorizer: {
          userId: user.id.toString(),
        },
      },
      body: '{}',
    };
  }));

  afterEach(() => co(function* () {
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
        filename: 'image.jpg',
      };

      beforeEach(() => {
        event.body = JSON.stringify(attrs);
      });

      it('returns presigned post attrs', () => co(function* () {
        const res = yield handle(event, {});
        assert(res.statusCode === 200);

        const body = JSON.parse(res.body);
        assert(body.url === `http://localhost:${process.env.S3_PORT}/higaidar-test-photos`);

        const { fields } = body;
        assert(_.keys(fields).length === 7);
        assert(fields.key.match(new RegExp(`^${user.id}/.*/image\\.jpg$`)));
        assert(fields.bucket === 'higaidar-test-photos');
        assert(fields['X-Amz-Algorithm']);
        assert(fields['X-Amz-Credential']);
        assert(fields['X-Amz-Date']);
        assert(fields['X-Amz-Signature']);
        assert(fields.Policy);
      }));
    });
  });
});
