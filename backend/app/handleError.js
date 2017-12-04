'use strict';

const handle = function* (err) {
  if (err.name === 'SequelizeValidationError') {
    yield {
      statusCode: 400,
      body: { message: 'Validation failed', messages: err.errors.map(e => e.message) }
    };
  }
  if (err.name === 'AppResourceNotFoundError') {
    yield {
      statusCode: 404,
      body: { message: 'Resource not found' }
    };
  }

  yield {
    statusCode: 500,
    body: err
  }
};

module.exports = (callback) => (err) => {
  const res = handle(err).next().value;
  callback(null, { statusCode: res.statusCode, body: JSON.stringify(res.body) });
};
