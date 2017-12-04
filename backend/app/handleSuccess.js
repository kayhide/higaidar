'use strict';

const handle = function* (body, opts) {
  if (body === null) {
    yield {
      statusCode: 204
    };
  }

  yield {
    statusCode: opts.statusCode || 200,
    body: body
  };
};

module.exports = (callback) => (body, opts = {}) => {
  const res = handle(body, opts).next().value;
  if (res.body) {
    callback(null, { statusCode: res.statusCode, body: JSON.stringify(res.body) });
  }
  else {
    callback(null, { statusCode: res.statusCode });
  }
};
