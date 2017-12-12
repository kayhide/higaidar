'use strict';

const handle = function* (body, opts) {
  if (body === null) {
    yield {
      statusCode: 204
    };
  }

  yield {
    statusCode: opts.statusCode || 200,
    body: JSON.stringify(body)
  };
};

module.exports = (callback) => (body, opts = {}) => {
  const res = handle(body, opts).next().value;
  res.headers = {
    "Access-Control-Allow-Origin": "*"
  };
  if (typeof opts.offset !== 'undefined') {
    const offset = opts.offset;
    const range = body.length > 0 ? `${offset}-${offset + body.length - 1}` : '*';
    const total = (opts.total !== 'undefined') ? opts.total : '*';
    res.headers['Range-Unit'] = 'items';
    res.headers['Content-Range'] = `${range}/${total}`;
  };
  callback(null, res);
};
