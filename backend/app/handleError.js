const handle = function* (err) {
  if (err.name === 'SequelizeValidationError') {
    yield {
      statusCode: 400,
      body: { message: 'Validation failed', messages: err.errors.map((e) => e.message) },
    };
  }
  if (err.name === 'AppResourceNotFoundError') {
    yield {
      statusCode: 404,
      body: { message: err.message },
    };
  }
  if (err.name === 'AppAuthenticationFailed') {
    yield {
      statusCode: 401,
      body: { message: err.message },
    };
  }

  console.log(err);
  yield {
    statusCode: 500,
    body: {},
  };
};

module.exports = (callback) => (err) => {
  const res = handle(err).next().value;
  res.headers = {
    'Access-Control-Allow-Origin': '*',
  };
  res.body = JSON.stringify(res.body);
  callback(null, res);
};
