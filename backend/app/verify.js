'use strict';

class AppResourceNotFoundError extends Error {
  constructor() {
    super('Resource not found');
    this.name = 'AppResourceNotFoundError';
    Error.captureStackTrace(this, this.constructor);
  }
};

module.exports.presence = (x) => {
  if (!x) {
    throw new AppResourceNotFoundError();
  }
  return x;
};
