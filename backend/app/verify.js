class AppResourceNotFoundError extends Error {
  constructor() {
    super('Resource not found');
    this.name = 'AppResourceNotFoundError';
    Error.captureStackTrace(this, this.constructor);
  }
}

class AppAuthenticationFailed extends Error {
  constructor() {
    super('Authentication failed');
    this.name = 'AppAuthenticationFailed';
    Error.captureStackTrace(this, this.constructor);
  }
}

module.exports.presence = (x) => {
  if (!x) {
    throw new AppResourceNotFoundError();
  }
  return x;
};

module.exports.authention = (x) => {
  if (!x) {
    throw new AppAuthenticationFailed();
  }
  return x;
};
