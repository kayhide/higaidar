'strict';

const helper = require('lib/helper.js');

const stage = process.env.STAGE || 'dev';
const vars = helper.readPublicEnv(stage);

exports.getAppEnv = function(key) {
  return vars[key];
};
