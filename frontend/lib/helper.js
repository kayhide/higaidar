'use strict';

const fs = require('fs');
const path = require('path');
const yaml = require('js-yaml');
const _ = require('lodash');

module.exports = {
  availableStages: ['dev', 'prod'],

  verifyStage(stage) {
    if (!this.availableStages.includes(stage)) {
      throw new Error(`Invalid STAGE: ${stage}`);
    }
  },

  envFile(stage) {
    const basename = `.env.${stage}.yml`;
    return this.dir.ascendings(process.env.PWD)
      .map((d) => path.join(d, basename))
      .find((f) => fs.existsSync(f));
  },

  readEnv(stage) {
    return yaml.safeLoad(fs.readFileSync(this.envFile(stage)));
  },

  readPublicEnv(stage){
    const env = this.readEnv(stage);
    return _.pick(env, env.public_keys);
  },

  dir: {
    ascendings(dir) {
      return Array.from(function *() {
        let last;
        while (!last || dir != last) {
          yield dir;
          last = dir;
          dir = path.dirname(dir);
        }
      }());
    }
  }
}
