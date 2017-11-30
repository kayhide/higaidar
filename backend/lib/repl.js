'use strict';

const os = require('os');
const fs = require('fs');
const path = require('path');
const yaml = require('js-yaml');
const co = require('co');


const repl = require('repl').start({ useGlobal: true });

const history = path.join(os.homedir(), '.node_repl_history');

fs.statSync(history);
fs.readFileSync(history, 'utf-8')
  .split('\n')
  .reverse()
  .filter(line => line.trim())
  .map(line => repl.history.push(line))

repl.on('exit', () => {
  fs.appendFileSync(history, repl.lines.join('\n') + '\n');
});

repl.context.display = (err, data) => {
  err ? console.log(err) : console.log(data);
}
repl.context.capture = (err, data) => {
  repl.context.it = err ? err : data;
}


const boot = require('lib/boot');

co(function *() {
  yield boot();

  load();
}).catch(err => {
  console.log(err);
});


function load() {
  repl.context['_'] = require('lodash');
  if (process.env.ON_REMOTE) {
  }
  else {
    const db_config = yaml.safeLoad(fs.readFileSync(path.join(__dirname, '../../db/config.yml')));
    const c = db_config[process.env.STAGE];

    const Sequelize = require('sequelize');
    const sequelize = new Sequelize(c.database, c.username, c.password, {
      host: 'localhost',
      dialect: { mysql2: 'mysql' }[c.adapter] || c.adapter
    })

    repl.context.User = require('app/model/user').define(sequelize);
  }
};
