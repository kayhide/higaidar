'use strict';

const os = require('os');
const fs = require('fs');
const path = require('path');
const yaml = require('js-yaml');
const co = require('co');
const proxyquire = require('proxyquire');


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

  const stub = Object.assign(
    {},
    require('lib/connStub').stub
  );
  const model = proxyquire('app/model', stub);
  model.with(m => Object.assign(repl.context, m));
};
