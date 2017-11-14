'use strict';

const os = require('os');
const fs = require('fs');
const path = require('path');
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


const boot = require('./boot');

co(function *() {
  yield boot();

  repl.context.Base = require('../app/models/base');
  repl.context.Photo = require('../app/models/photo');

}).catch(err => {
  console.log(err);
});
