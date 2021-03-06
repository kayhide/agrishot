'use strict';

const os = require('os');
const fs = require('fs');
const path = require('path');
const co = require('co');


if (!process.env.ON_REMOTE) {
  console.log('using localstack...');
}

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
    repl.context.Photo = require('app/models/photo');
    repl.context.Pest = require('app/models/pest');
    repl.context.Messenger = require('app/messenger');
  }
  else {
    const Localstack = require('lib/localstack');
    const proxyquire = require('proxyquire');
    const stub = {
      'aws-sdk': {
        DynamoDB: Localstack.DynamoDB,
        S3: Localstack.S3,
        '@global': true
      }
    }
    repl.context.Photo = proxyquire('app/models/photo', stub);
    repl.context.Pest = proxyquire('app/models/pest', stub);
    repl.context.Messenger = proxyquire('app/messenger', stub);
  }
};
