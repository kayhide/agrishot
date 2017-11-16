'use strict';

const co = require('co');

const boot = require('./boot');

const inits = {
  resources: require('./init/resources')
}

process.env.STAGE = process.env.STAGE || 'dev';

co(function *() {
  yield boot();
  for (let x in inits) {
    console.log(`  init: ${x}`)
    yield inits[x].run()
  }
}).catch((err) => {
  console.log('Failed:');
  console.log(err);
});
