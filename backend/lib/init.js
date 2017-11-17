'use strict';

const co = require('co');

const boot = require('lib/boot');

const inits = {
  resources: require('lib/init/resources')
}

process.env.STAGE = process.env.STAGE || 'dev';

co(function *() {
  const config = yield boot();
  for (let x in inits) {
    console.log(`  ${process.env.STAGE} init: ${x}`)
    yield inits[x].run(config)
  }
}).catch((err) => {
  console.log('Failed:');
  console.log(err);
});
