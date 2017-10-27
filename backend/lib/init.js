'use strict';

const co = require('co');

const inits = {
  resources: require('./init/resources')
}

process.env.STAGE = process.env.STAGE || 'dev';

co(function *() {
  for (let x in inits) {
    console.log(`  init: ${x}`)
    yield inits[x].run()
  }
}).catch((err) => {
  console.log('Failed:');
  console.log(err);
});
