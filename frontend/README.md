## Setup for local development

### NPM modules

```sh
$ yarn install
```

## Running dev server

Start LocalStack from backend script:

```sh
$ cd ../backend
$ yarn run localstack
```

Go back to frontend dir and start dev server:

```sh
$ yarn run dev
```

This script starts 2 servers:

- `webpack` for bundling
- `live-server` for browser auto reloading

To make bundling work, we need to compile par file.
For this, we use pscid.
Somehow pscid does not work via yarn script, start it directly.

```sh
$ pscid
```

## Build and deploy

First, we need to compile purescript modules using `pulp` command.

Then, we collect generated js and integrate them to a site along with static pages by `webpack`.

```sh
$ yarn run build 
```

To deploy built site to AWS S3 bucket:

```sh
$ yarn run deploy
```

For `prod` stage:

```sh
$ STAGE=prod yarn run deploy
```
