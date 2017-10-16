## Setup for local development

### NPM modules

```sh
yarn install
```

### Environment variables

We need env values as written in `.envrc.sample`.

Make sure to give those env values when running tests or deploying.

Recommended way to manage env values is to use [direnv](https://github.com/direnv/direnv).

```sh
$ cp .envrc.sample .envrc
# edit .envrc
$ direnv allow
```

### LocalStack

We use [LocalStack](https://github.com/localstack/localstack) to simulate AWS to work on a local environment.

To install:

```sh
$ pip install localstack
```

## Running tests

Start LocalStack:

```sh
$ yarn run localstack
```

LocalStack is required when we run tests.

Run tests:

```sh
$ yarn run test
```

Or run tests automatically when file changes:

```sh
$ yarn run test-watch
```

## AWS configs

Prepare AWS configs for deployments.

For each stage the following profiles are required:

- **development stage**: `agrishot-dev-deploy`
- **production stage**: `agrishot-prod-deploy`

### Steps to prepare AWS configs 

1. Open AWS IAM console on the browser.
2. Create new users with names, `agrishot-dev-deploy`, `agrishot-prod-deploy`.
3. Create access keys to access programatically.
4. Attach `AdministratorAccess` to them.
5. Update local `~/.aws/config` adding profiles corresponding to IAM users.
6. Update local `~/.aws/credentials` adding access keys for each profiles.

## Deploying

Backend is managed by [Serverless](https://serverless.com/).

To deploy:

```sh
$ yarn run deploy
```
