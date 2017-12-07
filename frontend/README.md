## Setup for local development

### NPM modules

```sh
$ yarn install
```

## Running dev server

Start dev server:

```sh
$ yarn run dev
```

This script starts 2 servers:

- `webpack` for bundling
- `live-server` for browser auto reloading

To make bundling work, we need to compile par file.
For this, use psc-ide via editor's support.
Or use pscid from console.

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
