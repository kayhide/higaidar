## Development

Prerequistes:
- ruby
- nodejs
- mysql
- localstack

### Using nix and direnv

Consume `shell.nix`.

If using `direnv`, copy `.envrc.sample` to `.envrc` and edit so to fit your environment.


### Start depending services

Start db and localstack by:

```console
$ make dev
```

This starts docker containers in which services are running.

For the first time, you need to initialize database with the following command:

```console
$ rake db:reset
$ STAGE=test rake db:reset
```

Note that the `STAGE` variable specifies which envirionment the operation targets.

### Run backend tests

```console
$ cd backend
$ yarn test
```

## Deploy for the first

Fill env vars on `.env.{ENV}.yml` copying `.env.test.yml`.

For dev stage:

```sh
$ cp .env.test.yml .env.dev.yml
$ vim .env.dev.yml
( edit )
```

### RDS setup

Create an instance on RDS and collect infomations about the created one:

```sh
$ rake rds:create
( wait for database to be ready )
$ rake rds:pull
```

The following command trys to connect to the database with `mysql` command.
Be sure the security group of the RDS instance is properly set.

Create an user on the database for lambda access:

```sh
$ rake rds:create_user
```

Create database and load the database schema.
Migration related commands require `ON_REMOTE=1` for RDS database, otherwise commands apply on local database.

```sh
$ ON_REMOTE=1 rake db:create
$ ON_REMOTE=1 rake db:schema:load
```

### API Gateway setup

Deploy backend:

```sh
$ cd backend
$ yarn deploy
```

Collect informations:

```sh
$ cd ..
$ rake apigateway:pull
```

Edit `.env.{ENV}.yml` and fill `API_ENDPOINT`:

```sh
$ vim .env.dev.yml
( edit )
```

### S3 setup

Create S3 bucket for frontend:

```sh
$ rake s3:create
```

Deploy frontend:

```sh
$ cd frontend
$ yarn deploy
```

### Route53 setup

Configure domain routing for the frontend.

If using Route53, creating an alias to the corresponding S3 bucket is fine.

## Deploy update

- Migrate database
- Deploy backend
- Deploy frontend

```sh
$ ON_REMOTE=1 rake db:migrate
$ (cd backend && yarn deploy)
$ (cd frontend && yarn deploy)
```

By default, these commands target on `dev` stage.

When deploying to `prod`, set `STAGE=prod` and run the same commands.
