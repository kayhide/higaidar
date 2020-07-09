COMPOSE_PROJECT_NAME := $(shell basename $(shell pwd))
COMPOSE_COMMAND := docker-compose


dev:
	${COMPOSE_COMMAND} up -d
	@$(MAKE) --no-print-directory envs
	@sleep 5
	@(source .env/ports && cd backend && yarn localstack:setup:all)
.PHONY: dev

down:
	${COMPOSE_COMMAND} down
	@echo > .env/ports
.PHONY: down

envs: DB_PORT := $(shell ${COMPOSE_COMMAND} port db 3306 | cut -d ':' -f 2)
envs: S3_PORT := $(shell ${COMPOSE_COMMAND} port localstack 4572 | cut -d ':' -f 2)
envs: CLOUDFORMATION_PORT := $(shell ${COMPOSE_COMMAND} port localstack 4581 | cut -d ':' -f 2)
envs:
	@mkdir -p .env
	@rm -f .env/ports
	@echo "export DB_PORT=${DB_PORT}" >> .env/ports
	@echo "export S3_PORT=${S3_PORT}" >> .env/ports
	@echo "export CLOUDFORMATION_PORT=${CLOUDFORMATION_PORT}" >> .env/ports
.PHONY: envs
