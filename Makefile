####
# DOCKER
####
docker_build:
	docker build -t shiny-explore-dataset .

docker_compose:
	docker compose -f docker-compose.yml up --build

run:
	open 'http://localhost:3838'

docker_shiny:
	docker compose -f docker-compose.yml up --build shiny

zsh:
	docker exec -it shiny-explore-dataset-bash-1 /bin/zsh

tests:
	R --quiet -e "testthat::test_dir('/code/shiny-explore-dataset/unit_tests')"
