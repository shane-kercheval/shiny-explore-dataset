####
# DOCKER
####

docker_compose:
	docker compose -f docker-compose.yml up --build

docker_rebuild:
	docker compose -f docker-compose.yml build --no-cache

run:
	open 'http://localhost:3838'

docker_shiny:
	docker compose -f docker-compose.yml up --build shiny

zsh:
	docker exec -it shiny-explore-dataset-bash-1 /bin/zsh

tests:
	R --quiet -e "testthat::test_dir('/code/shiny-explore-dataset/unit_tests')"
