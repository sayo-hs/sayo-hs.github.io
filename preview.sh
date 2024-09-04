#!/bin/sh

cd docs
docker run --rm -d --name preview-sayo-hs -v $PWD:/srv/jekyll -p 4000:4000 -it jekyll/jekyll sleep infinity
docker exec -it preview-sayo-hs bundle install
docker exec -it preview-sayo-hs bundle exec jekyll serve --host 0.0.0.0
docker kill preview-sayo-hs
