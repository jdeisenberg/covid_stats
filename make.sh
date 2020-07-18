#!/bin/bash

npm run build
rm -rf dist
parcel build src/index.html src/*csv --public-url ./ --no-minify
