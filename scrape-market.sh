#!/bin/bash

THREADS=300

cd $HOME/projects/haskell/steam-market-scraper/

./dist/build/steam-market-scraper/steam-market-scraper $THREADS

exit
