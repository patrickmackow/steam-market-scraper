#!/bin/bash

THREADS=300

/home/baitmaster/projects/haskell/steam-market-scraper \
    /dist/build/currency-rates/currency-rates
if [ $? -eq 0 ]
then
    /home/baitmaster/projects/haskell/steam-market-scraper \
        /dist/build/steam-listing-scraper/steam-listing-scraper $THREADS
fi

exit
