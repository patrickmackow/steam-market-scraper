#!/bin/bash

THREADS=300

./dist/build/currency-rates/currency-rates
if [ $? -eq 0 ]
then
    ./dist/build/steam-listing-scraper/steam-listing-scraper $THREADS
fi

exit
