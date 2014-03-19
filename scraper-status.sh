#!/bin/bash

MARKET_TEST=market-page-test.js
LISTING_TEST=market-listing-test.js
DATE=$(date)
EMAIL="patrickmackow@gmail.com"

for i in {1..5}
do
    FAILED=true
    casperjs test $MARKET_TEST
    if [ $? -eq 0 ]
    then
        FAILED=false
        break
    fi
done

if [ "$FAILED" == true ]
then
    echo "ERROR: Market page test failed 5 times $DATE" \
        >> scraper_status.log
    tail -1 scraper_status.log | mail -s "MARKET TESTS FAILED" "$EMAIL"
    exit 1
fi

for i in {1..5}
do
    FAILED=true
    casperjs test $LISTING_TEST
    if [ $? -eq 0 ]
    then
        FAILED=false
        break
    fi
done

if [ "$FAILED" == true ]
then
    echo "ERROR: Listing page test failed 5 times $DATE" >> \
        scraper_status.log
    tail -1 scraper_status.log | mail -s "LISTING TESTS FAILED" "$EMAIL"
    exit 1
fi

echo "OK: Both test suites passed $DATE" >> scraper_status.log

exit
