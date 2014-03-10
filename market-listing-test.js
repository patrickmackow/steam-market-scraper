"use strict";

// Casper settings
casper.options.timeout = 15000;
casper.options.pageSettings = {
        loadImages: false, // Load pages quicker
        userAgent: "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:25.0)" +
        + " Gecko/20100101 Firefox/25.0"
    }

casper.test.begin("Steam listing page has all scraped items",
    function suite(test) {
    casper.start("http://steamcommunity.com/market/listings/730/Sticker%20Capsule",
        function () {
            test.assertExists("div.market_listing_row", "Listing exists");
            test.assertExists({
                type: "xpath",
                path: "//*[starts-with(@id, 'listing_')]"
            }, "Listing number exists");
            test.assertExists("span.market_listing_price.market_listing_price_with_fee",
                "Price with fee found");
            test.assertExists("span.market_listing_price.market_listing_price_without_fee",
                "Price without fee found");
        });

    casper.run(function() {
        test.done();
    });
});
