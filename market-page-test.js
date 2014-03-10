"use strict";

// Casper settings
casper.options.timeout = 15000;
casper.options.pageSettings = {
        loadImages: false, // Load pages quicker
        userAgent: "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:25.0)" +
        + " Gecko/20100101 Firefox/25.0"
    }

casper.test.begin("Steam market page has all scraped items",
    function suite(test) {
    casper.start("http://steamcommunity.com/market/search?q=appid%3A730",
        function () {
            test.assertExists("a.market_listing_row_link", "Listings found");
            test.assertExists({
                type: "xpath",
                path: "//a[@class='market_listing_row_link' and @href]"
            }, "URL found");
            test.assertExists({
                type: "xpath",
                path: "//img[@class='market_listing_item_img' and @src]"
            }, "Image found");
            test.assertExists("div.market_listing_num_listings",
                "Quantity and Price found");
            test.assertExists("span.market_listing_item_name",
                "Item name and colour found");
            test.assertExists("span.market_listing_game_name",
                "Game name found");
        });

    casper.run(function() {
        test.done();
    });
});
