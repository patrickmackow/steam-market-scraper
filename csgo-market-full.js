var url = "http://steamcommunity.com/market/search?q=appid%3A730#p102"

// Casper settings
var casper = require("casper").create({
    pageSettings: {
        loadImages: false, // Load pages quicker
        userAgent: "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:25.0) Gecko/20100101 Firefox/25.0"
    }
});

casper.start(url);

casper.then(function() {
    this.waitUntilVisible("a.market_listing_row_link", function() {
        this.echo(this.getHTML("a.market_listing_row_link"));
    });
});

casper.run();
