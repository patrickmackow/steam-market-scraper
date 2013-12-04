var url = "http://steamcommunity.com/market/search?q=appid%3A730"

// Casper settings
var casper = require("casper").create({
    pageSettings: {
        loadImages: false, // Load pages quicker
        userAgent: "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:25.0) Gecko/20100101 Firefox/25.0"
    }
});

casper.start(url);

casper.waitFor(function() {
    var state = this.evaluate(function() {
        return document.readyState
    });

    return state == "complete"
}, function then() {
    var results = this.evaluate(function() {
        return document.getElementById("searchResults");
    });
    
    this.echo(results.outerHTML);
});

casper.run();
