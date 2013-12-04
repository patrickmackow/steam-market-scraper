var url = "http://steamcommunity.com/market/search?q=appid%3A730"

// Casper settings
var casper = require("casper").create({
    pageSettings: {
        loadImages: false, // Load pages quicker
        userAgent: "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:25.0) Gecko/20100101 Firefox/25.0"
    }
});

casper.start(url);

var totalPages = 0;

casper.waitFor(function() {
    var state = this.evaluate(function() {
        return document.readyState
    });

    return state == "complete"
}, function then() {
    totalPagesString = this.evaluate(function() {
        return document.getElementById("searchResults_links").children[6].textContent;
    });
    
    this.echo(parseInt(totalPagesString));
});

casper.then(function() {
    var results = this.evaluate(function() {
        return document.getElementById("searchResults");
    });
});

casper.run();
