"use strict";

var url = "http://steamcommunity.com/market/search?q=appid%3A730"
var links = [];
var totalPages = 0;

// Casper settings
var casper = require("casper").create({
    verbose: true,
    pageSettings: {
        loadImages: false, // Load pages quicker
        userAgent: "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:25.0) Gecko/20100101 Firefox/25.0"
    }
});

casper.pageReady = function() {
    var state = this.evaluate(function() {
        return document.readyState;
    });

    return state == "complete";
};

casper.getItemRows = function() {
    return this.evaluate(function() {
        return document.getElementById("searchResults");
    });
};

casper.getCurrentPage = function() {
    return this.evaluate(function() {
        return parseInt(docment.getElementsByClassName("market_paging_pagelink active")[0].textContent);
    });
};

casper.getTotalPages = function() {
    return this.evaluate(function() {
        return parseInt(document.getElementById("searchResults_links").children[6].textContent);
    });
};

casper.start(url);

casper.waitFor(casper.pageReady, function then() { // Get total pages
    totalPages = this.getTotalPages();
    this.echo(totalPages + " pages to scrape.");
});

casper.then(function() {
    for(var i = 1; i <= totalPages; i++) {
        links.push(url + "#p" + i);
    }
    this.echo(links[0]);
});

casper.then(function() {
    for (var page = 1; page <= 2; page++) {
        this.echo(page);
        var newUrl = url + "#p" + page;
        this.thenOpen(newUrl, function() {
            this.echo(page);
            this.waitFor(function() { // Wait for webpage to finish all AJAX calls
                var state = this.evaluate(function() {
                    return document.readyState
                });

                return state == "complete"
            }, function then() {
                // Get current page to avoid scraping page
                // if total page count is smaller than current page
                var currentPage = this.evaluate(function() {
                    return document.getElementsByClassName("market_paging_pagelink active")[0].textContent;
                });

                this.echo(page);

                if(parseInt(currentPage) == page) {
                    var results = this.evaluate(function() {
                        return document.getElementById("searchResults");
                    });

                    this.echo(results.outerHTML);
                } else {
                    this.echo("FALSE");
                }
            });
        });
    };
});

casper.then(function() {
    this.echo(links.length);
});

casper.run();
