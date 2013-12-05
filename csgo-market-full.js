"use strict";

// Global variables
var url = "http://steamcommunity.com/market/search?q=appid%3A730";
var links = [];
var totalPages = 0;
var scrapeDirectory = "./csgo-pages/";

// Casper settings
var casper = require("casper").create({
    verbose: true,
    pageSettings: {
        loadImages: false, // Load pages quicker
        userAgent: "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:25.0) Gecko/20100101 Firefox/25.0"
    }
});

// FileSystem module
var fs = require("fs");

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

casper.parsePage = function(page, results) {
    fs.write(scrapeDirectory + page, results.outerHTML, "w");
};

casper.getCurrentPage = function() {
    return this.evaluate(function() {
        return parseInt(document.getElementsByClassName("market_paging_pagelink active")[0].textContent);
    });
};

casper.getTotalPages = function() {
    return this.evaluate(function() {
        return parseInt(document.getElementById("searchResults_links").children[6].textContent);
    });
};

// Start of program
// Checks if directory exists and creates directory if it doesn't
casper.start(url, function() {
    if(!fs.isDirectory(scrapeDirectory)) {
        fs.makeDirectory(scrapeDirectory);
    }

    // Get total pages
    casper.waitFor(casper.pageReady, function() { 
        totalPages = this.getTotalPages();
        this.echo(totalPages + " pages to scrape.");
    });

    for(var i = 1; i <= 5; i++) {
        links.push(url + "#p" + i);
    }
});

casper.run();
