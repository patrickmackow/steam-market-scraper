"use strict";

// Global variables
//var url = "http://steamcommunity.com/market/search?q=appid%3A730";

// Casper settings
var casper = require("casper").create({
    //verbose: true,
    logLevel: "debug",
    pageSettings: {
        loadImages: false, // Load pages quicker
        userAgent: "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:25.0)" +
        + " Gecko/20100101 Firefox/25.0"
    }
});

var url = casper.cli.get(0);
var scrapeDirectory = casper.cli.get(1);

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
        return parseInt(document
            .getElementsByClassName("market_paging_pagelink active")[0]
            .textContent);
    });
};

casper.getTotalPages = function() {
    return this.evaluate(function() {
        return parseInt(document
            .getElementById("searchResults_links")
            .children[6].textContent);
    });
};

// Start of program
// Checks if directory exists and creates directory if it doesn't
casper.start(url, function() {
    // Add '/' to the end of directory path 
    // so files get written to proper location
    if(scrapeDirectory.lastIndexOf("/") !== (scrapeDirectory.length - 1)) {
        scrapeDirectory = scrapeDirectory + "/";
    }
    if(!fs.isDirectory(scrapeDirectory)) {
        fs.makeDirectory(scrapeDirectory);
    }

    // Get total pages
    this.waitFor(casper.pageReady, function() { 
        var page = 0;
        if (url.indexOf("#p") !== -1) {
            page = url.split("#p")[1];
        } else {
            // TODO: This is a listing pages
        }

        if (this.getCurrentPage() == page) {
            this.parsePage(page, this.getItemRows());
        } else {
            // TODO: Add some kind of failure
            this.log("Current page and url page do not match", "error");
            this.log("Current page: " + this.getCurrentPage(), "error");
            this.log("Url page: " + page, "error");
        }
    });
});

casper.run();
