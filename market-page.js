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
var timeout = casper.cli.get(1);
var scrapeDirectory = casper.cli.get(2);

casper.options.timeout = timeout;

// FileSystem module
var fs = require("fs");

casper.pageReady = function() {
    var state = this.evaluate(function() {
        return document.readyState;
    });

    return state == "complete";
};

casper.getMarketRows = function() {
    return this.evaluate(function() {
        return document.getElementById("searchResults");
    });
};

casper.getListingRows = function() {
    return this.evaluate(function() {
        return document.getElementById("searchResultsRows");
    });
};

casper.parsePage = function(page, results) {
    if(results.outerHTML != undefined) {
        // Append url to the top of the page if it is a listing page
        if (scrapeDirectory === "/dev/null/") {
            fs.write("/dev/null", results.outerHTML, "w");
        }
        else if (url.indexOf("/listings/") !== -1) {
            fs.write(scrapeDirectory + page + ".html", url, "w");
            fs.write(scrapeDirectory + page + ".html", "\n", "a");
            fs.write(scrapeDirectory + page + ".html",
                    results.outerHTML, "a");
        } else {
            fs.write(scrapeDirectory + page + ".html",
                    results.outerHTML, "w");
        }
    }
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
        // Check if page if listing or market
        if (url.indexOf("/listings/") !== -1) {
            var filename = url.split("/listings/")[1].split("/")[1];
            this.parsePage(filename, this.getListingRows());
        } else {
            var page = 0;
            if (url.indexOf("#p") !== -1) {
                page = url.split("#p")[1];
            } else {
                // TODO: This is a listing pages
            }

            if (this.getCurrentPage() == page) {
                this.parsePage(page, this.getMarketRows());
            } else {
                // TODO: Add some kind of failure
                this.log("Current page and url page do not match", "error");
                this.log("Current page: " + this.getCurrentPage(), "error");
                this.log("Url page: " + page, "error");
            }
        }
    });
});

casper.run();
