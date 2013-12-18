"use strict";

// Global variables
//var url = "http://steamcommunity.com/market/search?q=appid%3A730";

// Casper settings
var casper = require("casper").create({
    pageSettings: {
        loadImages: false, // Load pages quicker
        userAgent: "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:25.0) Gecko/20100101 Firefox/25.0"
    }
});

// First arguement is url
var url = casper.cli.get(0);

// FileSystem module
var fs = require("fs");

casper.pageReady = function() {
    var state = this.evaluate(function() {
        return document.readyState;
    });

    return state == "complete";
};

casper.getTotalPages = function() {
    return this.evaluate(function() {
        return parseInt(document.getElementById("searchResults_links").children[6].textContent);
    });
};

// Start of program
// Checks if directory exists and creates directory if it doesn't
casper.start(url, function() {
    // Get total pages
    this.waitFor(casper.pageReady, function() { 
        var totalPages = this.getTotalPages();
        this.echo(totalPages);
    });
});

casper.run();
