var url = "http://steamcommunity.com/market/search?q=appid%3A730#p102"

var casper = require("casper").create({
    pageSettings: {
        //        loadImages: false,
        userAgent: "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:25.0) Gecko/20100101 Firefox/25.0"
    }
});

casper.start(url);

/*casper.then(function() {
  var ready = this.evaluate(function() {
  return document.readyState == "complete";
  });
  this.echo(ready);
  });*/

casper.then(function() {
    this.waitUntilVisible("a.market_listing_row_link", function() {
        this.capture('test.png');
        this.echo(this.getPageContent());
        this.echo(this.getHTML("div#searchResultsTable"));
    });
});



/*casper.then(function() {
  var js = this.evaluate(function() {
  return document.documentElement.outerHTML;
  });

  this.echo(js);
  });*/

casper.run();
