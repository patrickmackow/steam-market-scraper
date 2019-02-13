var time = 5;
var interval = -1;
var scrape = -1;
var timestamp = "";
var timeSince;
var initialCheck = false;

function load() {
    var radios = document.getElementsByName("quantity");
    for (var i = 0; i < radios.length; i++) {
        if (radios[i].checked) {
            checked = radios[i].value;
        }
    }

    var labels = document.getElementsByTagName("input");
    for (var i = 0; i < labels.length; i++) {
        labels[i].addEventListener("change", changed, false);
    }

    loadListings();

    for (var i = 0; i < radios.length; i++) {
        if (radios[i].checked) {
            if (radios[i].value === "all") {
                createTimer();
            } else if (radios[i].value === "single") {
                createTimer();
            }
            break;
        }
    }
};

function createTimer() {
    // Update status every second, update listings every 5
    if (interval === -1) {
        interval = window.setInterval(function() {
            var count = document.getElementById("count");
            
            if (time !== 1) {
                time--;
                count.innerHTML = time;
            }
            else {
                loadListings();
                time = 5;
                count.innerHTML = time;
            }
        }, 1000);
    }
}

function changed() {
    var listings = document.getElementById("listings");
    listings.innerHTML = "";
    
    loadListings();

    var radios = document.getElementsByName("quantity");
    for (var i = 0; i < radios.length; i++) {
        if (radios[i].checked) {
            if (radios[i].value === "all") {
                createTimer();
            } else if (radios[i].value === "single") {
                createTimer();
            } else if (radios[i].value === "excluded") {
                clearInterval(interval);
                interval = -1;
            }
            break;
        }
    }
}

function excludeListing(no) {
    var listing = document.getElementById(no);
    listing.className += " hidden";

    var httpRequest;

    if (window.XMLHttpRequest) {
        httpRequest = new XMLHttpRequest();
    }

    httpRequest.open("PUT", "/sms/excluded/" + no);
    httpRequest.send();
}

function deleteExclude(no) {
    var listing = document.getElementById(no);
    listing.className += " hidden";

    var httpRequest;

    if (window.XMLHttpRequest) {
        httpRequest = new XMLHttpRequest();
    }

    httpRequest.open("DELETE", "/sms/excluded/" + no);
    httpRequest.send();
}

function loadListings() {
    var httpRequest;
    var listings = document.getElementById("listings");
    var lastListing = 0;

    if (window.XMLHttpRequest) {
        httpRequest = new XMLHttpRequest();
    }

    if (listings.childElementCount !== 0) {
        var lastElement = listings.lastChild;
        lastListing = lastElement.firstChild.innerHTML;
    }

    compareTimestamps();

    httpRequest.onreadystatechange = displayListings;
    var radios = document.getElementsByName("quantity");
    for (var i = 0; i < radios.length; i++) {
        if (radios[i].checked) {
            if (radios[i].value === "all") {
                if (lastListing !== 0) {
                    httpRequest.open("GET", "/sms/underpriced/all/"
                        + lastListing);
                } else {
                    httpRequest.open("GET", "/sms/underpriced/all/");
                }
            } else if (radios[i].value === "single") {
                if (lastListing !== 0) {
                    httpRequest.open("GET", "/sms/underpriced/single/"
                        + lastListing);
                } else {
                    httpRequest.open("GET", "/sms/underpriced/single/");
                }
            } else if (radios[i].value === "excluded") {
                httpRequest.onreadystatechange = displayExcluded;
                httpRequest.open("GET", "/sms/excluded/");
            }
            break;
        }
    }
    httpRequest.send();

    function compareTimestamps() {
        var httpRequest;

        if (window.XMLHttpRequest) {
            httpRequest = new XMLHttpRequest();
        }

        httpRequest.onreadystatechange = function() {
            if (httpRequest.readyState === 4) {
                if (httpRequest.status === 200) {
                    var response = JSON.parse(httpRequest.responseText);

                    var responseText = "";
                    if (response.length !== 0) {
                        responseText = response[0].timestamp;
                    }

                    var count = document.getElementById("scrape");
                    timeSince = 60 - Math.round((Math.round(+new Date()/1000)
                        - parseInt(responseText))/60);
                    count.innerHTML = timeSince;

                    if (timestamp !== responseText) {
                        timestamp = responseText;
                        var listings = document.getElementById("listings");
                        listings.innerHTML = "";
                    }

                    scrapeTimer();

                    if (!initialCheck) {
                        checkIfNeedsScrape();
                    }
                } else {
                    alert("Ajax Error");
                }
            }};

        httpRequest.open("GET", "/sms/last/");
        httpRequest.send();
    }

    function scrapeTimer() {
        // Update status every second, update listings every 5
        if (scrape === -1) {
            scrape = window.setInterval(function() {
                if (timeSince <= 0) {
                    var httpRequest;

                    if (window.XMLHttpRequest) {
                        httpRequest = new XMLHttpRequest();
                    }

                    httpRequest.open("POST", "/sms/scrape/");
                    httpRequest.send();
                }
            }, 20000);
        }
    }

    function checkIfNeedsScrape() {
        if (!initialCheck) {
            if (timeSince <= 0) {
                var httpRequest;

                if (window.XMLHttpRequest) {
                    httpRequest = new XMLHttpRequest();
                }

                httpRequest.open("POST", "/sms/scrape/");
                httpRequest.send();
            }
            initialCheck = true;
        }
    }

    function displayListings() {
        if (httpRequest.readyState === 4) {
            if (httpRequest.status === 200) {
                var response = JSON.parse(httpRequest.responseText);

                response.forEach(function(element, index, array) {
                    display(element, "listing");
                });
            } else {
                alert("Ajax Error");
            }
        }
    }

    function displayExcluded() {
        clearInterval(interval);

        if (httpRequest.readyState === 4) {
            if (httpRequest.status === 200) {
                var response = JSON.parse(httpRequest.responseText);

                response.forEach(function(element, index, array) {
                    display(element, "excluded");
                });
            }
            else {
                alert("Ajax Error");
            }
        }
    }

    function display(element, type) {
        var listing = document.createElement("div");
        listing.id = element.listingNo;

        var hidden = document.createElement("div");
        hidden.className = "hidden";
        hidden.innerHTML = element.itemId;

        var link = document.createElement("a");
        link.className = "link";
        link.setAttribute("target", "_blank");
        link.setAttribute("href", "http://anonym.to/?"
            + element.url);

        var content = document.createElement("div");
        content.className = "listing";
        content.setAttribute("style", "border: 2px solid "
            + element.itemNameColour);

        var name = document.createElement("div");
        name.className = "name";
        name.setAttribute("style", "color: "
            + element.itemNameColour);
        name.innerHTML = element.itemName;

        var price = document.createElement("div");
        price.className = "price";
        price.innerHTML = "$" + (element.price / 100).toFixed(2)
            + "<br>";

        var profit = document.createElement("div");
        profit.className = "profit";
        profit.innerHTML = "PROFIT: $"
            + (element.profit / 100).toFixed(2);

        var exclude = document.createElement("span");
        if (type === "listing") {
            exclude.setAttribute("onclick", "excludeListing('"
                + element.listingNo + "')");
        } else if (type === "excluded") {
            exclude.setAttribute("onclick", "deleteExclude('"
                + element.listingNo + "')");
        }
        exclude.className = "exclude";
        exclude.innerHTML = "X";

        content.appendChild(name);
        content.appendChild(price);
        content.innerHTML += "<br>";
        content.appendChild(profit);

        link.appendChild(content);

        listing.appendChild(hidden);
        listing.appendChild(link);
        listing.appendChild(exclude);

        listings.appendChild(listing);
    }
};
