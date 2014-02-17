DROP TABLE IF EXISTS underpriced;
DROP TABLE IF EXISTS listing;

CREATE TABLE listing (
    id serial PRIMARY KEY,
    listing_no text NOT NULL,
    url text REFERENCES market (url),
    item_price numeric(7,2) NOT NULL,
    item_price_before_fee numeric(7,2) NOT NULL,
    listing_timestamp timestamp DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE underpriced (
    id serial PRIMARY KEY,
    listing_id int REFERENCES listing (id),
    listing_no text UNIQUE NOT NULL,
    url text REFERENCES market (url),
    item_price numeric(7,2) NOT NULL,
    item_price_before_fee numeric(7,2) NOT NULL,
    listing_timestamp timestamp DEFAULT CURRENT_TIMESTAMP
);
