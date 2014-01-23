-- Database: steam_market

-- DROP DATABASE steam_market;

CREATE TABLE market (
	id serial PRIMARY KEY,
	url text UNIQUE NOT NULL,
	image text NOT NULL,
	quantity int NOT NULL,
	price numeric(7,2) NOT NULL,
	item_name text NOT NULL,
	item_name_colour text NOT NULL,
	game text NOT NULL
);

CREATE TABLE market_history (
	id serial PRIMARY KEY,
	url text REFERENCES market,
	quantity int NOT NULL,
	price numeric(7,2) NOT NULL,
	market_timestamp timestamp DEFAULT CURRENT_TIMESTAMP
);

CREATE TRIGGER insert_market
	AFTER INSERT, UPDATE ON market
	FOR EACH ROW
	EXECUTE PROCEDURE insert_market_history(url, quantity, price);

CREATE FUNCTION insert_market_history
	