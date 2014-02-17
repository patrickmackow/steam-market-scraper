-- Database: steam_market

-- DROP DATABASE steam_market;
DROP TRIGGER IF EXISTS insert_market ON market;
DROP FUNCTION IF EXISTS insert_market_history();
DROP TABLE IF EXISTS market_history;
DROP TABLE IF EXISTS market CASCADE;

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
	url text REFERENCES market (url),
	quantity int NOT NULL,
	price numeric(7,2) NOT NULL,
	market_timestamp timestamp DEFAULT CURRENT_TIMESTAMP
);

CREATE FUNCTION insert_market_history() RETURNS trigger AS $$
	BEGIN
		INSERT INTO market_history (url, quantity, price) 
			VALUES (NEW.url, NEW.quantity, NEW.price);
		RETURN NULL;
	END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER insert_market
	AFTER INSERT OR UPDATE ON market
	FOR EACH ROW
	EXECUTE PROCEDURE insert_market_history();
