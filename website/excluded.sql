DROP TABLE IF EXISTS excluded;

CREATE TABLE excluded (
    id serial PRIMARY KEY,
    underpriced_no text UNIQUE NOT NULL,
    excluded_timestamp timestamp DEFAULT CURRENT_TIMESTAMP
);
