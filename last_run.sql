DROP TABLE IF EXISTS last_run;

CREATE TABLE last_run (
    id serial PRIMARY KEY,
    latest_timestamp text
);
