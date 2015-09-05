CREATE TABLE stations (
  id bigserial PRIMARY KEY,
  name varchar UNIQUE NOT NULL,
  station_id bigint NOT NULL
);

CREATE TABLE connections (
  id bigserial PRIMARY KEY,
  delay integer NOT NULL,
  conn_id bigint NOT NULL,
  train_id varchar NOT NULL,
  arrival_time timestamp,

  station_id bigint REFERENCES stations(id) NOT NULL
);
