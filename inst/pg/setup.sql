-- CREATE DATABASE PEM;
-- SET search_path TO PEM;
-- CREATE EXTENSION postgis;
-- DROP SCHEMA IF EXISTS staging CASCADE;
-- DROP SCHEMA IF EXISTS transects CASCADE;
-- CREATE SCHEMA IF NOT EXISTS transects;
-- CREATE SCHEMA IF NOT EXISTS staging;
CREATE TABLE transects.field_data_points (
	"id"          SERIAL PRIMARY KEY,
	--placemark   varchar(255),
	"order"       integer,
	date_ymd      varchar(10),
	time_hms      varchar(8),
	transect_id   varchar(255),
	observer      varchar(255),
	point_type    varchar(255),
	mapunit1      varchar(255),
	transition    varchar(255),
	mapunit2      varchar(255),
	struc_stage   varchar(255),
	struc_mod     varchar(1),
	edatope       varchar(255),
	"comments"    varchar(255),
	photos        varchar(255), -- should this be file link?
	data_type     varchar(255),
	geom          geometry --points

);
CREATE INDEX idx_field_data_points on transects.field_data_points(id);
CREATE TABLE transects.field_tracklog (
	"id"          SERIAL PRIMARY KEY,
	transect_id   varchar(255),
	date_ymd      varchar(10),
	time_hms      varchar(8),
	photos        varchar(255), -- should this be file link?
	"comments"    varchar(255),
	data_type     varchar(255),
	geom          geometry --lines
);
CREATE INDEX idx_field_tracklog on transects.field_tracklog(id);
SELECT * INTO staging.field_data_points FROM transects.field_data_points;
CREATE INDEX idx_staging_field_data_points on transects.field_data_points(id);
SELECT * INTO staging.field_tracklog FROM transects.field_tracklog;
CREATE INDEX idx_staging_field_tracklog on transects.field_tracklog(id);
ALTER TABLE transects.field_data_points
ADD COLUMN staging_id integer not null;
ALTER TABLE transects.field_tracklog
ADD COLUMN staging_id integer NOT NULL;
--CREATE USER <uid> LOGIN PASSWORD <pwd>;
GRANT USAGE ON SCHEMA staging to <uid>;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA staging to <uid>;
GRANT USAGE ON SCHEMA transects to <uid>;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA transects to <uid>;

CREATE TABLE transects.sample_plan (
	"id"          SERIAL PRIMARY KEY,
	mapunit       varchar(255),
	geom          geometry --lines
);
