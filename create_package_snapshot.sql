-- Table: hackage.package_snapshot

-- DROP TABLE hackage.package_snapshot;

CREATE TABLE hackage.package_snapshot
(
    package_name character varying(255) COLLATE pg_catalog."default" NOT NULL,
    version character varying(100) COLLATE pg_catalog."default" NOT NULL,
    run_date date NOT NULL,
    date_time_snapshot timestamp without time zone,
    downloads_total bigint,
    downloads_30days bigint,
    tags character varying COLLATE pg_catalog."default",
    authors text COLLATE pg_catalog."default",
    maintainers text COLLATE pg_catalog."default",
    homepage character varying(255) COLLATE pg_catalog."default",
    bug_tracker character varying(255) COLLATE pg_catalog."default",
    source_repository character varying(255) COLLATE pg_catalog."default",
    uploaded timestamp without time zone,
    rating double precision,
    size bigint,
    extensions_declared character varying[] COLLATE pg_catalog."default",
    versions character varying[] COLLATE pg_catalog."default",
    link character varying COLLATE pg_catalog."default",
    description text COLLATE pg_catalog."default",
    categories character varying[] COLLATE pg_catalog."default",
    CONSTRAINT package_snapshot_pkey PRIMARY KEY (package_name, version, run_date)
)
WITH (
    OIDS = FALSE
)
TABLESPACE pg_default;

ALTER TABLE hackage.package_snapshot
    OWNER to "torstenkemps-benedix";
