-- Table: hackage.build_status

-- DROP TABLE hackage.build_status;

CREATE TABLE hackage.build_status
(
    package_name character varying(255) COLLATE pg_catalog."default" NOT NULL,
    version character varying(100) COLLATE pg_catalog."default" NOT NULL,
    run_date date NOT NULL,
    ghc_version character varying(10) COLLATE pg_catalog."default" NOT NULL,
    build_status character varying(20) COLLATE pg_catalog."default",
    CONSTRAINT build_status_pkey PRIMARY KEY (package_name, version, run_date, ghc_version),
    CONSTRAINT build_status_fkey1 FOREIGN KEY (package_name, version, run_date)
        REFERENCES hackage.package_snapshot (package_name, version, run_date) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
)
WITH (
    OIDS = FALSE
)
TABLESPACE pg_default;

ALTER TABLE hackage.build_status
    OWNER to "torstenkemps-benedix";

-- Index: fki_build_status_fkey1

-- DROP INDEX hackage.fki_build_status_fkey1;

CREATE INDEX fki_build_status_fkey1
    ON hackage.build_status USING btree
    (package_name COLLATE pg_catalog."default", version COLLATE pg_catalog."default", run_date)
    TABLESPACE pg_default;
