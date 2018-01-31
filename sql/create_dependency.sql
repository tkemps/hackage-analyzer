-- Table: hackage.dependency

-- DROP TABLE hackage.dependency;

CREATE TABLE hackage.dependency
(
    package_name character varying(255) COLLATE pg_catalog."default" NOT NULL,
    version character varying(100) COLLATE pg_catalog."default" NOT NULL,
    run_date date NOT NULL,
    package_name_dependency character varying(255) COLLATE pg_catalog."default" NOT NULL,
    version_dependency character varying(100) COLLATE pg_catalog."default",
    CONSTRAINT dependency_pkey PRIMARY KEY (package_name, version, run_date, package_name_dependency),
    CONSTRAINT dependency_fkey1 FOREIGN KEY (package_name, version, run_date)
        REFERENCES hackage.package_snapshot (package_name, version, run_date) MATCH SIMPLE
        ON UPDATE NO ACTION
        ON DELETE NO ACTION
)
WITH (
    OIDS = FALSE
)
TABLESPACE pg_default;

ALTER TABLE hackage.dependency
    OWNER to "torstenkemps-benedix";

-- Index: fki_dependency_fkey1

-- DROP INDEX hackage.fki_dependency_fkey1;

CREATE INDEX fki_dependency_fkey1
    ON hackage.dependency USING btree
    (package_name COLLATE pg_catalog."default", version COLLATE pg_catalog."default", run_date)
    TABLESPACE pg_default;

-- Index: ix_dependency_01

-- DROP INDEX hackage.ix_dependency_01;

CREATE INDEX ix_dependency_01
    ON hackage.dependency USING btree
    (package_name_dependency COLLATE pg_catalog."default", version_dependency COLLATE pg_catalog."default")
    TABLESPACE pg_default;
