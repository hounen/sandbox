-- Table: m_bases

-- DROP TABLE m_bases;

CREATE TABLE m_bases
(
  baseyear integer NOT NULL,
  created_at timestamp without time zone,
  created_ip character varying(20),
  updated_at timestamp without time zone,
  updated_ip character varying(20),
  delflg character(1) NOT NULL,
  CONSTRAINT mbases_key PRIMARY KEY (baseyear)
) 
WITHOUT OIDS;
ALTER TABLE m_bases OWNER TO mdms;


-- Index: pk_m_base

-- DROP INDEX pk_m_base;

CREATE INDEX pk_m_base
  ON m_bases
  USING btree
  (baseyear);

