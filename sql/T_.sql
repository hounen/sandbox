-- Table: t_docfileinfos

-- DROP TABLE t_docfileinfos;

CREATE TABLE t_docfileinfos
(
  fileno integer NOT NULL,
  makepostname character varying(40) NOT NULL,
  makesectionname character varying(40) NOT NULL,
  hknpostcd character(3) NOT NULL,
  hknsectioncd character(3) NOT NULL,
  bigclasscd character(2) NOT NULL,
  middleclasscd character(3) NOT NULL,
  smallclasscd character(3) NOT NULL,
  classcd character(1) NOT NULL,
  filename character varying(160) NOT NULL,
  bindyear date NOT NULL,
  stryear date NOT NULL,
  hznkkncd character(2) NOT NULL,
  hznkkn integer,
  haikiyoteidate date NOT NULL,
  shishocd character(1) NOT NULL,
  einenkbn character(1) NOT NULL,
  boxno integer NOT NULL,
  haikijshidate date NOT NULL,
  biko character varying(160),
  created_at timestamp without time zone,
  created_ip character varying(20),
  updated_at timestamp without time zone,
  updated_ip character varying(20),
  haikiflg character(1) NOT NULL,
  delflg character(1) NOT NULL,
  CONSTRAINT tdocfile_key PRIMARY KEY (fileno)
) 
WITHOUT OIDS;
ALTER TABLE t_docfileinfos OWNER TO mdms;


-- Index: pk_t_doc_file_infos

-- DROP INDEX pk_t_doc_file_infos;

CREATE INDEX pk_t_doc_file_infos
  ON t_docfileinfos
  USING btree
  (fileno);
