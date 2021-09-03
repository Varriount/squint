import std/strutils

import npeg


type Keyword* = distinct string


proc `==`*(a: Keyword, b: string): bool =
  result = cmpIgnoreCase(string(a), b) == 0

proc `==`*(a: string, b: Keyword): bool =
  result = cmpIgnoreCase(a, string(b)) == 0


grammar "postgresql":
  KEYWORD(name) <- [Keyword(name)]

  ABORT_P       <- KEYWORD("ABORT_P")
  ABSOLUTE_P    <- KEYWORD("ABSOLUTE_P")
  ACCESS        <- KEYWORD("ACCESS")
  ACTION        <- KEYWORD("ACTION")
  ADD_P         <- KEYWORD("ADD_P")
  ADMIN         <- KEYWORD("ADMIN")
  AFTER         <- KEYWORD("AFTER")
  AGGREGATE     <- KEYWORD("AGGREGATE")
  ALL           <- KEYWORD("ALL")
  ALSO          <- KEYWORD("ALSO")
  ALTER         <- KEYWORD("ALTER")
  ALWAYS        <- KEYWORD("ALWAYS")
  ANALYSE       <- KEYWORD("ANALYSE")
  ANALYZE       <- KEYWORD("ANALYZE")
  AND           <- KEYWORD("AND")
  ANY           <- KEYWORD("ANY")
  ARRAY         <- KEYWORD("ARRAY")
  AS            <- KEYWORD("AS")
  ASC           <- KEYWORD("ASC")
  ASENSITIVE    <- KEYWORD("ASENSITIVE")
  ASSERTION     <- KEYWORD("ASSERTION")
  ASSIGNMENT    <- KEYWORD("ASSIGNMENT")
  ASYMMETRIC    <- KEYWORD("ASYMMETRIC")
  ATOMIC        <- KEYWORD("ATOMIC")
  AT            <- KEYWORD("AT")
  ATTACH        <- KEYWORD("ATTACH")
  ATTRIBUTE     <- KEYWORD("ATTRIBUTE")
  AUTHORIZATION <- KEYWORD("AUTHORIZATION")

  BACKWARD  <- KEYWORD("BACKWARD")
  BEFORE    <- KEYWORD("BEFORE")
  BEGIN_P   <- KEYWORD("BEGIN_P")
  BETWEEN   <- KEYWORD("BETWEEN")
  BIGINT    <- KEYWORD("BIGINT")
  BINARY    <- KEYWORD("BINARY")
  BIT       <- KEYWORD("BIT")
  BOOLEAN_P <- KEYWORD("BOOLEAN_P")
  BOTH      <- KEYWORD("BOTH")
  BREADTH   <- KEYWORD("BREADTH")
  BY        <- KEYWORD("BY")

  CACHE     <- KEYWORD("CACHE")
  CALL      <- KEYWORD("CALL")
  CALLED    <- KEYWORD("CALLED")
  CASCADE   <- KEYWORD("CASCADE")
  CASCADED  <- KEYWORD("CASCADED")
  CASE      <- KEYWORD("CASE")
  CAST      <- KEYWORD("CAST")
  CATALOG_P <- KEYWORD("CATALOG_P")
  CHAIN     <- KEYWORD("CHAIN")
  CHAR_P    <- KEYWORD("CHAR_P")

  CHARACTER       <- KEYWORD("CHARACTER")
  CHARACTERISTICS <- KEYWORD("CHARACTERISTICS")
  CHECK           <- KEYWORD("CHECK")
  CHECKPOINT      <- KEYWORD("CHECKPOINT")
  CLASS           <- KEYWORD("CLASS")
  CLOSE           <- KEYWORD("CLOSE")

  CLUSTER   <- KEYWORD("CLUSTER")
  COALESCE  <- KEYWORD("COALESCE")
  COLLATE   <- KEYWORD("COLLATE")
  COLLATION <- KEYWORD("COLLATION")
  COLUMN    <- KEYWORD("COLUMN")
  COLUMNS   <- KEYWORD("COLUMNS")
  COMMENT   <- KEYWORD("COMMENT")
  COMMENTS  <- KEYWORD("COMMENTS")
  COMMIT    <- KEYWORD("COMMIT")

  COMMITTED     <- KEYWORD("COMMITTED")
  COMPRESSION   <- KEYWORD("COMPRESSION")
  CONCURRENTLY  <- KEYWORD("CONCURRENTLY")
  CONFIGURATION <- KEYWORD("CONFIGURATION")
  CONFLICT      <- KEYWORD("CONFLICT")

  CONNECTION   <- KEYWORD("CONNECTION")
  CONSTRAINT   <- KEYWORD("CONSTRAINT")
  CONSTRAINTS  <- KEYWORD("CONSTRAINTS")
  CONTENT_P    <- KEYWORD("CONTENT_P")
  CONTINUE_P   <- KEYWORD("CONTINUE_P")
  CONVERSION_P <- KEYWORD("CONVERSION_P")
  COPY         <- KEYWORD("COPY")

  COST      <- KEYWORD("COST")
  CREATE    <- KEYWORD("CREATE")
  CROSS     <- KEYWORD("CROSS")
  CSV       <- KEYWORD("CSV")
  CUBE      <- KEYWORD("CUBE")
  CURRENT_P <- KEYWORD("CURRENT_P")

  CURRENT_CATALOG <- KEYWORD("CURRENT_CATALOG")
  CURRENT_DATE    <- KEYWORD("CURRENT_DATE")
  CURRENT_ROLE    <- KEYWORD("CURRENT_ROLE")
  CURRENT_SCHEMA  <- KEYWORD("CURRENT_SCHEMA")

  CURRENT_TIME      <- KEYWORD("CURRENT_TIME")
  CURRENT_TIMESTAMP <- KEYWORD("CURRENT_TIMESTAMP")
  CURRENT_USER      <- KEYWORD("CURRENT_USER")
  CURSOR            <- KEYWORD("CURSOR")
  CYCLE             <- KEYWORD("CYCLE")

  DATA_P     <- KEYWORD("DATA_P")
  DATABASE   <- KEYWORD("DATABASE")
  DAY_P      <- KEYWORD("DAY_P")
  DEALLOCATE <- KEYWORD("DEALLOCATE")
  DEC        <- KEYWORD("DEC")
  DECIMAL_P  <- KEYWORD("DECIMAL_P")
  DECLARE    <- KEYWORD("DECLARE")
  DEFAULT    <- KEYWORD("DEFAULT")
  DEFAULTS   <- KEYWORD("DEFAULTS")

  DEFERRABLE <- KEYWORD("DEFERRABLE")
  DEFERRED   <- KEYWORD("DEFERRED")
  DEFINER    <- KEYWORD("DEFINER")
  DELETE_P   <- KEYWORD("DELETE_P")
  DELIMITER  <- KEYWORD("DELIMITER")
  DELIMITERS <- KEYWORD("DELIMITERS")
  DEPENDS    <- KEYWORD("DEPENDS")
  DEPTH      <- KEYWORD("DEPTH")
  DESC       <- KEYWORD("DESC")

  DETACH     <- KEYWORD("DETACH")
  DICTIONARY <- KEYWORD("DICTIONARY")
  DISABLE_P  <- KEYWORD("DISABLE_P")
  DISCARD    <- KEYWORD("DISCARD")
  DISTINCT   <- KEYWORD("DISTINCT")
  DO         <- KEYWORD("DO")
  DOCUMENT_P <- KEYWORD("DOCUMENT_P")
  DOMAIN_P   <- KEYWORD("DOMAIN_P")

  DOUBLE_P <- KEYWORD("DOUBLE_P")
  DROP     <- KEYWORD("DROP")

  EACH      <- KEYWORD("EACH")
  ELSE      <- KEYWORD("ELSE")
  ENABLE_P  <- KEYWORD("ENABLE_P")
  ENCODING  <- KEYWORD("ENCODING")
  ENCRYPTED <- KEYWORD("ENCRYPTED")
  END_P     <- KEYWORD("END_P")
  ENUM_P    <- KEYWORD("ENUM_P")
  ESCAPE    <- KEYWORD("ESCAPE")
  EVENT     <- KEYWORD("EVENT")
  EXCEPT    <- KEYWORD("EXCEPT")

  EXCLUDE    <- KEYWORD("EXCLUDE")
  EXCLUDING  <- KEYWORD("EXCLUDING")
  EXCLUSIVE  <- KEYWORD("EXCLUSIVE")
  EXECUTE    <- KEYWORD("EXECUTE")
  EXISTS     <- KEYWORD("EXISTS")
  EXPLAIN    <- KEYWORD("EXPLAIN")
  EXPRESSION <- KEYWORD("EXPRESSION")

  EXTENSION <- KEYWORD("EXTENSION")
  EXTERNAL  <- KEYWORD("EXTERNAL")
  EXTRACT   <- KEYWORD("EXTRACT")

  FALSE_P   <- KEYWORD("FALSE_P")
  FAMILY    <- KEYWORD("FAMILY")
  FETCH     <- KEYWORD("FETCH")
  FILTER    <- KEYWORD("FILTER")
  FINALIZE  <- KEYWORD("FINALIZE")
  FIRST_P   <- KEYWORD("FIRST_P")
  FLOAT_P   <- KEYWORD("FLOAT_P")
  FOLLOWING <- KEYWORD("FOLLOWING")
  FOR       <- KEYWORD("FOR")

  FORCE     <- KEYWORD("FORCE")
  FOREIGN   <- KEYWORD("FOREIGN")
  FORWARD   <- KEYWORD("FORWARD")
  FREEZE    <- KEYWORD("FREEZE")
  FROM      <- KEYWORD("FROM")
  FULL      <- KEYWORD("FULL")
  FUNCTION  <- KEYWORD("FUNCTION")
  FUNCTIONS <- KEYWORD("FUNCTIONS")

  GENERATED <- KEYWORD("GENERATED")
  GLOBAL    <- KEYWORD("GLOBAL")
  GRANT     <- KEYWORD("GRANT")
  GRANTED   <- KEYWORD("GRANTED")
  GREATEST  <- KEYWORD("GREATEST")
  GROUP_P   <- KEYWORD("GROUP_P")
  GROUPING  <- KEYWORD("GROUPING")
  GROUPS    <- KEYWORD("GROUPS")

  HANDLER  <- KEYWORD("HANDLER")
  HAVING   <- KEYWORD("HAVING")
  HEADER_P <- KEYWORD("HEADER_P")
  HOLD     <- KEYWORD("HOLD")
  HOUR_P   <- KEYWORD("HOUR_P")

  IDENTITY_P <- KEYWORD("IDENTITY_P")
  IF_P       <- KEYWORD("IF_P")
  ILIKE      <- KEYWORD("ILIKE")
  IMMEDIATE  <- KEYWORD("IMMEDIATE")
  IMMUTABLE  <- KEYWORD("IMMUTABLE")
  IMPLICIT_P <- KEYWORD("IMPLICIT_P")
  IMPORT_P   <- KEYWORD("IMPORT_P")
  IN_P       <- KEYWORD("IN_P")
  INCLUDE    <- KEYWORD("INCLUDE")

  INCLUDING <- KEYWORD("INCLUDING")
  INCREMENT <- KEYWORD("INCREMENT")
  INDEX     <- KEYWORD("INDEX")
  INDEXES   <- KEYWORD("INDEXES")
  INHERIT   <- KEYWORD("INHERIT")
  INHERITS  <- KEYWORD("INHERITS")
  INITIALLY <- KEYWORD("INITIALLY")
  INLINE_P  <- KEYWORD("INLINE_P")

  INNER_P     <- KEYWORD("INNER_P")
  INOUT       <- KEYWORD("INOUT")
  INPUT_P     <- KEYWORD("INPUT_P")
  INSENSITIVE <- KEYWORD("INSENSITIVE")
  INSERT      <- KEYWORD("INSERT")
  INSTEAD     <- KEYWORD("INSTEAD")
  INT_P       <- KEYWORD("INT_P")
  INTEGER     <- KEYWORD("INTEGER")

  INTERSECT <- KEYWORD("INTERSECT")
  INTERVAL  <- KEYWORD("INTERVAL")
  INTO      <- KEYWORD("INTO")
  INVOKER   <- KEYWORD("INVOKER")
  IS        <- KEYWORD("IS")
  ISNULL    <- KEYWORD("ISNULL")
  ISOLATION <- KEYWORD("ISOLATION")

  JOIN <- KEYWORD("JOIN")

  KEY <- KEYWORD("KEY")

  LABEL     <- KEYWORD("LABEL")
  LANGUAGE  <- KEYWORD("LANGUAGE")
  LARGE_P   <- KEYWORD("LARGE_P")
  LAST_P    <- KEYWORD("LAST_P")
  LATERAL_P <- KEYWORD("LATERAL_P")

  LEADING   <- KEYWORD("LEADING")
  LEAKPROOF <- KEYWORD("LEAKPROOF")
  LEAST     <- KEYWORD("LEAST")
  LEFT      <- KEYWORD("LEFT")
  LEVEL     <- KEYWORD("LEVEL")
  LIKE      <- KEYWORD("LIKE")
  LIMIT     <- KEYWORD("LIMIT")
  LISTEN    <- KEYWORD("LISTEN")
  LOAD      <- KEYWORD("LOAD")
  LOCAL     <- KEYWORD("LOCAL")

  LOCALTIME      <- KEYWORD("LOCALTIME")
  LOCALTIMESTAMP <- KEYWORD("LOCALTIMESTAMP")
  LOCATION       <- KEYWORD("LOCATION")
  LOCK_P         <- KEYWORD("LOCK_P")
  LOCKED         <- KEYWORD("LOCKED")
  LOGGED         <- KEYWORD("LOGGED")

  MAPPING      <- KEYWORD("MAPPING")
  MATCH        <- KEYWORD("MATCH")
  MATERIALIZED <- KEYWORD("MATERIALIZED")
  MAXVALUE     <- KEYWORD("MAXVALUE")
  METHOD       <- KEYWORD("METHOD")
  MINUTE_P     <- KEYWORD("MINUTE_P")
  MINVALUE     <- KEYWORD("MINVALUE")
  MODE         <- KEYWORD("MODE")
  MONTH_P      <- KEYWORD("MONTH_P")
  MOVE         <- KEYWORD("MOVE")

  NAME_P   <- KEYWORD("NAME_P")
  NAMES    <- KEYWORD("NAMES")
  NATIONAL <- KEYWORD("NATIONAL")
  NATURAL  <- KEYWORD("NATURAL")
  NCHAR    <- KEYWORD("NCHAR")
  NEW      <- KEYWORD("NEW")
  NEXT     <- KEYWORD("NEXT")
  NFC      <- KEYWORD("NFC")
  NFD      <- KEYWORD("NFD")
  NFKC     <- KEYWORD("NFKC")
  NFKD     <- KEYWORD("NFKD")
  NO       <- KEYWORD("NO")
  NONE     <- KEYWORD("NONE")

  NORMALIZE  <- KEYWORD("NORMALIZE")
  NORMALIZED <- KEYWORD("NORMALIZED")

  NOT     <- KEYWORD("NOT")
  NOTHING <- KEYWORD("NOTHING")
  NOTIFY  <- KEYWORD("NOTIFY")
  NOTNULL <- KEYWORD("NOTNULL")
  NOWAIT  <- KEYWORD("NOWAIT")
  NULL_P  <- KEYWORD("NULL_P")
  NULLIF  <- KEYWORD("NULLIF")

  NULLS_P <- KEYWORD("NULLS_P")
  NUMERIC <- KEYWORD("NUMERIC")

  OBJECT_P <- KEYWORD("OBJECT_P")
  OF       <- KEYWORD("OF")
  OFF      <- KEYWORD("OFF")
  OFFSET   <- KEYWORD("OFFSET")
  OIDS     <- KEYWORD("OIDS")
  OLD      <- KEYWORD("OLD")
  ON       <- KEYWORD("ON")
  ONLY     <- KEYWORD("ONLY")
  OPERATOR <- KEYWORD("OPERATOR")
  OPTION   <- KEYWORD("OPTION")
  OPTIONS  <- KEYWORD("OPTIONS")
  OR       <- KEYWORD("OR")

  ORDER      <- KEYWORD("ORDER")
  ORDINALITY <- KEYWORD("ORDINALITY")
  OTHERS     <- KEYWORD("OTHERS")
  OUT_P      <- KEYWORD("OUT_P")
  OUTER_P    <- KEYWORD("OUTER_P")

  OVER       <- KEYWORD("OVER")
  OVERLAPS   <- KEYWORD("OVERLAPS")
  OVERLAY    <- KEYWORD("OVERLAY")
  OVERRIDING <- KEYWORD("OVERRIDING")
  OWNED      <- KEYWORD("OWNED")
  OWNER      <- KEYWORD("OWNER")

  PARALLEL  <- KEYWORD("PARALLEL")
  PARSER    <- KEYWORD("PARSER")
  PARTIAL   <- KEYWORD("PARTIAL")
  PARTITION <- KEYWORD("PARTITION")
  PASSING   <- KEYWORD("PASSING")
  PASSWORD  <- KEYWORD("PASSWORD")
  PLACING   <- KEYWORD("PLACING")
  PLANS     <- KEYWORD("PLANS")
  POLICY    <- KEYWORD("POLICY")

  POSITION  <- KEYWORD("POSITION")
  PRECEDING <- KEYWORD("PRECEDING")
  PRECISION <- KEYWORD("PRECISION")
  PRESERVE  <- KEYWORD("PRESERVE")
  PREPARE   <- KEYWORD("PREPARE")
  PREPARED  <- KEYWORD("PREPARED")
  PRIMARY   <- KEYWORD("PRIMARY")

  PRIOR       <- KEYWORD("PRIOR")
  PRIVILEGES  <- KEYWORD("PRIVILEGES")
  PROCEDURAL  <- KEYWORD("PROCEDURAL")
  PROCEDURE   <- KEYWORD("PROCEDURE")
  PROCEDURES  <- KEYWORD("PROCEDURES")
  PROGRAM     <- KEYWORD("PROGRAM")
  PUBLICATION <- KEYWORD("PUBLICATION")

  QUOTE <- KEYWORD("QUOTE")

  RANGE       <- KEYWORD("RANGE")
  READ        <- KEYWORD("READ")
  REAL        <- KEYWORD("REAL")
  REASSIGN    <- KEYWORD("REASSIGN")
  RECHECK     <- KEYWORD("RECHECK")
  RECURSIVE   <- KEYWORD("RECURSIVE")
  REF         <- KEYWORD("REF")
  REFERENCES  <- KEYWORD("REFERENCES")
  REFERENCING <- KEYWORD("REFERENCING")

  REFRESH    <- KEYWORD("REFRESH")
  REINDEX    <- KEYWORD("REINDEX")
  RELATIVE_P <- KEYWORD("RELATIVE_P")
  RELEASE    <- KEYWORD("RELEASE")
  RENAME     <- KEYWORD("RENAME")
  REPEATABLE <- KEYWORD("REPEATABLE")
  REPLACE    <- KEYWORD("REPLACE")
  REPLICA    <- KEYWORD("REPLICA")

  RESET     <- KEYWORD("RESET")
  RESTART   <- KEYWORD("RESTART")
  RESTRICT  <- KEYWORD("RESTRICT")
  RETURN    <- KEYWORD("RETURN")
  RETURNING <- KEYWORD("RETURNING")
  RETURNS   <- KEYWORD("RETURNS")
  REVOKE    <- KEYWORD("REVOKE")
  RIGHT     <- KEYWORD("RIGHT")
  ROLE      <- KEYWORD("ROLE")
  ROLLBACK  <- KEYWORD("ROLLBACK")
  ROLLUP    <- KEYWORD("ROLLUP")

  ROUTINE  <- KEYWORD("ROUTINE")
  ROUTINES <- KEYWORD("ROUTINES")
  ROW      <- KEYWORD("ROW")
  ROWS     <- KEYWORD("ROWS")
  RULE     <- KEYWORD("RULE")

  SAVEPOINT <- KEYWORD("SAVEPOINT")
  SCHEMA    <- KEYWORD("SCHEMA")
  SCHEMAS   <- KEYWORD("SCHEMAS")
  SCROLL    <- KEYWORD("SCROLL")
  SEARCH    <- KEYWORD("SEARCH")
  SECOND_P  <- KEYWORD("SECOND_P")
  SECURITY  <- KEYWORD("SECURITY")
  SELECT    <- KEYWORD("SELECT")
  SEQUENCE  <- KEYWORD("SEQUENCE")
  SEQUENCES <- KEYWORD("SEQUENCES")

  SERIALIZABLE <- KEYWORD("SERIALIZABLE")
  SERVER       <- KEYWORD("SERVER")
  SESSION      <- KEYWORD("SESSION")
  SESSION_USER <- KEYWORD("SESSION_USER")
  SET          <- KEYWORD("SET")
  SETS         <- KEYWORD("SETS")
  SETOF        <- KEYWORD("SETOF")
  SHARE        <- KEYWORD("SHARE")
  SHOW         <- KEYWORD("SHOW")

  SIMILAR      <- KEYWORD("SIMILAR")
  SIMPLE       <- KEYWORD("SIMPLE")
  SKIP         <- KEYWORD("SKIP")
  SMALLINT     <- KEYWORD("SMALLINT")
  SNAPSHOT     <- KEYWORD("SNAPSHOT")
  SOME         <- KEYWORD("SOME")
  SQL_P        <- KEYWORD("SQL_P")
  STABLE       <- KEYWORD("STABLE")
  STANDALONE_P <- KEYWORD("STANDALONE_P")

  START      <- KEYWORD("START")
  STATEMENT  <- KEYWORD("STATEMENT")
  STATISTICS <- KEYWORD("STATISTICS")
  STDIN      <- KEYWORD("STDIN")
  STDOUT     <- KEYWORD("STDOUT")
  STORAGE    <- KEYWORD("STORAGE")
  STORED     <- KEYWORD("STORED")
  STRICT_P   <- KEYWORD("STRICT_P")
  STRIP_P    <- KEYWORD("STRIP_P")

  SUBSCRIPTION <- KEYWORD("SUBSCRIPTION")
  SUBSTRING    <- KEYWORD("SUBSTRING")
  SUPPORT      <- KEYWORD("SUPPORT")
  SYMMETRIC    <- KEYWORD("SYMMETRIC")
  SYSID        <- KEYWORD("SYSID")
  SYSTEM_P     <- KEYWORD("SYSTEM_P")

  TABLE       <- KEYWORD("TABLE")
  TABLES      <- KEYWORD("TABLES")
  TABLESAMPLE <- KEYWORD("TABLESAMPLE")
  TABLESPACE  <- KEYWORD("TABLESPACE")
  TEMP        <- KEYWORD("TEMP")
  TEMPLATE    <- KEYWORD("TEMPLATE")
  TEMPORARY   <- KEYWORD("TEMPORARY")
  TEXT_P      <- KEYWORD("TEXT_P")
  THEN        <- KEYWORD("THEN")

  TIES        <- KEYWORD("TIES")
  TIME        <- KEYWORD("TIME")
  TIMESTAMP   <- KEYWORD("TIMESTAMP")
  TO          <- KEYWORD("TO")
  TRAILING    <- KEYWORD("TRAILING")
  TRANSACTION <- KEYWORD("TRANSACTION")
  TRANSFORM   <- KEYWORD("TRANSFORM")

  TREAT   <- KEYWORD("TREAT")
  TRIGGER <- KEYWORD("TRIGGER")
  TRIM    <- KEYWORD("TRIM")
  TRUE_P  <- KEYWORD("TRUE_P")

  TRUNCATE <- KEYWORD("TRUNCATE")
  TRUSTED  <- KEYWORD("TRUSTED")
  TYPE_P   <- KEYWORD("TYPE_P")
  TYPES_P  <- KEYWORD("TYPES_P")

  UESCAPE     <- KEYWORD("UESCAPE")
  UNBOUNDED   <- KEYWORD("UNBOUNDED")
  UNCOMMITTED <- KEYWORD("UNCOMMITTED")
  UNENCRYPTED <- KEYWORD("UNENCRYPTED")
  UNION       <- KEYWORD("UNION")
  UNIQUE      <- KEYWORD("UNIQUE")
  UNKNOWN     <- KEYWORD("UNKNOWN")

  UNLISTEN <- KEYWORD("UNLISTEN")
  UNLOGGED <- KEYWORD("UNLOGGED")
  UNTIL    <- KEYWORD("UNTIL")
  UPDATE   <- KEYWORD("UPDATE")
  USER     <- KEYWORD("USER")
  USING    <- KEYWORD("USING")

  VACUUM    <- KEYWORD("VACUUM")
  VALID     <- KEYWORD("VALID")
  VALIDATE  <- KEYWORD("VALIDATE")
  VALIDATOR <- KEYWORD("VALIDATOR")
  VALUE_P   <- KEYWORD("VALUE_P")
  VALUES    <- KEYWORD("VALUES")
  VARCHAR   <- KEYWORD("VARCHAR")
  VARIADIC  <- KEYWORD("VARIADIC")
  VARYING   <- KEYWORD("VARYING")

  VERBOSE   <- KEYWORD("VERBOSE")
  VERSION_P <- KEYWORD("VERSION_P")
  VIEW      <- KEYWORD("VIEW")
  VIEWS     <- KEYWORD("VIEWS")
  VOLATILE  <- KEYWORD("VOLATILE")

  WHEN         <- KEYWORD("WHEN")
  WHERE        <- KEYWORD("WHERE")
  WHITESPACE_P <- KEYWORD("WHITESPACE_P")
  WINDOW       <- KEYWORD("WINDOW")
  WITH         <- KEYWORD("WITH")
  WITHIN       <- KEYWORD("WITHIN")
  WITHOUT      <- KEYWORD("WITHOUT")
  WORK         <- KEYWORD("WORK")
  WRAPPER      <- KEYWORD("WRAPPER")
  WRITE        <- KEYWORD("WRITE")

  XML_P         <- KEYWORD("XML_P")
  XMLATTRIBUTES <- KEYWORD("XMLATTRIBUTES")
  XMLCONCAT     <- KEYWORD("XMLCONCAT")
  XMLELEMENT    <- KEYWORD("XMLELEMENT")
  XMLEXISTS     <- KEYWORD("XMLEXISTS")
  XMLFOREST     <- KEYWORD("XMLFOREST")
  XMLNAMESPACES <- KEYWORD("XMLNAMESPACES")

  XMLPARSE     <- KEYWORD("XMLPARSE")
  XMLPI        <- KEYWORD("XMLPI")
  XMLROOT      <- KEYWORD("XMLROOT")
  XMLSERIALIZE <- KEYWORD("XMLSERIALIZE")
  XMLTABLE     <- KEYWORD("XMLTABLE")

  YEAR_P <- KEYWORD("YEAR_P")
  YES_P  <- KEYWORD("YES_P")

  ZONE <- KEYWORD("ZONE")

  #
  # Keyword category lists.  Generally, every keyword present in
  # the Postgres grammar should appear in exactly one of these lists.
  #
  # Put a new keyword into the first list that it can go into without causing
  # shift or reduce conflicts.  The earlier lists define "less reserved"
  # categories of keywords.
  #
  # Make sure that each keyword's category in kwlist.h matches where
  # it is listed here.  (Someday we may be able to generate these lists and
  # kwlist.h's table from one source of truth.)
  #

  # "Unreserved" keywords --- available for use as any kind of name.
  #
  unreserved_keyword <- (
    ABORT_P         |
    ABSOLUTE_P      |
    ACCESS          |
    ACTION          |
    ADD_P           |
    ADMIN           |
    AFTER           |
    AGGREGATE       |
    ALSO            |
    ALTER           |
    ALWAYS          |
    ASENSITIVE      |
    ASSERTION       |
    ASSIGNMENT      |
    AT              |
    ATOMIC          |
    ATTACH          |
    ATTRIBUTE       |
    BACKWARD        |
    BEFORE          |
    BEGIN_P         |
    BREADTH         |
    BY              |
    CACHE           |
    CALL            |
    CALLED          |
    CASCADE         |
    CASCADED        |
    CATALOG_P       |
    CHAIN           |
    CHARACTERISTICS |
    CHECKPOINT      |
    CLASS           |
    CLOSE           |
    CLUSTER         |
    COLUMNS         |
    COMMENT         |
    COMMENTS        |
    COMMIT          |
    COMMITTED       |
    COMPRESSION     |
    CONFIGURATION   |
    CONFLICT        |
    CONNECTION      |
    CONSTRAINTS     |
    CONTENT_P       |
    CONTINUE_P      |
    CONVERSION_P    |
    COPY            |
    COST            |
    CSV             |
    CUBE            |
    CURRENT_P       |
    CURSOR          |
    CYCLE           |
    DATA_P          |
    DATABASE        |
    DAY_P           |
    DEALLOCATE      |
    DECLARE         |
    DEFAULTS        |
    DEFERRED        |
    DEFINER         |
    DELETE_P        |
    DELIMITER       |
    DELIMITERS      |
    DEPENDS         |
    DEPTH           |
    DETACH          |
    DICTIONARY      |
    DISABLE_P       |
    DISCARD         |
    DOCUMENT_P      |
    DOMAIN_P        |
    DOUBLE_P        |
    DROP            |
    EACH            |
    ENABLE_P        |
    ENCODING        |
    ENCRYPTED       |
    ENUM_P          |
    ESCAPE          |
    EVENT           |
    EXCLUDE         |
    EXCLUDING       |
    EXCLUSIVE       |
    EXECUTE         |
    EXPLAIN         |
    EXPRESSION      |
    EXTENSION       |
    EXTERNAL        |
    FAMILY          |
    FILTER          |
    FINALIZE        |
    FIRST_P         |
    FOLLOWING       |
    FORCE           |
    FORWARD         |
    FUNCTION        |
    FUNCTIONS       |
    GENERATED       |
    GLOBAL          |
    GRANTED         |
    GROUPS          |
    HANDLER         |
    HEADER_P        |
    HOLD            |
    HOUR_P          |
    IDENTITY_P      |
    IF_P            |
    IMMEDIATE       |
    IMMUTABLE       |
    IMPLICIT_P      |
    IMPORT_P        |
    INCLUDE         |
    INCLUDING       |
    INCREMENT       |
    INDEX           |
    INDEXES         |
    INHERIT         |
    INHERITS        |
    INLINE_P        |
    INPUT_P         |
    INSENSITIVE     |
    INSERT          |
    INSTEAD         |
    INVOKER         |
    ISOLATION       |
    KEY             |
    LABEL           |
    LANGUAGE        |
    LARGE_P         |
    LAST_P          |
    LEAKPROOF       |
    LEVEL           |
    LISTEN          |
    LOAD            |
    LOCAL           |
    LOCATION        |
    LOCK_P          |
    LOCKED          |
    LOGGED          |
    MAPPING         |
    MATCH           |
    MATERIALIZED    |
    MAXVALUE        |
    METHOD          |
    MINUTE_P        |
    MINVALUE        |
    MODE            |
    MONTH_P         |
    MOVE            |
    NAME_P          |
    NAMES           |
    NEW             |
    NEXT            |
    NFC             |
    NFD             |
    NFKC            |
    NFKD            |
    NO              |
    NORMALIZED      |
    NOTHING         |
    NOTIFY          |
    NOWAIT          |
    NULLS_P         |
    OBJECT_P        |
    OF              |
    OFF             |
    OIDS            |
    OLD             |
    OPERATOR        |
    OPTION          |
    OPTIONS         |
    ORDINALITY      |
    OTHERS          |
    OVER            |
    OVERRIDING      |
    OWNED           |
    OWNER           |
    PARALLEL        |
    PARSER          |
    PARTIAL         |
    PARTITION       |
    PASSING         |
    PASSWORD        |
    PLANS           |
    POLICY          |
    PRECEDING       |
    PREPARE         |
    PREPARED        |
    PRESERVE        |
    PRIOR           |
    PRIVILEGES      |
    PROCEDURAL      |
    PROCEDURE       |
    PROCEDURES      |
    PROGRAM         |
    PUBLICATION     |
    QUOTE           |
    RANGE           |
    READ            |
    REASSIGN        |
    RECHECK         |
    RECURSIVE       |
    REF             |
    REFERENCING     |
    REFRESH         |
    REINDEX         |
    RELATIVE_P      |
    RELEASE         |
    RENAME          |
    REPEATABLE      |
    REPLACE         |
    REPLICA         |
    RESET           |
    RESTART         |
    RESTRICT        |
    RETURN          |
    RETURNS         |
    REVOKE          |
    ROLE            |
    ROLLBACK        |
    ROLLUP          |
    ROUTINE         |
    ROUTINES        |
    ROWS            |
    RULE            |
    SAVEPOINT       |
    SCHEMA          |
    SCHEMAS         |
    SCROLL          |
    SEARCH          |
    SECOND_P        |
    SECURITY        |
    SEQUENCE        |
    SEQUENCES       |
    SERIALIZABLE    |
    SERVER          |
    SESSION         |
    SET             |
    SETS            |
    SHARE           |
    SHOW            |
    SIMPLE          |
    SKIP            |
    SNAPSHOT        |
    SQL_P           |
    STABLE          |
    STANDALONE_P    |
    START           |
    STATEMENT       |
    STATISTICS      |
    STDIN           |
    STDOUT          |
    STORAGE         |
    STORED          |
    STRICT_P        |
    STRIP_P         |
    SUBSCRIPTION    |
    SUPPORT         |
    SYSID           |
    SYSTEM_P        |
    TABLES          |
    TABLESPACE      |
    TEMP            |
    TEMPLATE        |
    TEMPORARY       |
    TEXT_P          |
    TIES            |
    TRANSACTION     |
    TRANSFORM       |
    TRIGGER         |
    TRUNCATE        |
    TRUSTED         |
    TYPE_P          |
    TYPES_P         |
    UESCAPE         |
    UNBOUNDED       |
    UNCOMMITTED     |
    UNENCRYPTED     |
    UNKNOWN         |
    UNLISTEN        |
    UNLOGGED        |
    UNTIL           |
    UPDATE          |
    VACUUM          |
    VALID           |
    VALIDATE        |
    VALIDATOR       |
    VALUE_P         |
    VARYING         |
    VERSION_P       |
    VIEW            |
    VIEWS           |
    VOLATILE        |
    WHITESPACE_P    |
    WITHIN          |
    WITHOUT         |
    WORK            |
    WRAPPER         |
    WRITE           |
    XML_P           |
    YEAR_P          |
    YES_P           |
    ZONE
  )

  # Column identifier --- keywords that can be column, table, etc names.
  #
  # Many of these keywords will in fact be recognized as type or function
  # names too; but they have special productions for the purpose, and so
  # can't be treated as "generic" type or function names.
  #
  # The type names appearing here are not usable as function names
  # because they can be followed by '(' in typename productions, which
  # looks too much like a function call for an LR(1) parser.
  #
  col_name_keyword <- (
    BETWEEN       |
    BIGINT        |
    BIT           |
    BOOLEAN_P     |
    CHAR_P        |
    CHARACTER     |
    COALESCE      |
    DEC           |
    DECIMAL_P     |
    EXISTS        |
    EXTRACT       |
    FLOAT_P       |
    GREATEST      |
    GROUPING      |
    INOUT         |
    INT_P         |
    INTEGER       |
    INTERVAL      |
    LEAST         |
    NATIONAL      |
    NCHAR         |
    NONE          |
    NORMALIZE     |
    NULLIF        |
    NUMERIC       |
    OUT_P         |
    OVERLAY       |
    POSITION      |
    PRECISION     |
    REAL          |
    ROW           |
    SETOF         |
    SMALLINT      |
    SUBSTRING     |
    TIME          |
    TIMESTAMP     |
    TREAT         |
    TRIM          |
    VALUES        |
    VARCHAR       |
    XMLATTRIBUTES |
    XMLCONCAT     |
    XMLELEMENT    |
    XMLEXISTS     |
    XMLFOREST     |
    XMLNAMESPACES |
    XMLPARSE      |
    XMLPI         |
    XMLROOT       |
    XMLSERIALIZE  |
    XMLTABLE
  )

  # Type/function identifier --- keywords that can be type or function names.
  #
  # Most of these are keywords that are used as operators in expressions;
  # in general such keywords can't be column names because they would be
  # ambiguous with variables, but they are unambiguous as function identifiers.
  #
  # Do not include POSITION, SUBSTRING, etc here since they have explicit
  # productions in a_expr to support the goofy SQL9x argument syntax.
  # - thomas 2000-11-28
  #
  type_func_name_keyword <- (
    AUTHORIZATION   |
    BINARY          |
    COLLATION       |
    CONCURRENTLY    |
    CROSS           |
    CURRENT_SCHEMA  |
    FREEZE          |
    FULL            |
    ILIKE           |
    INNER_P         |
    IS              |
    ISNULL          |
    JOIN            |
    LEFT            |
    LIKE            |
    NATURAL         |
    NOTNULL         |
    OUTER_P         |
    OVERLAPS        |
    RIGHT           |
    SIMILAR         |
    TABLESAMPLE     |
    VERBOSE
  )

  # Reserved keyword --- these keywords are usable only as a ColLabel.
  #
  # Keywords appear here if they could not be distinguished from variable,
  # type, or function names in some contexts.  Don't put things here unless
  # forced to.
  #
  reserved_keyword <- (
    ALL               |
    ANALYSE           |
    ANALYZE           |
    AND               |
    ANY               |
    ARRAY             |
    AS                |
    ASC               |
    ASYMMETRIC        |
    BOTH              |
    CASE              |
    CAST              |
    CHECK             |
    COLLATE           |
    COLUMN            |
    CONSTRAINT        |
    CREATE            |
    CURRENT_CATALOG   |
    CURRENT_DATE      |
    CURRENT_ROLE      |
    CURRENT_TIME      |
    CURRENT_TIMESTAMP |
    CURRENT_USER      |
    DEFAULT           |
    DEFERRABLE        |
    DESC              |
    DISTINCT          |
    DO                |
    ELSE              |
    END_P             |
    EXCEPT            |
    FALSE_P           |
    FETCH             |
    FOR               |
    FOREIGN           |
    FROM              |
    GRANT             |
    GROUP_P           |
    HAVING            |
    IN_P              |
    INITIALLY         |
    INTERSECT         |
    INTO              |
    LATERAL_P         |
    LEADING           |
    LIMIT             |
    LOCALTIME         |
    LOCALTIMESTAMP    |
    NOT               |
    NULL_P            |
    OFFSET            |
    ON                |
    ONLY              |
    OR                |
    ORDER             |
    PLACING           |
    PRIMARY           |
    REFERENCES        |
    RETURNING         |
    SELECT            |
    SESSION_USER      |
    SOME              |
    SYMMETRIC         |
    TABLE             |
    THEN              |
    TO                |
    TRAILING          |
    TRUE_P            |
    UNION             |
    UNIQUE            |
    USER              |
    USING             |
    VARIADIC          |
    WHEN              |
    WHERE             |
    WINDOW            |
    WITH
  )

  #
  # While all keywords can be used as column labels when preceded by AS,
  # not all of them can be used as a "bare" column label without AS.
  # Those that can be used as a bare label must be listed here,
  # in addition to appearing in one of the category lists above.
  #
  # Always add a new keyword to this list if possible.  Mark it BARE_LABEL
  # in kwlist.h if it is included here, or AS_LABEL if it is not.
  #
  bare_label_keyword <- (
    ABORT_P           |
    ABSOLUTE_P        |
    ACCESS            |
    ACTION            |
    ADD_P             |
    ADMIN             |
    AFTER             |
    AGGREGATE         |
    ALL               |
    ALSO              |
    ALTER             |
    ALWAYS            |
    ANALYSE           |
    ANALYZE           |
    AND               |
    ANY               |
    ASC               |
    ASENSITIVE        |
    ASSERTION         |
    ASSIGNMENT        |
    ASYMMETRIC        |
    AT                |
    ATOMIC            |
    ATTACH            |
    ATTRIBUTE         |
    AUTHORIZATION     |
    BACKWARD          |
    BEFORE            |
    BEGIN_P           |
    BETWEEN           |
    BIGINT            |
    BINARY            |
    BIT               |
    BOOLEAN_P         |
    BOTH              |
    BREADTH           |
    BY                |
    CACHE             |
    CALL              |
    CALLED            |
    CASCADE           |
    CASCADED          |
    CASE              |
    CAST              |
    CATALOG_P         |
    CHAIN             |
    CHARACTERISTICS   |
    CHECK             |
    CHECKPOINT        |
    CLASS             |
    CLOSE             |
    CLUSTER           |
    COALESCE          |
    COLLATE           |
    COLLATION         |
    COLUMN            |
    COLUMNS           |
    COMMENT           |
    COMMENTS          |
    COMMIT            |
    COMMITTED         |
    COMPRESSION       |
    CONCURRENTLY      |
    CONFIGURATION     |
    CONFLICT          |
    CONNECTION        |
    CONSTRAINT        |
    CONSTRAINTS       |
    CONTENT_P         |
    CONTINUE_P        |
    CONVERSION_P      |
    COPY              |
    COST              |
    CROSS             |
    CSV               |
    CUBE              |
    CURRENT_P         |
    CURRENT_CATALOG   |
    CURRENT_DATE      |
    CURRENT_ROLE      |
    CURRENT_SCHEMA    |
    CURRENT_TIME      |
    CURRENT_TIMESTAMP |
    CURRENT_USER      |
    CURSOR            |
    CYCLE             |
    DATA_P            |
    DATABASE          |
    DEALLOCATE        |
    DEC               |
    DECIMAL_P         |
    DECLARE           |
    DEFAULT           |
    DEFAULTS          |
    DEFERRABLE        |
    DEFERRED          |
    DEFINER           |
    DELETE_P          |
    DELIMITER         |
    DELIMITERS        |
    DEPENDS           |
    DEPTH             |
    DESC              |
    DETACH            |
    DICTIONARY        |
    DISABLE_P         |
    DISCARD           |
    DISTINCT          |
    DO                |
    DOCUMENT_P        |
    DOMAIN_P          |
    DOUBLE_P          |
    DROP              |
    EACH              |
    ELSE              |
    ENABLE_P          |
    ENCODING          |
    ENCRYPTED         |
    END_P             |
    ENUM_P            |
    ESCAPE            |
    EVENT             |
    EXCLUDE           |
    EXCLUDING         |
    EXCLUSIVE         |
    EXECUTE           |
    EXISTS            |
    EXPLAIN           |
    EXPRESSION        |
    EXTENSION         |
    EXTERNAL          |
    EXTRACT           |
    FALSE_P           |
    FAMILY            |
    FINALIZE          |
    FIRST_P           |
    FLOAT_P           |
    FOLLOWING         |
    FORCE             |
    FOREIGN           |
    FORWARD           |
    FREEZE            |
    FULL              |
    FUNCTION          |
    FUNCTIONS         |
    GENERATED         |
    GLOBAL            |
    GRANTED           |
    GREATEST          |
    GROUPING          |
    GROUPS            |
    HANDLER           |
    HEADER_P          |
    HOLD              |
    IDENTITY_P        |
    IF_P              |
    ILIKE             |
    IMMEDIATE         |
    IMMUTABLE         |
    IMPLICIT_P        |
    IMPORT_P          |
    IN_P              |
    INCLUDE           |
    INCLUDING         |
    INCREMENT         |
    INDEX             |
    INDEXES           |
    INHERIT           |
    INHERITS          |
    INITIALLY         |
    INLINE_P          |
    INNER_P           |
    INOUT             |
    INPUT_P           |
    INSENSITIVE       |
    INSERT            |
    INSTEAD           |
    INT_P             |
    INTEGER           |
    INTERVAL          |
    INVOKER           |
    IS                |
    ISOLATION         |
    JOIN              |
    KEY               |
    LABEL             |
    LANGUAGE          |
    LARGE_P           |
    LAST_P            |
    LATERAL_P         |
    LEADING           |
    LEAKPROOF         |
    LEAST             |
    LEFT              |
    LEVEL             |
    LIKE              |
    LISTEN            |
    LOAD              |
    LOCAL             |
    LOCALTIME         |
    LOCALTIMESTAMP    |
    LOCATION          |
    LOCK_P            |
    LOCKED            |
    LOGGED            |
    MAPPING           |
    MATCH             |
    MATERIALIZED      |
    MAXVALUE          |
    METHOD            |
    MINVALUE          |
    MODE              |
    MOVE              |
    NAME_P            |
    NAMES             |
    NATIONAL          |
    NATURAL           |
    NCHAR             |
    NEW               |
    NEXT              |
    NFC               |
    NFD               |
    NFKC              |
    NFKD              |
    NO                |
    NONE              |
    NORMALIZE         |
    NORMALIZED        |
    NOT               |
    NOTHING           |
    NOTIFY            |
    NOWAIT            |
    NULL_P            |
    NULLIF            |
    NULLS_P           |
    NUMERIC           |
    OBJECT_P          |
    OF                |
    OFF               |
    OIDS              |
    OLD               |
    ONLY              |
    OPERATOR          |
    OPTION            |
    OPTIONS           |
    OR                |
    ORDINALITY        |
    OTHERS            |
    OUT_P             |
    OUTER_P           |
    OVERLAY           |
    OVERRIDING        |
    OWNED             |
    OWNER             |
    PARALLEL          |
    PARSER            |
    PARTIAL           |
    PARTITION         |
    PASSING           |
    PASSWORD          |
    PLACING           |
    PLANS             |
    POLICY            |
    POSITION          |
    PRECEDING         |
    PREPARE           |
    PREPARED          |
    PRESERVE          |
    PRIMARY           |
    PRIOR             |
    PRIVILEGES        |
    PROCEDURAL        |
    PROCEDURE         |
    PROCEDURES        |
    PROGRAM           |
    PUBLICATION       |
    QUOTE             |
    RANGE             |
    READ              |
    REAL              |
    REASSIGN          |
    RECHECK           |
    RECURSIVE         |
    REF               |
    REFERENCES        |
    REFERENCING       |
    REFRESH           |
    REINDEX           |
    RELATIVE_P        |
    RELEASE           |
    RENAME            |
    REPEATABLE        |
    REPLACE           |
    REPLICA           |
    RESET             |
    RESTART           |
    RESTRICT          |
    RETURN            |
    RETURNS           |
    REVOKE            |
    RIGHT             |
    ROLE              |
    ROLLBACK          |
    ROLLUP            |
    ROUTINE           |
    ROUTINES          |
    ROW               |
    ROWS              |
    RULE              |
    SAVEPOINT         |
    SCHEMA            |
    SCHEMAS           |
    SCROLL            |
    SEARCH            |
    SECURITY          |
    SELECT            |
    SEQUENCE          |
    SEQUENCES         |
    SERIALIZABLE      |
    SERVER            |
    SESSION           |
    SESSION_USER      |
    SET               |
    SETOF             |
    SETS              |
    SHARE             |
    SHOW              |
    SIMILAR           |
    SIMPLE            |
    SKIP              |
    SMALLINT          |
    SNAPSHOT          |
    SOME              |
    SQL_P             |
    STABLE            |
    STANDALONE_P      |
    START             |
    STATEMENT         |
    STATISTICS        |
    STDIN             |
    STDOUT            |
    STORAGE           |
    STORED            |
    STRICT_P          |
    STRIP_P           |
    SUBSCRIPTION      |
    SUBSTRING         |
    SUPPORT           |
    SYMMETRIC         |
    SYSID             |
    SYSTEM_P          |
    TABLE             |
    TABLES            |
    TABLESAMPLE       |
    TABLESPACE        |
    TEMP              |
    TEMPLATE          |
    TEMPORARY         |
    TEXT_P            |
    THEN              |
    TIES              |
    TIME              |
    TIMESTAMP         |
    TRAILING          |
    TRANSACTION       |
    TRANSFORM         |
    TREAT             |
    TRIGGER           |
    TRIM              |
    TRUE_P            |
    TRUNCATE          |
    TRUSTED           |
    TYPE_P            |
    TYPES_P           |
    UESCAPE           |
    UNBOUNDED         |
    UNCOMMITTED       |
    UNENCRYPTED       |
    UNIQUE            |
    UNKNOWN           |
    UNLISTEN          |
    UNLOGGED          |
    UNTIL             |
    UPDATE            |
    USER              |
    USING             |
    VACUUM            |
    VALID             |
    VALIDATE          |
    VALIDATOR         |
    VALUE_P           |
    VALUES            |
    VARCHAR           |
    VARIADIC          |
    VERBOSE           |
    VERSION_P         |
    VIEW              |
    VIEWS             |
    VOLATILE          |
    WHEN              |
    WHITESPACE_P      |
    WORK              |
    WRAPPER           |
    WRITE             |
    XML_P             |
    XMLATTRIBUTES     |
    XMLCONCAT         |
    XMLELEMENT        |
    XMLEXISTS         |
    XMLFOREST         |
    XMLNAMESPACES     |
    XMLPARSE          |
    XMLPI             |
    XMLROOT           |
    XMLSERIALIZE      |
    XMLTABLE          |
    YES_P             |
    ZONE
  )

