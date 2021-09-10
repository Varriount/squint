import npeg


grammar "sql":
  Optional(x) <- ?x
  # %type <node>  stmt toplevel_stmt schema_stmt routine_body_stmt
  #       AlterEventTrigStmt AlterCollationStmt
  #       AlterDatabaseStmt AlterDatabaseSetStmt AlterDomainStmt AlterEnumStmt
  #       AlterFdwStmt AlterForeignServerStmt AlterGroupStmt
  #       AlterObjectDependsStmt AlterObjectSchemaStmt AlterOwnerStmt
  #       AlterOperatorStmt AlterTypeStmt AlterSeqStmt AlterSystemStmt AlterTableStmt
  #       AlterTblSpcStmt AlterExtensionStmt AlterExtensionContentsStmt
  #       AlterCompositeTypeStmt AlterUserMappingStmt
  #       AlterRoleStmt AlterRoleSetStmt AlterPolicyStmt AlterStatsStmt
  #       AlterDefaultPrivilegesStmt DefACLAction
  #       AnalyzeStmt CallStmt ClosePortalStmt ClusterStmt CommentStmt
  #       ConstraintsSetStmt CopyStmt CreateAsStmt CreateCastStmt
  #       CreateDomainStmt CreateExtensionStmt CreateGroupStmt CreateOpClassStmt
  #       CreateOpFamilyStmt AlterOpFamilyStmt CreatePLangStmt
  #       CreateSchemaStmt CreateSeqStmt CreateStmt CreateStatsStmt CreateTableSpaceStmt
  #       CreateFdwStmt CreateForeignServerStmt CreateForeignTableStmt
  #       CreateAssertionStmt CreateTransformStmt CreateTrigStmt CreateEventTrigStmt
  #       CreateUserStmt CreateUserMappingStmt CreateRoleStmt CreatePolicyStmt
  #       CreatedbStmt DeclareCursorStmt DefineStmt DeleteStmt DiscardStmt DoStmt
  #       DropOpClassStmt DropOpFamilyStmt DropStmt
  #       DropCastStmt DropRoleStmt
  #       DropdbStmt DropTableSpaceStmt
  #       DropTransformStmt
  #       DropUserMappingStmt ExplainStmt FetchStmt
  #       GrantStmt GrantRoleStmt ImportForeignSchemaStmt IndexStmt InsertStmt
  #       ListenStmt LoadStmt LockStmt NotifyStmt ExplainableStmt PreparableStmt
  #       CreateFunctionStmt AlterFunctionStmt ReindexStmt RemoveAggrStmt
  #       RemoveFuncStmt RemoveOperStmt RenameStmt ReturnStmt RevokeStmt RevokeRoleStmt
  #       RuleActionStmt RuleActionStmtOrEmpty RuleStmt
  #       SecLabelStmt SelectStmt TransactionStmt TransactionStmtLegacy TruncateStmt
  #       UnlistenStmt UpdateStmt VacuumStmt
  #       VariableResetStmt VariableSetStmt VariableShowStmt
  #       ViewStmt CheckPointStmt CreateConversionStmt
  #       DeallocateStmt PrepareStmt ExecuteStmt
  #       DropOwnedStmt ReassignOwnedStmt
  #       AlterTSConfigurationStmt AlterTSDictionaryStmt
  #       CreateMatViewStmt RefreshMatViewStmt CreateAmStmt
  #       CreatePublicationStmt AlterPublicationStmt
  #       CreateSubscriptionStmt AlterSubscriptionStmt DropSubscriptionStmt

  # %type <node>  select_no_parens select_with_parens select_clause
  #               simple_select values_clause
  #               PLpgSQL_Expr PLAssignStmt

  # %type <node>  alter_column_default opclass_item opclass_drop alter_using
  # %type <ival>  add_drop opt_asc_desc opt_nulls_order

  # %type <node>  alter_table_cmd alter_type_cmd opt_collate_clause
  #      replica_identity partition_cmd index_partition_cmd
  # %type <list>  alter_table_cmds alter_type_cmds
  # %type <list>    alter_identity_column_option_list
  # %type <defelt>  alter_identity_column_option

  # %type <dbehavior> opt_drop_behavior

  # %type <list>  createdb_opt_list createdb_opt_items copy_opt_list
  #               transaction_mode_list
  #               create_extension_opt_list alter_extension_opt_list
  # %type <defelt>    createdb_opt_item copy_opt_item
  #               transaction_mode_item
  #               create_extension_opt_item alter_extension_opt_item

  # %type <ival>  opt_lock lock_type cast_context
  # %type <str>       utility_option_name
  # %type <defelt>    utility_option_elem
  # %type <list>  utility_option_list
  # %type <node>  utility_option_arg
  # %type <defelt>    drop_option
  # %type <boolean>   opt_or_replace opt_no
  #               opt_grant_grant_option opt_grant_admin_option
  #               opt_nowait opt_if_exists opt_with_data
  #               opt_transaction_chain
  # %type <ival>  opt_nowait_or_skip

  # %type <list>  OptRoleList AlterOptRoleList
  # %type <defelt>    CreateOptRoleElem AlterOptRoleElem

  # %type <str>       opt_type
  # %type <str>       foreign_server_version opt_foreign_server_version
  # %type <str>       opt_in_database

  # %type <str>       OptSchemaName
  # %type <list>  OptSchemaEltList

  # %type <chr>       am_type

  # %type <boolean> TriggerForSpec TriggerForType
  # %type <ival>  TriggerActionTime
  # %type <list>  TriggerEvents TriggerOneEvent
  # %type <value> TriggerFuncArg
  # %type <node>  TriggerWhen
  # %type <str>       TransitionRelName
  # %type <boolean>   TransitionRowOrTable TransitionOldOrNew
  # %type <node>  TriggerTransition

  # %type <list>  event_trigger_when_list event_trigger_value_list
  # %type <defelt>    event_trigger_when_item
  # %type <chr>       enable_trigger

  # %type <str>       copy_file_name
  #               access_method_clause attr_name
  #               table_access_method_clause name cursor_name file_name
  #               opt_index_name cluster_index_specification

  # %type <list>  func_name handler_name qual_Op qual_all_Op subquery_Op
  #               opt_class opt_inline_handler opt_validator validator_clause
  #               opt_collate

  # %type <range> qualified_name insert_target OptConstrFromTable

  # %type <str>       all_Op MathOp

  # %type <str>       row_security_cmd RowSecurityDefaultForCmd
  # %type <boolean> RowSecurityDefaultPermissive
  # %type <node>  RowSecurityOptionalWithCheck RowSecurityOptionalExpr
  # %type <list>  RowSecurityDefaultToRole RowSecurityOptionalToRole

  # %type <str>       iso_level opt_encoding
  # %type <rolespec> grantee
  # %type <list>  grantee_list
  # %type <accesspriv> privilege
  # %type <list>  privileges privilege_list
  # %type <privtarget> privilege_target
  # %type <objwithargs> function_with_argtypes aggregate_with_argtypes operator_with_argtypes
  # %type <list>  function_with_argtypes_list aggregate_with_argtypes_list operator_with_argtypes_list
  # %type <ival>  defacl_privilege_target
  # %type <defelt>    DefACLOption
  # %type <list>  DefACLOptionList
  # %type <ival>  import_qualification_type
  # %type <importqual> import_qualification
  # %type <node>  vacuum_relation
  # %type <selectlimit> opt_select_limit select_limit limit_clause

  # %type <list>  parse_toplevel stmtmulti routine_body_stmt_list
  #               OptTableElementList TableElementList OptInherit definition
  #               OptTypedTableElementList TypedTableElementList
  #               reloptions opt_reloptions
  #               OptWith opt_definition func_args func_args_list
  #               func_args_with_defaults func_args_with_defaults_list
  #               aggr_args aggr_args_list
  #               func_as createfunc_opt_list opt_createfunc_opt_list alterfunc_opt_list
  #               old_aggr_definition old_aggr_list
  #               oper_argtypes RuleActionList RuleActionMulti
  #               opt_column_list columnList opt_name_list
  #               sort_clause opt_sort_clause sortby_list index_params stats_params
  #               opt_include opt_c_include index_including_params
  #               name_list role_list from_clause from_list opt_array_bounds
  #               qualified_name_list any_name any_name_list type_name_list
  #               any_operator expr_list attrs
  #               distinct_clause opt_distinct_clause
  #               target_list opt_target_list insert_column_list set_target_list
  #               set_clause_list set_clause
  #               def_list operator_def_list indirection opt_indirection
  #               reloption_list TriggerFuncArgs opclass_item_list opclass_drop_list
  #               opclass_purpose opt_opfamily transaction_mode_list_or_empty
  #               OptTableFuncElementList TableFuncElementList opt_type_modifiers
  #               prep_type_clause
  #               execute_param_clause using_clause returning_clause
  #               opt_enum_val_list enum_val_list table_func_column_list
  #               create_generic_options alter_generic_options
  #               relation_expr_list dostmt_opt_list
  #               transform_element_list transform_type_list
  #               TriggerTransitions TriggerReferencing
  #               vacuum_relation_list opt_vacuum_relation_list
  #               drop_option_list

  # %type <node>  opt_routine_body
  # %type <groupclause> group_clause
  # %type <list>  group_by_list
  # %type <node>  group_by_item empty_grouping_set rollup_clause cube_clause
  # %type <node>  grouping_sets_clause
  # %type <node>  opt_publication_for_tables publication_for_tables

  # %type <list>  opt_fdw_options fdw_options
  # %type <defelt>    fdw_option

  # %type <range> OptTempTableName
  # %type <into>  into_clause create_as_target create_mv_target

  # %type <defelt>    createfunc_opt_item common_func_opt_item dostmt_opt_item
  # %type <fun_param> func_arg func_arg_with_default table_func_column aggr_arg
  # %type <fun_param_mode> arg_class
  # %type <typnam>    func_return func_type

  # %type <boolean>  opt_trusted opt_restart_seqs
  # %type <ival>   OptTemp
  # %type <ival>   OptNoLog
  # %type <oncommit> OnCommitOption

  # %type <ival>  for_locking_strength
  # %type <node>  for_locking_item
  # %type <list>  for_locking_clause opt_for_locking_clause for_locking_items
  # %type <list>  locked_rels_list
  # %type <setquantifier> set_quantifier

  # %type <node>  join_qual
  # %type <jtype> join_type

  # %type <list>  extract_list overlay_list position_list
  # %type <list>  substr_list trim_list
  # %type <list>  opt_interval interval_second
  # %type <str>       unicode_normal_form

  # %type <boolean> opt_instead
  # %type <boolean> opt_unique opt_concurrently opt_verbose opt_full
  # %type <boolean> opt_freeze opt_analyze opt_default opt_recheck
  # %type <defelt>    opt_binary copy_delimiter

  # %type <boolean> copy_from opt_program

  # %type <ival>  event cursor_options opt_hold opt_set_data
  # %type <objtype>   object_type_any_name object_type_name object_type_name_on_any_name
  #               drop_type_name

  # %type <node>  fetch_args select_limit_value
  #               offset_clause select_offset_value
  #               select_fetch_first_value I_or_F_const
  # %type <ival>  row_or_rows first_or_next

  # %type <list>  OptSeqOptList SeqOptList OptParenthesizedSeqOptList
  # %type <defelt>    SeqOptElem

  # %type <istmt> insert_rest
  # %type <infer> opt_conf_expr
  # %type <onconflict> opt_on_conflict

  # %type <vsetstmt> generic_set set_rest set_rest_more generic_reset reset_rest
  #                SetResetClause FunctionSetResetClause

  # %type <node>  TableElement TypedTableElement ConstraintElem TableFuncElement
  # %type <node>  columnDef columnOptions
  # %type <defelt>    def_elem reloption_elem old_aggr_elem operator_def_elem
  # %type <node>  def_arg columnElem where_clause where_or_current_clause
  #               a_expr b_expr c_expr AexprConst indirection_el opt_slice_bound
  #               columnref in_expr having_clause func_table xmltable array_expr
  #               OptWhereClause operator_def_arg
  # %type <list>  rowsfrom_item rowsfrom_list opt_col_def_list
  # %type <boolean> opt_ordinality
  # %type <list>  ExclusionConstraintList ExclusionConstraintElem
  # %type <list>  func_arg_list func_arg_list_opt
  # %type <node>  func_arg_expr
  # %type <list>  row explicit_row implicit_row type_list array_expr_list
  # %type <node>  case_expr case_arg when_clause case_default
  # %type <list>  when_clause_list
  # %type <node>  opt_search_clause opt_cycle_clause
  # %type <ival>  sub_type opt_materialized
  # %type <value> NumericOnly
  # %type <list>  NumericOnly_list
  # %type <alias> alias_clause opt_alias_clause opt_alias_clause_for_join_using
  # %type <list>  func_alias_clause
  # %type <sortby>    sortby
  # %type <ielem> index_elem index_elem_options
  # %type <selem> stats_param
  # %type <node>  table_ref
  # %type <jexpr> joined_table
  # %type <range> relation_expr
  # %type <range> relation_expr_opt_alias
  # %type <node>  tablesample_clause opt_repeatable_clause
  # %type <target>    target_el set_target insert_column_item

  # %type <str>       generic_option_name
  # %type <node>  generic_option_arg
  # %type <defelt>    generic_option_elem alter_generic_option_elem
  # %type <list>  generic_option_list alter_generic_option_list

  # %type <ival>  reindex_target_type reindex_target_multitable

  # %type <node>  copy_generic_opt_arg copy_generic_opt_arg_list_item
  # %type <defelt>    copy_generic_opt_elem
  # %type <list>  copy_generic_opt_list copy_generic_opt_arg_list
  # %type <list>  copy_options

  # %type <typnam>    Typename SimpleTypename ConstTypename
  #               GenericType Numeric opt_float
  #               Character ConstCharacter
  #               CharacterWithLength CharacterWithoutLength
  #               ConstDatetime ConstInterval
  #               Bit ConstBit BitWithLength BitWithoutLength
  # %type <str>       character
  # %type <str>       extract_arg
  # %type <boolean> opt_varying opt_timezone opt_no_inherit

  # %type <ival>  Iconst SignedIconst
  # %type <str>       Sconst comment_text notify_payload
  # %type <str>       RoleId opt_boolean_or_string
  # %type <list>  var_list
  # %type <str>       ColId ColLabel BareColLabel
  # %type <str>       NonReservedWord NonReservedWord_or_Sconst
  # %type <str>       var_name type_function_name param_name
  # %type <str>       createdb_opt_name plassign_target
  # %type <node>  var_value zone_value
  # %type <rolespec> auth_ident RoleSpec opt_granted_by

  # %type <keyword> unreserved_keyword type_func_name_keyword
  # %type <keyword> col_name_keyword reserved_keyword
  # %type <keyword> bare_label_keyword

  # %type <node>  TableConstraint TableLikeClause
  # %type <ival>  TableLikeOptionList TableLikeOption
  # %type <str>       column_compression opt_column_compression
  # %type <list>  ColQualList
  # %type <node>  ColConstraint ColConstraintElem ConstraintAttr
  # %type <ival>  key_actions key_delete key_match key_update key_action
  # %type <ival>  ConstraintAttributeSpec ConstraintAttributeElem
  # %type <str>       ExistingIndex

  # %type <list>  constraints_set_list
  # %type <boolean> constraints_set_mode
  # %type <str>       OptTableSpace OptConsTableSpace
  # %type <rolespec> OptTableSpaceOwner
  # %type <ival>  opt_check_option

  # %type <str>       opt_provider security_label

  # %type <target>    xml_attribute_el
  # %type <list>  xml_attribute_list xml_attributes
  # %type <node>  xml_root_version opt_xml_root_standalone
  # %type <node>  xmlexists_argument
  # %type <ival>  document_or_content
  # %type <boolean> xml_whitespace_option
  # %type <list>  xmltable_column_list xmltable_column_option_list
  # %type <node>  xmltable_column_el
  # %type <defelt>    xmltable_column_option_el
  # %type <list>  xml_namespace_list
  # %type <target>    xml_namespace_el

  # %type <node>  func_application func_expr_common_subexpr
  # %type <node>  func_expr func_expr_windowless
  # %type <node>  common_table_expr
  # %type <with>  with_clause opt_with_clause
  # %type <list>  cte_list

  # %type <list>  within_group_clause
  # %type <node>  filter_clause
  # %type <list>  window_clause window_definition_list opt_partition_clause
  # %type <windef>    window_definition over_clause window_specification
  #               opt_frame_clause frame_extent frame_bound
  # %type <ival>  opt_window_exclusion_clause
  # %type <str>       opt_existing_window_name
  # %type <boolean> opt_if_not_exists
  # %type <ival>  generated_when override_kind
  # %type <partspec>  PartitionSpec OptPartitionSpec
  # %type <partelem>  part_elem
  # %type <list>      part_params
  # %type <partboundspec> PartitionBoundSpec
  # %type <list>      hash_partbound
  # %type <defelt>        hash_partbound_elem


  # /*
  #  * Non-keyword token types.  These are hard-wired into the "flex" lexer.
  #  * They must be listed first so that their numeric codes do not depend on
  #  * the set of keywords.  PL/pgSQL depends on this so that it can share the
  #  * same lexer.  If you add/change tokens here, fix PL/pgSQL to match!
  #  *
  #  * UIDENT and USCONST are reduced to IDENT and SCONST in parser.c, so that
  #  * they need no productions here ; but we must assign token codes to them.
  #  *
  #  * DOT_DOT is unused in the core SQL grammar, and so will always provoke
  #  * parse errors.  It is needed by PL/pgSQL.
  #  */
  # %token <str>  IDENT UIDENT FCONST SCONST USCONST BCONST XCONST Op
  # %token <ival> ICONST PARAM
  # %token            TYPECAST DOT_DOT COLON_EQUALS EQUALS_GREATER
  # %token            LESS_EQUALS GREATER_EQUALS NOT_EQUALS



  # /*
  #  * The grammar thinks these are keywords, but they are not in the kwlist.h
  #  * list and so can never be entered directly.  The filter in parser.c
  #  * creates these tokens when required (based on looking one token ahead).
  #  *
  #  * NOT_LA exists so that productions such as NOT LIKE can be given the same
  #  * precedence as LIKE; otherwise they'd effectively have the same precedence
  #  * as NOT, at least with respect to their left-hand subexpression.
  #  * NULLS_LA and WITH_LA are needed to make the grammar LALR(1).
  #  */
  # %token        NOT_LA NULLS_LA WITH_LA

  # /*
  #  * The grammar likewise thinks these tokens are keywords, but they are never
  #  * generated by the scanner.  Rather, they can be injected by parser.c as
  #  * the initial token of the string (using the lookahead-token mechanism
  #  * implemented there).  This provides a way to tell the grammar to parse
  #  * something other than the usual list of SQL commands.
  #  */
  # %token        MODE_TYPE_NAME
  # %token        MODE_PLPGSQL_EXPR
  # %token        MODE_PLPGSQL_ASSIGN1
  # %token        MODE_PLPGSQL_ASSIGN2
  # %token        MODE_PLPGSQL_ASSIGN3


  # /* Precedence: lowest to highest */
  # %nonassoc SET             /* see relation_expr_opt_alias */
  # %left     UNION EXCEPT
  # %left     INTERSECT
  # %left     OR
  # %left     AND
  # %right        NOT
  # %nonassoc IS ISNULL NOTNULL   /* IS sets precedence for IS NULL, etc */
  # %nonassoc '<' '>' '=' LESS_EQUALS GREATER_EQUALS NOT_EQUALS
  # %nonassoc BETWEEN IN_P LIKE ILIKE SIMILAR NOT_LA
  # %nonassoc ESCAPE          /* ESCAPE must be just above LIKE/ILIKE/SIMILAR */
  # /*
  #  * To support target_el without AS, it used to be necessary to assign IDENT an
  #  * explicit precedence just less than Op.  While that's not really necessary
  #  * since we removed postfix operators, it's still helpful to do so because
  #  * there are some other unreserved keywords that need precedence assignments.
  #  * If those keywords have the same precedence as IDENT then they clearly act
  #  * the same as non-keywords, reducing the risk of unwanted precedence effects.
  #  *
  #  * We need to do this for PARTITION, RANGE, ROWS, and GROUPS to support
  #  * opt_existing_window_name (see comment there).
  #  *
  #  * The frame_bound productions UNBOUNDED PRECEDING and UNBOUNDED FOLLOWING
  #  * are even messier: since UNBOUNDED is an unreserved keyword (per spec!),
  #  * there is no principled way to distinguish these from the productions
  #  * a_expr PRECEDING/FOLLOWING.  We hack this up by giving UNBOUNDED slightly
  #  * lower precedence than PRECEDING and FOLLOWING.  At present this doesn't
  #  * appear to cause UNBOUNDED to be treated differently from other unreserved
  #  * keywords anywhere else in the grammar, but it's definitely risky.  We can
  #  * blame any funny behavior of UNBOUNDED on the SQL standard, though.
  #  *
  #  * To support CUBE and ROLLUP in GROUP BY without reserving them, we give them
  #  * an explicit priority lower than '(', so that a rule with CUBE '(' will shift
  #  * rather than reducing a conflicting rule that takes CUBE as a function name.
  #  * Using the same precedence as IDENT seems right for the reasons given above.
  #  */
  # %nonassoc UNBOUNDED       /* ideally would have same precedence as IDENT */
  # %nonassoc IDENT PARTITION RANGE ROWS GROUPS PRECEDING FOLLOWING CUBE ROLLUP
  # %left     Op OPERATOR     /* multi-character ops and user-defined operators */
  # %left     '+' '-'
  # %left     '*' '/' '%'
  # %left     '^'
  # /* Unary Operators */
  # %left     AT              /* sets precedence for AT TIME ZONE */
  # %left     COLLATE
  # %right        UMINUS
  # %left     '[' ']'
  # %left     '(' ')'
  # %left     TYPECAST
  # %left     '.'
  # /*
  #  * These might seem to be low-precedence, but actually they are not part
  #  * of the arithmetic hierarchy at all in their use as JOIN operators.
  #  * We make them high-precedence to support their use as function names.
  #  * They wouldn't be given a precedence at all, were it not that we need
  #  * left-associativity among the JOIN rules themselves.
  #  */
  # %left     JOIN CROSS LEFT FULL RIGHT INNER_P NATURAL
  #
  #   The target production for the whole parse.
  #
  # Ordinarily we parse a list of statements, but if we see one of the
  # special MODE_XXX symbols as first token, we parse something else.
  # The options here correspond to enum RawParseMode, which see for details.
  #
  parse_toplevel <- (
    stmtmulti                           |
    MODE_TYPE_NAME * Typename           |
    MODE_PLPGSQL_EXPR * PLpgSQL_Expr    |
    MODE_PLPGSQL_ASSIGN1 * PLAssignStmt |
    MODE_PLPGSQL_ASSIGN2 * PLAssignStmt |
    MODE_PLPGSQL_ASSIGN3 * PLAssignStmt
  )

  #
  # At top level, we wrap each stmt with a RawStmt node carrying start location
  # and length of the stmt's text.  Notice that the start loc/len are driven
  # entirely from semicolon locations (@2).  It would seem natural to use
  # @1 or @3 to get the true start location of a stmt, but that doesn't work
  # for statements that can start with empty nonterminals (opt_with_clause is
  # the main offender here); as noted in the comments for YYLLOC_DEFAULT,
  # we'd get -1 for the location in such cases.
  # We also take care to discard empty statements entirely.
   #
  stmtmulti <- (
    stmtmulti * ';' * toplevel_stmt |
    toplevel_stmt
  )

  #
  # toplevel_stmt includes BEGIN and END.  stmt does not include them, because
  # those words have different meanings in function bodys.
  #
  toplevel_stmt <- (
    stmt |
    TransactionStmtLegacy
  )

  stmt <- Optional(
    AlterEventTrigStmt         |
    AlterCollationStmt         |
    AlterDatabaseStmt          |
    AlterDatabaseSetStmt       |
    AlterDefaultPrivilegesStmt |
    AlterDomainStmt            |
    AlterEnumStmt              |
    AlterExtensionStmt         |
    AlterExtensionContentsStmt |
    AlterFdwStmt               |
    AlterForeignServerStmt     |
    AlterFunctionStmt          |
    AlterGroupStmt             |
    AlterObjectDependsStmt     |
    AlterObjectSchemaStmt      |
    AlterOwnerStmt             |
    AlterOperatorStmt          |
    AlterTypeStmt              |
    AlterPolicyStmt            |
    AlterSeqStmt               |
    AlterSystemStmt            |
    AlterTableStmt             |
    AlterTblSpcStmt            |
    AlterCompositeTypeStmt     |
    AlterPublicationStmt       |
    AlterRoleSetStmt           |
    AlterRoleStmt              |
    AlterSubscriptionStmt      |
    AlterStatsStmt             |
    AlterTSConfigurationStmt   |
    AlterTSDictionaryStmt      |
    AlterUserMappingStmt       |
    AnalyzeStmt                |
    CallStmt                   |
    CheckPointStmt             |
    ClosePortalStmt            |
    ClusterStmt                |
    CommentStmt                |
    ConstraintsSetStmt         |
    CopyStmt                   |
    CreateAmStmt               |
    CreateAsStmt               |
    CreateAssertionStmt        |
    CreateCastStmt             |
    CreateConversionStmt       |
    CreateDomainStmt           |
    CreateExtensionStmt        |
    CreateFdwStmt              |
    CreateForeignServerStmt    |
    CreateForeignTableStmt     |
    CreateFunctionStmt         |
    CreateGroupStmt            |
    CreateMatViewStmt          |
    CreateOpClassStmt          |
    CreateOpFamilyStmt         |
    CreatePublicationStmt      |
    AlterOpFamilyStmt          |
    CreatePolicyStmt           |
    CreatePLangStmt            |
    CreateSchemaStmt           |
    CreateSeqStmt              |
    CreateStmt                 |
    CreateSubscriptionStmt     |
    CreateStatsStmt            |
    CreateTableSpaceStmt       |
    CreateTransformStmt        |
    CreateTrigStmt             |
    CreateEventTrigStmt        |
    CreateRoleStmt             |
    CreateUserStmt             |
    CreateUserMappingStmt      |
    CreatedbStmt               |
    DeallocateStmt             |
    DeclareCursorStmt          |
    DefineStmt                 |
    DeleteStmt                 |
    DiscardStmt                |
    DoStmt                     |
    DropCastStmt               |
    DropOpClassStmt            |
    DropOpFamilyStmt           |
    DropOwnedStmt              |
    DropStmt                   |
    DropSubscriptionStmt       |
    DropTableSpaceStmt         |
    DropTransformStmt          |
    DropRoleStmt               |
    DropUserMappingStmt        |
    DropdbStmt                 |
    ExecuteStmt                |
    ExplainStmt                |
    FetchStmt                  |
    GrantStmt                  |
    GrantRoleStmt              |
    ImportForeignSchemaStmt    |
    IndexStmt                  |
    InsertStmt                 |
    ListenStmt                 |
    RefreshMatViewStmt         |
    LoadStmt                   |
    LockStmt                   |
    NotifyStmt                 |
    PrepareStmt                |
    ReassignOwnedStmt          |
    ReindexStmt                |
    RemoveAggrStmt             |
    RemoveFuncStmt             |
    RemoveOperStmt             |
    RenameStmt                 |
    RevokeStmt                 |
    RevokeRoleStmt             |
    RuleStmt                   |
    SecLabelStmt               |
    SelectStmt                 |
    TransactionStmt            |
    TruncateStmt               |
    UnlistenStmt               |
    UpdateStmt                 |
    VacuumStmt                 |
    VariableResetStmt          |
    VariableSetStmt            |
    VariableShowStmt           |
    ViewStmt
  )

  #****************************************************************************
  #
  # CALL statement
  #
  #****************************************************************************/

  CallStmt <- (
    CALL * func_application
  )

  #****************************************************************************
  #
  # Create a new Postgres DBMS role
  #
  #****************************************************************************/

  CreateRoleStmt <- (
    CREATE * ROLE * RoleId * opt_with * OptRoleList
  )


  opt_with <- Optional(
    WITH |
    WITH_LA
  )

  #
  # Options for CREATE ROLE and ALTER ROLE (also used by CREATE/ALTER USER
  # for backwards compatibility).  Note: the only option required by SQL99
  # is "WITH ADMIN name".
  #
  OptRoleList <- Optional(
    OptRoleList * CreateOptRoleElem
  )

  AlterOptRoleList <- Optional(
    AlterOptRoleList * AlterOptRoleElem
  )

  AlterOptRoleElem <- (
    PASSWORD * Sconst                 |
    PASSWORD * NULL_P                 |
    ENCRYPTED * PASSWORD * Sconst     |
    UNENCRYPTED * PASSWORD * Sconst   |
    INHERIT                           |
    CONNECTION * LIMIT * SignedIconst |
    VALID * UNTIL * Sconst            |

    #  Supported but not documented for roles, for use by ALTER GROUP. */
    USER * role_list |
    IDENT
  )

  CreateOptRoleElem <- (
    AlterOptRoleElem |
    # The following are not supported by ALTER ROLE/USER/GROUP */
    SYSID * Iconst          |
    ADMIN * role_list       |
    ROLE * role_list        |
    IN_P * ROLE * role_list |
    IN_P * GROUP_P * role_list
  )


  #****************************************************************************
  #
  # Create a new Postgres DBMS user (role with implied login ability)
  #
  #****************************************************************************/

  CreateUserStmt <- (
    CREATE * USER * RoleId * opt_with * OptRoleList
  )


  #****************************************************************************
  #
  # Alter a postgresql DBMS role
  #
  #****************************************************************************/

  AlterRoleStmt <- (
    ALTER * ROLE * RoleSpec * opt_with * AlterOptRoleList |
    ALTER * USER * RoleSpec * opt_with * AlterOptRoleList
  )

  opt_in_database <- Optional(
    IN_P * DATABASE * name
  )

  AlterRoleSetStmt <- (
    ALTER * ROLE * RoleSpec * opt_in_database * SetResetClause |
    ALTER * ROLE * ALL      * opt_in_database * SetResetClause |
    ALTER * USER * RoleSpec * opt_in_database * SetResetClause |
    ALTER * USER * ALL      * opt_in_database * SetResetClause
  )


  #****************************************************************************
  #
  # Drop a postgresql DBMS role
  #
  # XXX Ideally this would have CASCADE/RESTRICT options, but a role
  # might own objects in multiple databases, and there is presently no way to
  # implement cascading to other databases.  So we always behave as RESTRICT.
  #****************************************************************************/

  DropRoleStmt <- (
    DROP * ROLE    * role_list                     |
    DROP * ROLE    * IF_P * EXISTS * role_list     |
    DROP * USER    * role_list                     |
    DROP * USER    * IF_P * EXISTS * role_list     |
    DROP * GROUP_P * role_list                     |
    DROP * GROUP_P * IF_P * EXISTS * role_list
  )


  #****************************************************************************
  #
  # Create a postgresql group (role without login ability)
  #
  #****************************************************************************/

  CreateGroupStmt <- (
    CREATE * GROUP_P * RoleId * opt_with * OptRoleList
  )


  #****************************************************************************
  #
  # Alter a postgresql group
  #
  #****************************************************************************/

  AlterGroupStmt <- (
    ALTER * GROUP_P * RoleSpec * add_drop * USER * role_list
  )

  add_drop <- (
    ADD_P |
    DROP
  )


  #****************************************************************************
  #
  # Manipulate a schema
  #
  #****************************************************************************/

  CreateSchemaStmt <- (
    CREATE * SCHEMA * OptSchemaName * AUTHORIZATION * RoleSpec * OptSchemaEltList                       |
    CREATE * SCHEMA * ColId * OptSchemaEltList                                                          |
    CREATE * SCHEMA * IF_P * NOT * EXISTS * OptSchemaName * AUTHORIZATION * RoleSpec * OptSchemaEltList |
    CREATE * SCHEMA * IF_P * NOT * EXISTS * ColId * OptSchemaEltList
  )

  OptSchemaName <- Optional(
    ColId
  )

  OptSchemaEltList <- Optional(
    OptSchemaEltList * schema_stmt
  )

  #
  #   schema_stmt are the ones that can show up inside a CREATE SCHEMA
  #   statement (in addition to by themselves).
  #
  schema_stmt <- (
    CreateStmt     |
    IndexStmt      |
    CreateSeqStmt  |
    CreateTrigStmt |
    GrantStmt      |
    ViewStmt
  )


  #****************************************************************************
  #
  # Set PG internal variable
  #     SET name TO 'var_value'
  # Include SQL syntax (thomas 1997-10-22):
  #     SET TIME ZONE 'var_value'
  #
  #****************************************************************************/

  VariableSetStmt <- (
    SET * set_rest           |
    SET * LOCAL * set_rest   |
    SET * SESSION * set_rest
  )

  set_rest <- (
    TRANSACTION * transaction_mode_list                                  |
    SESSION * CHARACTERISTICS * AS * TRANSACTION * transaction_mode_list |
    set_rest_more
  )

  generic_set <- (
    var_name * TO * var_list   |
    var_name * '=' * var_list  |
    var_name * TO * DEFAULT    |
    var_name * '=' * DEFAULT
  )

  set_rest_more <- (
    # Generic * SET * syntaxes:
      generic_set               |
    var_name * FROM * CURRENT_P |

    # Special syntaxes mandated by SQL standard:
    TIME * ZONE * zone_value                            |
    CATALOG_P * Sconst                                  |
    SCHEMA * Sconst                                     |
    NAMES * opt_encoding                                |
    ROLE * NonReservedWord_or_Sconst                    |
    SESSION * AUTHORIZATION * NonReservedWord_or_Sconst |
    SESSION * AUTHORIZATION * DEFAULT                   |
    XML_P * OPTION * document_or_content                |

    # Special syntaxes invented by PostgreSQL: */
    TRANSACTION * SNAPSHOT * Sconst
  )

  var_name <- (
    ColId |
    var_name * '.' * ColId
  )

  var_list <- (
    var_value |
    var_list * ',' * var_value
  )

  var_value <- (
    opt_boolean_or_string |
    NumericOnly
  )

  iso_level <- (
    READ * UNCOMMITTED |
    READ * COMMITTED     |
    REPEATABLE * READ    |
    SERIALIZABLE
  )

  opt_boolean_or_string <- (
    TRUE_P |
    FALSE_P  |
    ON       |
    #
    # OFF is also accepted as a boolean value, but is handled by
    # the NonReservedWord rule.  The action for booleans and strings
    # is the same, so we don't need to distinguish them here.
    #
    NonReservedWord_or_Sconst
  )

  # Timezone values can be:
  # - a string such as 'pst8pdt'
  # - an identifier such as "pst8pdt"
  # - an integer or floating point number
  # - a time interval per SQL99
  # ColId gives reduce/reduce errors against ConstInterval and LOCAL,
  # so use IDENT (meaning we reject anything that is a key word).
  #
  zone_value <- (
    Sconst                                      |
    IDENT                                       |
    ConstInterval * Sconst * opt_interval       |
    ConstInterval * '(' * Iconst * ')' * Sconst |
    NumericOnly                                 |
    DEFAULT                                     |
    LOCAL
  )

  opt_encoding <- Optional(
    Sconst |
    DEFAULT
  )

  NonReservedWord_or_Sconst <- (
    NonReservedWord |
    Sconst
  )

  VariableResetStmt <- (
    RESET * reset_rest
  )

  reset_rest <- (
    generic_reset                   |
    TIME * ZONE                     |
    TRANSACTION * ISOLATION * LEVEL |
    SESSION * AUTHORIZATION
  )

  generic_reset <- (
    var_name |
    ALL
  )

  # SetResetClause allows SET or RESET without LOCAL */
  SetResetClause <- (
    SET * set_rest |
    VariableResetStmt
  )

  # SetResetClause allows SET or RESET without LOCAL */
  FunctionSetResetClause <- (
    SET * set_rest_more |
    VariableResetStmt
  )


  VariableShowStmt <- (
    SHOW * var_name                        |
    SHOW * TIME * ZONE                     |
    SHOW * TRANSACTION * ISOLATION * LEVEL |
    SHOW * SESSION * AUTHORIZATION         |
    SHOW * ALL
  )


  ConstraintsSetStmt <- (
    SET * CONSTRAINTS * constraints_set_list * constraints_set_mode
  )

  constraints_set_list <- (
    ALL |
    qualified_name_list
  )

  constraints_set_mode <- (
    DEFERRED |
    IMMEDIATE
  )


  #
  # Checkpoint statement
  #
  CheckPointStmt <- (
    CHECKPOINT
  )


  #****************************************************************************
  #
  # DISCARD { ALL | TEMP | PLANS | SEQUENCES }
  #
  #****************************************************************************/

  DiscardStmt <- (
    DISCARD * ALL       |
    DISCARD * TEMP      |
    DISCARD * TEMPORARY |
    DISCARD * PLANS     |
    DISCARD * SEQUENCES

  )


  #****************************************************************************
  #
  #   ALTER [ TABLE | INDEX | SEQUENCE | VIEW | MATERIALIZED VIEW | FOREIGN TABLE ] variations
  #
  # Note: we accept all subcommands for each of the variants, and sort
  # out what's really legal at execution time.
  #****************************************************************************/

  AlterTableStmt <- (
    ALTER * TABLE * relation_expr * alter_table_cmds                                                                           |
    ALTER * TABLE * IF_P * EXISTS * relation_expr * alter_table_cmds                                                             |
    ALTER * TABLE * relation_expr * partition_cmd                                                                                |
    ALTER * TABLE * IF_P * EXISTS * relation_expr * partition_cmd                                                                |
    ALTER * TABLE * ALL * IN_P * TABLESPACE * name * SET * TABLESPACE * name * opt_nowait                                        |
    ALTER * TABLE * ALL * IN_P * TABLESPACE * name * OWNED * BY * role_list * SET * TABLESPACE * name * opt_nowait               |
    ALTER * INDEX * qualified_name * alter_table_cmds                                                                            |
    ALTER * INDEX * IF_P * EXISTS * qualified_name * alter_table_cmds                                                            |
    ALTER * INDEX * qualified_name * index_partition_cmd                                                                         |
    ALTER * INDEX * ALL * IN_P * TABLESPACE * name * SET * TABLESPACE * name * opt_nowait                                        |
    ALTER * INDEX * ALL * IN_P * TABLESPACE * name * OWNED * BY * role_list * SET * TABLESPACE * name * opt_nowait               |
    ALTER * SEQUENCE * qualified_name * alter_table_cmds                                                                         |
    ALTER * SEQUENCE * IF_P * EXISTS * qualified_name * alter_table_cmds                                                         |
    ALTER * VIEW * qualified_name * alter_table_cmds                                                                             |
    ALTER * VIEW * IF_P * EXISTS * qualified_name * alter_table_cmds                                                             |
    ALTER * MATERIALIZED * VIEW * qualified_name * alter_table_cmds                                                              |
    ALTER * MATERIALIZED * VIEW * IF_P * EXISTS * qualified_name * alter_table_cmds                                              |
    ALTER * MATERIALIZED * VIEW * ALL * IN_P * TABLESPACE * name * SET * TABLESPACE * name * opt_nowait                          |
    ALTER * MATERIALIZED * VIEW * ALL * IN_P * TABLESPACE * name * OWNED * BY * role_list * SET * TABLESPACE * name * opt_nowait |
    ALTER * FOREIGN * TABLE * relation_expr * alter_table_cmds                                                                   |
    ALTER * FOREIGN * TABLE * IF_P * EXISTS * relation_expr * alter_table_cmds
  )

  alter_table_cmds <- (
    alter_table_cmd |
    alter_table_cmds * ',' * alter_table_cmd
  )

  partition_cmd <- (
    # ALTER TABLE <name> ATTACH PARTITION <table_name> FOR VALUES */
      ATTACH * PARTITION * qualified_name * PartitionBoundSpec |

    # ALTER TABLE <name> DETACH PARTITION <partition_name> [CONCURRENTLY] */
    DETACH * PARTITION * qualified_name * opt_concurrently |
    DETACH * PARTITION * qualified_name * FINALIZE
  )

  index_partition_cmd <- (
    # ALTER INDEX <name> ATTACH PARTITION <index_name> */
      ATTACH * PARTITION * qualified_name
  )

  alter_table_cmd <- (
    # ALTER TABLE <name> ADD <coldef> */
      ADD_P * columnDef |

    # ALTER TABLE <name> ADD IF NOT EXISTS <coldef> */
    ADD_P * IF_P * NOT * EXISTS * columnDef |

    # ALTER TABLE <name> ADD COLUMN <coldef> */ 
    ADD_P * COLUMN * columnDef |

    # ALTER TABLE <name> ADD COLUMN IF NOT EXISTS <coldef> */ 
    ADD_P * COLUMN * IF_P * NOT * EXISTS * columnDef |

    # ALTER TABLE <name> ALTER [COLUMN] <colname> {SET DEFAULT <expr>|DROP DEFAULT} */ 
    ALTER * opt_column * ColId * alter_column_default |

    # ALTER TABLE <name> ALTER [COLUMN] <colname> DROP NOT NULL */ 
    ALTER * opt_column * ColId * DROP * NOT * NULL_P |

    # ALTER TABLE <name> ALTER [COLUMN] <colname> SET NOT NULL */ 
    ALTER * opt_column * ColId * SET * NOT * NULL_P |

    # ALTER TABLE <name> ALTER [COLUMN] <colname> DROP EXPRESSION */ 
    ALTER * opt_column * ColId * DROP * EXPRESSION |

    # ALTER TABLE <name> ALTER [COLUMN] <colname> DROP EXPRESSION IF EXISTS */ 
    ALTER * opt_column * ColId * DROP * EXPRESSION * IF_P * EXISTS |

    # ALTER TABLE <name> ALTER [COLUMN] <colname> SET STATISTICS <SignedIconst> */ 
    ALTER * opt_column * ColId * SET * STATISTICS * SignedIconst |

    # ALTER TABLE <name> ALTER [COLUMN] <colnum> SET STATISTICS <SignedIconst> */ 
    ALTER * opt_column * Iconst * SET * STATISTICS * SignedIconst |

    # ALTER TABLE <name> ALTER [COLUMN] <colname> SET ( column_parameter = value [, ... ] ) */ 
    ALTER * opt_column * ColId * SET * reloptions |

    # ALTER TABLE <name> ALTER [COLUMN] <colname> RESET ( column_parameter = value [, ... ] ) */ 
    ALTER * opt_column * ColId * RESET * reloptions |

    # ALTER TABLE <name> ALTER [COLUMN] <colname> SET STORAGE <storagemode> */ 
    ALTER * opt_column * ColId * SET * STORAGE * ColId |

    # ALTER TABLE <name> ALTER [COLUMN] <colname> SET COMPRESSION <cm> */ 
    ALTER * opt_column * ColId * SET * column_compression |

    # ALTER TABLE <name> ALTER [COLUMN] <colname> ADD GENERATED ... AS IDENTITY ... */ 
    ALTER * opt_column * ColId * ADD_P * GENERATED * generated_when * AS * IDENTITY_P * OptParenthesizedSeqOptList |

    # ALTER TABLE <name> ALTER [COLUMN] <colname> SET <sequence options>/RESET */ 
    ALTER * opt_column * ColId * alter_identity_column_option_list |

    # ALTER TABLE <name> ALTER [COLUMN] <colname> DROP IDENTITY */ 
    ALTER * opt_column * ColId * DROP * IDENTITY_P |

    # ALTER TABLE <name> ALTER [COLUMN] <colname> DROP IDENTITY IF EXISTS */ 
    ALTER * opt_column * ColId * DROP * IDENTITY_P * IF_P * EXISTS |

    # ALTER TABLE <name> DROP [COLUMN] IF EXISTS <colname> [RESTRICT|CASCADE] */ 
    DROP * opt_column * IF_P * EXISTS * ColId * opt_drop_behavior |

    # ALTER TABLE <name> DROP [COLUMN] <colname> [RESTRICT|CASCADE] */ 
    DROP * opt_column * ColId * opt_drop_behavior |

    # 
    # ALTER TABLE <name> ALTER [COLUMN] <colname> [SET DATA] TYPE <typename> 
    #      [ USING <expression> ] 
    # 
    ALTER * opt_column * ColId * opt_set_data * TYPE_P * Typename * opt_collate_clause * alter_using |

    # ALTER FOREIGN TABLE <name> ALTER [COLUMN] <colname> OPTIONS */ 
    ALTER * opt_column * ColId * alter_generic_options |

    # ALTER TABLE <name> ADD CONSTRAINT ... */ 
    ADD_P * TableConstraint |

    # ALTER TABLE <name> ALTER CONSTRAINT ... */ 
    ALTER * CONSTRAINT * name * ConstraintAttributeSpec |

    # ALTER TABLE <name> VALIDATE CONSTRAINT ... */ 
    VALIDATE * CONSTRAINT * name |

    # ALTER TABLE <name> DROP CONSTRAINT IF EXISTS <name> [RESTRICT|CASCADE] */ 
    DROP * CONSTRAINT * IF_P * EXISTS * name * opt_drop_behavior |

    # ALTER TABLE <name> DROP CONSTRAINT <name> [RESTRICT|CASCADE] */ 
    DROP * CONSTRAINT * name * opt_drop_behavior |

    # ALTER TABLE <name> SET WITHOUT OIDS, for backward compat */ 
    SET * WITHOUT * OIDS |

    # ALTER TABLE <name> CLUSTER ON <indexname> */ 
    CLUSTER * ON * name |

    # ALTER TABLE <name> SET WITHOUT CLUSTER */ 
    SET * WITHOUT * CLUSTER |

    # ALTER TABLE <name> SET LOGGED */ 
    SET * LOGGED |

    # ALTER TABLE <name> SET UNLOGGED */ 
    SET * UNLOGGED |

    # ALTER TABLE <name> ENABLE TRIGGER <trig> */ 
    ENABLE_P * TRIGGER * name |

    # ALTER TABLE <name> ENABLE ALWAYS TRIGGER <trig> */ 
    ENABLE_P * ALWAYS * TRIGGER * name |

    # ALTER TABLE <name> ENABLE REPLICA TRIGGER <trig> */ 
    ENABLE_P * REPLICA * TRIGGER * name |

    # ALTER TABLE <name> ENABLE TRIGGER ALL */ 
    ENABLE_P * TRIGGER * ALL |

    # ALTER TABLE <name> ENABLE TRIGGER USER */ 
    ENABLE_P * TRIGGER * USER |

    # ALTER TABLE <name> DISABLE TRIGGER <trig> */ 
    DISABLE_P * TRIGGER * name |

    # ALTER TABLE <name> DISABLE TRIGGER ALL */ 
    DISABLE_P * TRIGGER * ALL |

    # ALTER TABLE <name> DISABLE TRIGGER USER */ 
    DISABLE_P * TRIGGER * USER |

    # ALTER TABLE <name> ENABLE RULE <rule> */ 
    ENABLE_P * RULE * name |

    # ALTER TABLE <name> ENABLE ALWAYS RULE <rule> */ 
    ENABLE_P * ALWAYS * RULE * name |

    # ALTER TABLE <name> ENABLE REPLICA RULE <rule> */ 
    ENABLE_P * REPLICA * RULE * name |

    # ALTER TABLE <name> DISABLE RULE <rule> */ 
    DISABLE_P * RULE * name |

    # ALTER TABLE <name> INHERIT <parent> */ 
    INHERIT * qualified_name |

    # ALTER TABLE <name> NO INHERIT <parent> */ 
    NO * INHERIT * qualified_name |

    # ALTER TABLE <name> OF <type_name> */ 
    OF * any_name |

    # ALTER TABLE <name> NOT OF */ 
    NOT * OF |

    # ALTER TABLE <name> OWNER TO RoleSpec */ 
    OWNER * TO * RoleSpec |

    # ALTER TABLE <name> SET ACCESS METHOD <amname> */ 
    SET * ACCESS * METHOD * name |

    # ALTER TABLE <name> SET TABLESPACE <tablespacename> */ 
    SET * TABLESPACE * name |

    # ALTER TABLE <name> SET (...) */ 
    SET * reloptions |

    # ALTER TABLE <name> RESET (...) */ 
    RESET * reloptions |

    # ALTER TABLE <name> REPLICA IDENTITY */ 
    REPLICA * IDENTITY_P * replica_identity |

    # ALTER TABLE <name> ENABLE ROW LEVEL SECURITY */ 
    ENABLE_P * ROW * LEVEL * SECURITY |

    # ALTER TABLE <name> DISABLE ROW LEVEL SECURITY */ 
    DISABLE_P * ROW * LEVEL * SECURITY |

    # ALTER TABLE <name> FORCE ROW LEVEL SECURITY */ 
    FORCE * ROW * LEVEL * SECURITY |

    # ALTER TABLE <name> NO FORCE ROW LEVEL SECURITY */ 
    NO * FORCE * ROW * LEVEL * SECURITY |
    alter_generic_options
  )

  alter_column_default <- (
    SET * DEFAULT * a_expr |
    DROP * DEFAULT
  )

  opt_drop_behavior <- Optional(
    CASCADE |
    RESTRICT
  )

  opt_collate_clause <- Optional(
    COLLATE * any_name
  )

  alter_using <- Optional(
    USING * a_expr
  )

  replica_identity <- (
    NOTHING |
    FULL      |
    DEFAULT   |
    USING * INDEX * name

  )

  reloptions <- (
      '(' * reloption_list * ')'
  )

  opt_reloptions <- Optional(
    WITH * reloptions
  )

  reloption_list <- (
    reloption_elem |
    reloption_list * ',' * reloption_elem
  )

  # This should match def_elem and also allow qualified names */
  reloption_elem <- (
    ColLabel * '=' * def_arg                |
    ColLabel                                  |
    ColLabel * '.' * ColLabel * '=' * def_arg |
    ColLabel * '.' * ColLabel
  )

  alter_identity_column_option_list <- (
    alter_identity_column_option |
    alter_identity_column_option_list * alter_identity_column_option
  )

  alter_identity_column_option <- (
    RESTART                        |
    RESTART * opt_with * NumericOnly |
    SET * SeqOptElem                 |
    SET * GENERATED * generated_when
  )

  PartitionBoundSpec <- (
    # a HASH partition */
      FOR * VALUES * WITH * '(' * hash_partbound * ')' |

    # a LIST partition */ 
    FOR * VALUES * IN_P * '(' * expr_list * ')' |

    # a RANGE partition */ 
    FOR * VALUES * FROM * '(' * expr_list * ')' * TO * '(' * expr_list * ')' |

    # a DEFAULT partition */ 
    DEFAULT
  )

  hash_partbound_elem <- (
    NonReservedWord * Iconst
  )

  hash_partbound <- (
    hash_partbound_elem |
    hash_partbound * ',' * hash_partbound_elem
  )

  #****************************************************************************
  #
  #   ALTER TYPE
  #
  # really variants of the ALTER TABLE subcommands with different spellings
  #****************************************************************************/

  AlterCompositeTypeStmt <- (
    ALTER * TYPE_P * any_name * alter_type_cmds

  )

  alter_type_cmds <- (
    alter_type_cmd |
    alter_type_cmds * ',' * alter_type_cmd
  )

  alter_type_cmd <- (
    # ALTER TYPE <name> ADD ATTRIBUTE <coldef> [RESTRICT|CASCADE] */
      ADD_P * ATTRIBUTE * TableFuncElement * opt_drop_behavior |

    # ALTER TYPE <name> DROP ATTRIBUTE IF EXISTS <attname> [RESTRICT|CASCADE] */ 
    DROP * ATTRIBUTE * IF_P * EXISTS * ColId * opt_drop_behavior |

    # ALTER TYPE <name> DROP ATTRIBUTE <attname> [RESTRICT|CASCADE] */ 
    DROP * ATTRIBUTE * ColId * opt_drop_behavior |

    # ALTER TYPE <name> ALTER ATTRIBUTE <attname> [SET DATA] TYPE <typename> [RESTRICT|CASCADE] */ 
    ALTER * ATTRIBUTE * ColId * opt_set_data * TYPE_P * Typename * opt_collate_clause * opt_drop_behavior
  )


  #****************************************************************************
  #
  #       QUERY :
  #               close <portalname>
  #
  #****************************************************************************/

  ClosePortalStmt <- (
    CLOSE * cursor_name |
    CLOSE * ALL
  )


  #****************************************************************************
  #
  #       QUERY :
  #               COPY relname [(columnList)] FROM/TO file [WITH] [(options)]
  #               COPY ( query ) TO file  [WITH] [(options)]
  #
  #               where 'query' can be one of:
  #               { SELECT | UPDATE | INSERT | DELETE }
  #
  #               and 'file' can be one of:
  #               { PROGRAM 'command' | STDIN | STDOUT | 'filename' }
  #
  #               In the preferred syntax the options are comma-separated
  #               and use generic identifiers instead of keywords.  The pre-9.0
  #               syntax had a hard-wired, space-separated set of options.
  #
  #               Really old syntax, from versions 7.2 and prior:
  #               COPY [ BINARY ] table FROM/TO file
  #                   [ [ USING ] DELIMITERS 'delimiter' ] ]
  #                   [ WITH NULL AS 'null string' ]
  #               This option placement is not supported with COPY (query...).
  #
  #****************************************************************************/

  CopyStmt <- (
    COPY * opt_binary * qualified_name * opt_column_list *
        copy_from * opt_program * copy_file_name * copy_delimiter * opt_with *
        copy_options * where_clause |
    COPY * '(' * PreparableStmt * ')' * TO * opt_program * copy_file_name * opt_with * copy_options
  )

  copy_from <- (
    FROM |
    TO
  )

  opt_program <- Optional(
    PROGRAM
  )

  #
  # copy_file_name NULL indicates stdio is used. Whether stdin or stdout is
  # used depends on the direction. (It really doesn't make sense to copy from
  # stdout. We silently correct the "typo".)       - AY 9/94
  #
  copy_file_name <- (
    Sconst |
    STDIN    |
    STDOUT
  )

  copy_options <- (
    copy_opt_list |
    '(' * copy_generic_opt_list * ')'
  )

  # old COPY option syntax */
  copy_opt_list <- Optional(
    copy_opt_list * copy_opt_item
  )

  copy_opt_item <- (
    BINARY                          |
    FREEZE                            |
    DELIMITER * opt_as * Sconst       |
    NULL_P * opt_as * Sconst          |
    CSV                               |
    HEADER_P                          |
    QUOTE * opt_as * Sconst           |
    ESCAPE * opt_as * Sconst          |
    FORCE * QUOTE * columnList        |
    FORCE * QUOTE * '*'               |
    FORCE * NOT * NULL_P * columnList |
    FORCE * NULL_P * columnList       |
    ENCODING * Sconst
  )

  # The following exist for backward compatibility with very old versions */

  opt_binary <- Optional(
    BINARY
  )

  copy_delimiter <- Optional(
    opt_using * DELIMITERS * Sconst
  )

  opt_using <- Optional(
    USING
  )

  # new COPY option syntax */
  copy_generic_opt_list <- (
    copy_generic_opt_elem |
    copy_generic_opt_list * ',' * copy_generic_opt_elem
  )

  copy_generic_opt_elem <- (
    ColLabel * copy_generic_opt_arg
  )

  copy_generic_opt_arg <- Optional(
    opt_boolean_or_string |
    NumericOnly             |
    '*'                     |
    '(' * copy_generic_opt_arg_list * ')'
  )

  copy_generic_opt_arg_list <- (
    copy_generic_opt_arg_list_item |
    copy_generic_opt_arg_list * ',' * copy_generic_opt_arg_list_item
  )

  # beware of emitting non-string list elements here; see commands/define.c */
  copy_generic_opt_arg_list_item <- (
    opt_boolean_or_string
  )


  #****************************************************************************
  #
  #       QUERY :
  #               CREATE TABLE relname
  #
  #****************************************************************************/

  CreateStmt <- (
    CREATE * OptTemp * TABLE * qualified_name * '(' * OptTableElementList * ')' *
        OptInherit * OptPartitionSpec * table_access_method_clause * OptWith *
        OnCommitOption * OptTableSpace |
    CREATE * OptTemp * TABLE * IF_P * NOT * EXISTS * qualified_name * '(' *
        OptTableElementList * ')' * OptInherit * OptPartitionSpec * table_access_method_clause *
        OptWith * OnCommitOption * OptTableSpace |
    CREATE * OptTemp * TABLE * qualified_name * OF * any_name *
        OptTypedTableElementList * OptPartitionSpec * table_access_method_clause *
        OptWith * OnCommitOption * OptTableSpace |
    CREATE * OptTemp * TABLE * IF_P * NOT * EXISTS * qualified_name * OF * any_name *
        OptTypedTableElementList * OptPartitionSpec * table_access_method_clause *
        OptWith * OnCommitOption * OptTableSpace |
    CREATE * OptTemp * TABLE * qualified_name * PARTITION * OF * qualified_name *
        OptTypedTableElementList * PartitionBoundSpec * OptPartitionSpec *
        table_access_method_clause * OptWith * OnCommitOption * OptTableSpace |
    CREATE * OptTemp * TABLE * IF_P * NOT * EXISTS * qualified_name * PARTITION * OF *
        qualified_name * OptTypedTableElementList * PartitionBoundSpec * OptPartitionSpec *
        table_access_method_clause * OptWith * OnCommitOption * OptTableSpace
  )

  #
  # Redundancy here is needed to avoid shift/reduce conflicts,
  # since TEMP is not a reserved word.  See also OptTempTableName.
  #
  # NOTE: we accept both GLOBAL and LOCAL options.  They currently do nothing,
  # but future versions might consider GLOBAL to request SQL-spec-compliant
  # temp table behavior, so warn about that.  Since we have no modules the
  # LOCAL keyword is really meaningless; furthermore, some other products
  # implement LOCAL as meaning the same as our default temp table behavior,
  # so we'll probably continue to treat LOCAL as a noise word.
  #
  OptTemp <- Optional(
    TEMPORARY        |
    TEMP               |
    LOCAL * TEMPORARY  |
    LOCAL * TEMP       |
    GLOBAL * TEMPORARY |
    GLOBAL * TEMP      |
    UNLOGGED
  )

  OptTableElementList <- Optional(
    TableElementList
  )

  OptTypedTableElementList <- Optional(
      '(' * TypedTableElementList * ')'
  )

  TableElementList <- (
    TableElement |
    TableElementList * ',' * TableElement
  )

  TypedTableElementList <- (
    TypedTableElement |
    TypedTableElementList * ',' * TypedTableElement
  )

  TableElement <- (
    columnDef     |
    TableLikeClause |
    TableConstraint
  )

  TypedTableElement <- (
    columnOptions |
    TableConstraint
  )

  columnDef <- (
    ColId * Typename * opt_column_compression * create_generic_options * ColQualList
  )

  columnOptions <- (
    ColId * ColQualList |
    ColId * WITH * OPTIONS * ColQualList
  )

  column_compression <- (
    COMPRESSION * ColId |
    COMPRESSION * DEFAULT
  )

  opt_column_compression <- Optional(
    column_compression
  )

  ColQualList <- Optional(
    ColQualList * ColConstraint
  )

  ColConstraint <- (
    CONSTRAINT * name * ColConstraintElem |
    ColConstraintElem                       |
    ConstraintAttr                          |
    COLLATE * any_name
  )

  # DEFAULT NULL is already the default for Postgres.
  # But define it here and carry it forward into the system
  # to make it explicit.
  # - thomas 1998-09-13
  #
  # WITH NULL and NULL are not SQL-standard syntax elements,
  # so leave them out. Use DEFAULT NULL to explicitly indicate
  # that a column may have that value. WITH NULL leads to
  # shift/reduce conflicts with WITH TIME ZONE anyway.
  # - thomas 1999-01-08
  #
  # DEFAULT expression must be b_expr not a_expr to prevent shift/reduce
  # conflict on NOT (since NOT might start a subsequent NOT NULL constraint,
  # or be part of a_expr NOT LIKE or similar constructs).
  #
  ColConstraintElem <- (
    NOT * NULL_P                                                            |
    NULL_P                                                                    |
    UNIQUE * opt_definition * OptConsTableSpace                               |
    PRIMARY * KEY * opt_definition * OptConsTableSpace                        |
    CHECK * '(' * a_expr * ')' * opt_no_inherit                               |
    DEFAULT * b_expr                                                          |
    GENERATED * generated_when * AS * IDENTITY_P * OptParenthesizedSeqOptList |
    GENERATED * generated_when * AS * '(' * a_expr * ')' * STORED             |
    REFERENCES * qualified_name * opt_column_list * key_match * key_actions
  )

  generated_when <- (
    ALWAYS |
    BY * DEFAULT
  )

  #
  # ConstraintAttr represents constraint attributes, which we parse as if
  # they were independent constraint clauses, in order to avoid shift/reduce
  # conflicts (since NOT might start either an independent NOT NULL clause
  # or an attribute).  parse_utilcmd.c is responsible for attaching the
  # attribute information to the preceding "real" constraint node, and for
  # complaining if attribute clauses appear in the wrong place or wrong
  # combinations.
  #
  # See also ConstraintAttributeSpec, which can be used in places where
  # there is no parsing conflict.  (Note: currently, NOT VALID and NO INHERIT
  # are allowed clauses in ConstraintAttributeSpec, but not here.  Someday we
  # might need to allow them here too, but for the moment it doesn't seem
  # useful in the statements that use ConstraintAttr.)
  #
  ConstraintAttr <- (
    DEFERRABLE         |
    NOT * DEFERRABLE     |
    INITIALLY * DEFERRED |
    INITIALLY * IMMEDIATE
  )


  TableLikeClause <- (
    LIKE * qualified_name * TableLikeOptionList
  )

  TableLikeOptionList <- Optional(
    TableLikeOptionList * INCLUDING * TableLikeOption |
    TableLikeOptionList * EXCLUDING * TableLikeOption
  )

  TableLikeOption <- (
    COMMENTS  |
    COMPRESSION |
    CONSTRAINTS |
    DEFAULTS    |
    IDENTITY_P  |
    GENERATED   |
    INDEXES     |
    STATISTICS  |
    STORAGE     |
    ALL
  )


  # ConstraintElem specifies constraint syntax which is not embedded into
  #   a column definition. ColConstraintElem specifies the embedded form.
  # - thomas 1997-12-03
  #
  TableConstraint <- (
    CONSTRAINT * name * ConstraintElem |
    ConstraintElem
  )

  ConstraintElem <- (
    CHECK * '(' * a_expr * ')' * ConstraintAttributeSpec |
    UNIQUE * '(' * columnList * ')' * opt_c_include * opt_definition * OptConsTableSpace *
                  ConstraintAttributeSpec            |
    UNIQUE * ExistingIndex * ConstraintAttributeSpec |
    PRIMARY * KEY * '(' * columnList * ')' * opt_c_include * opt_definition * OptConsTableSpace *
                  ConstraintAttributeSpec                   |
    PRIMARY * KEY * ExistingIndex * ConstraintAttributeSpec |
    EXCLUDE * access_method_clause * '(' * ExclusionConstraintList * ')' *
        opt_c_include * opt_definition * OptConsTableSpace * OptWhereClause *
                  ConstraintAttributeSpec |
    FOREIGN * KEY * '(' * columnList * ')' * REFERENCES * qualified_name *
        opt_column_list * key_match * key_actions * ConstraintAttributeSpec
  )

  opt_no_inherit <- Optional(
    NO * INHERIT
  )

  opt_column_list <- Optional(
      '(' * columnList * ')'
  )

  columnList <- (
    columnElem |
    columnList * ',' * columnElem
  )

  columnElem <- (
    ColId
  )

  opt_c_include <- Optional(
    INCLUDE * '(' * columnList * ')'
  )

  key_match <- Optional(
    MATCH * FULL  |
    MATCH * PARTIAL |
    MATCH * SIMPLE
  )

  ExclusionConstraintList <- (
    ExclusionConstraintElem |
    ExclusionConstraintList * ',' * ExclusionConstraintElem
  )

  ExclusionConstraintElem <- (
    index_elem * WITH * any_operator |

    # allow OPERATOR() decoration for the benefit of ruleutils.c */ 
    index_elem * WITH * OPERATOR * '(' * any_operator * ')'
  )

  OptWhereClause <- Optional(
    WHERE * '(' * a_expr * ')'
  )

  #
  # We combine the update and delete actions into one value temporarily
  # for simplicity of parsing, and then break them down again in the
  # calling production.  update is in the left 8 bits, delete in the right.
  # Note that NOACTION is the default.
  #
  key_actions <- Optional(
              key_update    |
    key_delete              |
    key_update * key_delete |
    key_delete * key_update
  )

  key_update <- (
    ON * UPDATE * key_action
  )

  key_delete <- (
    ON * DELETE_P * key_action
  )

  key_action <- (
    NO * ACTION |
    RESTRICT      |
    CASCADE       |
    SET * NULL_P  |
    SET * DEFAULT
  )

  OptInherit <- Optional(
    INHERITS * '(' * qualified_name_list * ')'
  )

  # Optional partition key specification */
  OptPartitionSpec <- Optional(
    PartitionSpec
  )

  PartitionSpec <- (
    PARTITION * BY * ColId * '(' * part_params * ')'
  )

  part_params <- (
    part_elem |
    part_params * ',' * part_elem
  )

  part_elem <- (
    ColId * opt_collate * opt_class              |
    func_expr_windowless * opt_collate * opt_class |
    '(' * a_expr * ')' * opt_collate * opt_class
  )

  table_access_method_clause <- Optional(
    USING * name
  )

  # WITHOUT OIDS is legacy only */
  OptWith <- Optional(
    WITH * reloptions |
    WITHOUT * OIDS
  )

  OnCommitOption <- Optional(
    ON * COMMIT * DROP          |
    ON * COMMIT * DELETE_P * ROWS |
    ON * COMMIT * PRESERVE * ROWS
  )

  OptTableSpace <- Optional(
    TABLESPACE * name
  )

  OptConsTableSpace <- Optional(
    USING * INDEX * TABLESPACE * name
  )

  ExistingIndex <- (
    USING * INDEX * name
  )

  #****************************************************************************
  #
  #       QUERY :
  #               CREATE STATISTICS [IF NOT EXISTS] stats_name [(stat types)]
  #                   ON expression-list FROM from_list
  #
  # Note: the expectation here is that the clauses after ON are a subset of
  # SELECT syntax, allowing for expressions and joined tables, and probably
  # someday a WHERE clause.  Much less than that is currently implemented,
  # but the grammar accepts it and then we'll throw FEATURE_NOT_SUPPORTED
  # errors as necessary at execution.
  #
  #****************************************************************************/

  CreateStatsStmt <- (
    CREATE * STATISTICS * any_name *
        opt_name_list * ON * stats_params * FROM * from_list |
    CREATE * STATISTICS * IF_P * NOT * EXISTS * any_name *
        opt_name_list * ON * stats_params * FROM * from_list

  )

  #
  # Statistics attributes can be either simple column references, or arbitrary
  # expressions in parens.  For compatibility with index attributes permitted
  # in CREATE INDEX, we allow an expression that's just a function call to be
  # written without parens.
  #

  stats_params <- (
    stats_param |
    stats_params * ',' * stats_param
  )

  stats_param <- (
    ColId              |
    func_expr_windowless |
    '(' * a_expr * ')'
  )

  #****************************************************************************
  #
  #       QUERY :
  #               ALTER STATISTICS [IF EXISTS] stats_name
  #                   SET STATISTICS  <SignedIconst>
  #
  #****************************************************************************/

  AlterStatsStmt <- (
    ALTER * STATISTICS * any_name * SET * STATISTICS * SignedIconst |
    ALTER * STATISTICS * IF_P * EXISTS * any_name * SET * STATISTICS * SignedIconst

  )

  #****************************************************************************
  #
  #       QUERY :
  #               CREATE TABLE relname AS SelectStmt [ WITH [NO] DATA ]
  #
  #
  # Note: SELECT ... INTO is a now-deprecated alternative for this.
  #
  #****************************************************************************/

  CreateAsStmt <- (
    CREATE * OptTemp * TABLE * create_as_target * AS * SelectStmt * opt_with_data |
    CREATE * OptTemp * TABLE * IF_P * NOT * EXISTS * create_as_target * AS * SelectStmt * opt_with_data
  )

  create_as_target <- (
    qualified_name * opt_column_list * table_access_method_clause *
        OptWith * OnCommitOption * OptTableSpace
  )

  opt_with_data <- Optional(
    WITH * DATA_P |
    WITH * NO * DATA_P
  )


  #****************************************************************************
  #
  #       QUERY :
  #               CREATE MATERIALIZED VIEW relname AS SelectStmt
  #
  #****************************************************************************/

  CreateMatViewStmt <- (
    CREATE * OptNoLog * MATERIALIZED * VIEW * create_mv_target * AS * SelectStmt * opt_with_data |
    CREATE * OptNoLog * MATERIALIZED * VIEW * IF_P * NOT * EXISTS * create_mv_target * AS * SelectStmt * opt_with_data
  )

  create_mv_target <- (
    qualified_name * opt_column_list * table_access_method_clause * opt_reloptions * OptTableSpace
  )

  OptNoLog <- Optional(
    UNLOGGED
  )


  #****************************************************************************
  #
  #       QUERY :
  #               REFRESH MATERIALIZED VIEW qualified_name
  #
  #****************************************************************************/

  RefreshMatViewStmt <- (
    REFRESH * MATERIALIZED * VIEW * opt_concurrently * qualified_name * opt_with_data
  )


  #****************************************************************************
  #
  #       QUERY :
  #               CREATE SEQUENCE seqname
  #               ALTER SEQUENCE seqname
  #
  #****************************************************************************/

  CreateSeqStmt <- (
    CREATE * OptTemp * SEQUENCE * qualified_name * OptSeqOptList |
    CREATE * OptTemp * SEQUENCE * IF_P * NOT * EXISTS * qualified_name * OptSeqOptList
  )

  AlterSeqStmt <- (
    ALTER * SEQUENCE * qualified_name * SeqOptList |
    ALTER * SEQUENCE * IF_P * EXISTS * qualified_name * SeqOptList

  )

  OptSeqOptList <- Optional(
    SeqOptList
  )

  OptParenthesizedSeqOptList <- Optional(
      '(' * SeqOptList * ')'
  )

  SeqOptList <- (
    SeqOptElem |
    SeqOptList * SeqOptElem
  )

  SeqOptElem <- (
    AS * SimpleTypename            |
    CACHE * NumericOnly              |
    CYCLE                            |
    NO * CYCLE                       |
    INCREMENT * opt_by * NumericOnly |
    MAXVALUE * NumericOnly           |
    MINVALUE * NumericOnly           |
    NO * MAXVALUE                    |
    NO * MINVALUE                    |
    OWNED * BY * any_name            |
    SEQUENCE * NAME_P * any_name     |
    START * opt_with * NumericOnly   |
    RESTART                          |
    RESTART * opt_with * NumericOnly
  )

  opt_by <- Optional(
    BY
  )

  NumericOnly <- (
    FCONST     |
    '+' * FCONST |
    '-' * FCONST |
    SignedIconst
  )

  NumericOnly_list <- (
    NumericOnly |
    NumericOnly_list * ',' * NumericOnly
  )

  #****************************************************************************
  #
  #       QUERIES :
  #               CREATE [OR REPLACE] [TRUSTED] [PROCEDURAL] LANGUAGE ...
  #               DROP [PROCEDURAL] LANGUAGE ...
  #
  #****************************************************************************/

  CreatePLangStmt <- (
    CREATE * opt_or_replace * opt_trusted * opt_procedural * LANGUAGE * name |
    CREATE * opt_or_replace * opt_trusted * opt_procedural * LANGUAGE * name *
        HANDLER * handler_name * opt_inline_handler * opt_validator
  )

  opt_trusted <- Optional(
    TRUSTED
  )

  # This ought to be just func_name, but that causes reduce/reduce conflicts
  # (CREATE LANGUAGE is the only place where func_name isn't followed by '(').
  # Work around by using simple names, instead.
  #
  handler_name <- (
    name |
    name * attrs
  )

  opt_inline_handler <- Optional(
    INLINE_P * handler_name
  )

  validator_clause <- (
    VALIDATOR * handler_name |
    NO * VALIDATOR
  )

  opt_validator <- Optional(
    validator_clause
  )

  opt_procedural <- Optional(
    PROCEDURAL
  )

  #****************************************************************************
  #
  #       QUERY:
  #             CREATE TABLESPACE tablespace LOCATION '/path/to/tablespace/'
  #
  #****************************************************************************/

  CreateTableSpaceStmt <- (
    CREATE * TABLESPACE * name * OptTableSpaceOwner * LOCATION * Sconst * opt_reloptions
  )

  OptTableSpaceOwner <- Optional(
    OWNER * RoleSpec
  )

  #****************************************************************************
  #
  #       QUERY :
  #               DROP TABLESPACE <tablespace>
  #
  #       No need for drop behaviour as we cannot implement dependencies for
  #       objects in other databases; we can only support RESTRICT.
  #
  #***************************************************************************/

  DropTableSpaceStmt <- (
    DROP * TABLESPACE * name |
    DROP * TABLESPACE * IF_P * EXISTS * name
  )

  #****************************************************************************
  #
  #       QUERY:
  #             CREATE EXTENSION extension
  #             [ WITH ] [ SCHEMA schema ] [ VERSION version ]
  #
  #****************************************************************************/

  CreateExtensionStmt <- (
    CREATE * EXTENSION * name * opt_with * create_extension_opt_list |
    CREATE * EXTENSION * IF_P * NOT * EXISTS * name * opt_with * create_extension_opt_list
  )

  create_extension_opt_list <- Optional(
    create_extension_opt_list * create_extension_opt_item
  )

  create_extension_opt_item <- (
    SCHEMA * name                       |
    VERSION_P * NonReservedWord_or_Sconst |
    FROM * NonReservedWord_or_Sconst      |
    CASCADE
  )

  #****************************************************************************
  #
  # ALTER EXTENSION name UPDATE [ TO version ]
  #
  #****************************************************************************/

  AlterExtensionStmt <- (
    ALTER * EXTENSION * name * UPDATE * alter_extension_opt_list
  )

  alter_extension_opt_list <- Optional(
    alter_extension_opt_list * alter_extension_opt_item
  )

  alter_extension_opt_item <- (
    TO * NonReservedWord_or_Sconst
  )

  #****************************************************************************
  #
  # ALTER EXTENSION name ADD/DROP object-identifier
  #
  #****************************************************************************/

  AlterExtensionContentsStmt <- (
    ALTER * EXTENSION * name * add_drop * object_type_name * name                    |
    ALTER * EXTENSION * name * add_drop * object_type_any_name * any_name              |
    ALTER * EXTENSION * name * add_drop * AGGREGATE * aggregate_with_argtypes          |
    ALTER * EXTENSION * name * add_drop * CAST * '(' * Typename * AS * Typename * ')'  |
    ALTER * EXTENSION * name * add_drop * DOMAIN_P * Typename                          |
    ALTER * EXTENSION * name * add_drop * FUNCTION * function_with_argtypes            |
    ALTER * EXTENSION * name * add_drop * OPERATOR * operator_with_argtypes            |
    ALTER * EXTENSION * name * add_drop * OPERATOR * CLASS * any_name * USING * name   |
    ALTER * EXTENSION * name * add_drop * OPERATOR * FAMILY * any_name * USING * name  |
    ALTER * EXTENSION * name * add_drop * PROCEDURE * function_with_argtypes           |
    ALTER * EXTENSION * name * add_drop * ROUTINE * function_with_argtypes             |
    ALTER * EXTENSION * name * add_drop * TRANSFORM * FOR * Typename * LANGUAGE * name |
    ALTER * EXTENSION * name * add_drop * TYPE_P * Typename
  )

  #****************************************************************************
  #
  #       QUERY:
  #             CREATE FOREIGN DATA WRAPPER name options
  #
  #****************************************************************************/

  CreateFdwStmt <- (
    CREATE * FOREIGN * DATA_P * WRAPPER * name * opt_fdw_options * create_generic_options
  )

  fdw_option <- (
    HANDLER * handler_name |
    NO * HANDLER             |
    VALIDATOR * handler_name |
    NO * VALIDATOR
  )

  fdw_options <- (
    fdw_option |
    fdw_options * fdw_option
  )

  opt_fdw_options <- Optional(
    fdw_options
  )

  #****************************************************************************
  #
  #       QUERY :
  #               ALTER FOREIGN DATA WRAPPER name options
  #
  #***************************************************************************/

  AlterFdwStmt <- (
    ALTER * FOREIGN * DATA_P * WRAPPER * name * opt_fdw_options * alter_generic_options |
    ALTER * FOREIGN * DATA_P * WRAPPER * name * fdw_options
  )

  # Options definition for CREATE FDW, SERVER and USER MAPPING */
  create_generic_options <- Optional(
    OPTIONS * '(' * generic_option_list * ')'
  )

  generic_option_list <- (
    generic_option_elem |
    generic_option_list * ',' * generic_option_elem
  )

  # Options definition for ALTER FDW, SERVER and USER MAPPING */
  alter_generic_options <- (
    OPTIONS * '(' * alter_generic_option_list * ')'
  )

  alter_generic_option_list <- (
    alter_generic_option_elem |
    alter_generic_option_list * ',' * alter_generic_option_elem
  )

  alter_generic_option_elem <- (
    generic_option_elem       |
    SET * generic_option_elem   |
    ADD_P * generic_option_elem |
    DROP * generic_option_name
  )

  generic_option_elem <- (
    generic_option_name * generic_option_arg
  )

  generic_option_name <- (
    ColLabel
  )

  # We could use def_arg here, but the spec only requires string literals */
  generic_option_arg <- (
    Sconst
  )

  #****************************************************************************
  #
  #       QUERY:
  #             CREATE SERVER name [TYPE] [VERSION] [OPTIONS]
  #
  #****************************************************************************/

  CreateForeignServerStmt <- (
    CREATE * SERVER * name * opt_type * opt_foreign_server_version *
        FOREIGN * DATA_P * WRAPPER * name * create_generic_options |
    CREATE * SERVER * IF_P * NOT * EXISTS * name * opt_type * opt_foreign_server_version *
        FOREIGN * DATA_P * WRAPPER * name * create_generic_options
  )

  opt_type <- Optional(
    TYPE_P * Sconst
  )


  foreign_server_version <- (
    VERSION_P * Sconst |
    VERSION_P * NULL_P
  )

  opt_foreign_server_version <- Optional(
    foreign_server_version
  )

  #****************************************************************************
  #
  #       QUERY :
  #               ALTER SERVER name [VERSION] [OPTIONS]
  #
  #***************************************************************************/

  AlterForeignServerStmt <- (
    ALTER * SERVER * name * foreign_server_version * alter_generic_options |
    ALTER * SERVER * name * foreign_server_version                           |
    ALTER * SERVER * name * alter_generic_options
  )

  #****************************************************************************
  #
  #       QUERY:
  #             CREATE FOREIGN TABLE relname (...) SERVER name (...)
  #
  #****************************************************************************/

  CreateForeignTableStmt <- (
    CREATE * FOREIGN * TABLE * qualified_name *
        '(' * OptTableElementList * ')' *
        OptInherit * SERVER * name * create_generic_options |
    CREATE * FOREIGN * TABLE * IF_P * NOT * EXISTS * qualified_name *
        '(' * OptTableElementList * ')' *
        OptInherit * SERVER * name * create_generic_options |
    CREATE * FOREIGN * TABLE * qualified_name *
        PARTITION * OF * qualified_name * OptTypedTableElementList * PartitionBoundSpec *
        SERVER * name * create_generic_options |
    CREATE * FOREIGN * TABLE * IF_P * NOT * EXISTS * qualified_name *
        PARTITION * OF * qualified_name * OptTypedTableElementList * PartitionBoundSpec *
        SERVER * name * create_generic_options
  )

  #****************************************************************************
  #
  #       QUERY:
  #               IMPORT FOREIGN SCHEMA remote_schema
  #               [ { LIMIT TO | EXCEPT } ( table_list ) ]
  #               FROM SERVER server_name INTO local_schema [ OPTIONS (...) ]
  #
  #***************************************************************************/

  ImportForeignSchemaStmt <- (
    IMPORT_P * FOREIGN * SCHEMA * name * import_qualification *
        FROM * SERVER * name * INTO * name * create_generic_options
  )

  import_qualification_type <- (
    LIMIT * TO |
    EXCEPT
  )

  import_qualification <- Optional(
    import_qualification_type * '(' * relation_expr_list * ')'
  )

  #****************************************************************************
  #
  #       QUERY:
  #             CREATE USER MAPPING FOR auth_ident SERVER name [OPTIONS]
  #
  #****************************************************************************/

  CreateUserMappingStmt <- (
    CREATE * USER * MAPPING * FOR * auth_ident * SERVER * name * create_generic_options |
    CREATE * USER * MAPPING * IF_P * NOT * EXISTS * FOR * auth_ident * SERVER * name * create_generic_options
  )

  # User mapping authorization identifier */
  auth_ident <- (
    RoleSpec |
    USER
  )

  #****************************************************************************
  #
  #       QUERY :
  #               DROP USER MAPPING FOR auth_ident SERVER name
  #
  # XXX you'd think this should have a CASCADE/RESTRICT option, even if it's
  # only pro forma; but the SQL standard doesn't show one.
  #***************************************************************************/

  DropUserMappingStmt <- (
    DROP * USER * MAPPING * FOR * auth_ident * SERVER * name |
    DROP * USER * MAPPING * IF_P * EXISTS * FOR * auth_ident * SERVER * name
  )

  #****************************************************************************
  #
  #       QUERY :
  #               ALTER USER MAPPING FOR auth_ident SERVER name OPTIONS
  #
  #***************************************************************************/

  AlterUserMappingStmt <- (
    ALTER * USER * MAPPING * FOR * auth_ident * SERVER * name * alter_generic_options
  )

  #****************************************************************************
  #
  #       QUERIES:
  #               CREATE POLICY name ON table
  #                   [AS { PERMISSIVE | RESTRICTIVE } ]
  #                   [FOR { SELECT | INSERT | UPDATE | DELETE } ]
  #                   [TO role, ...]
  #                   [USING (qual)] [WITH CHECK (with check qual)]
  #               ALTER POLICY name ON table [TO role, ...]
  #                   [USING (qual)] [WITH CHECK (with check qual)]
  #
  #****************************************************************************/

  CreatePolicyStmt <- (
    CREATE * POLICY * name * ON * qualified_name * RowSecurityDefaultPermissive *
        RowSecurityDefaultForCmd * RowSecurityDefaultToRole *
        RowSecurityOptionalExpr * RowSecurityOptionalWithCheck
  )

  AlterPolicyStmt <- (
    ALTER * POLICY * name * ON * qualified_name * RowSecurityOptionalToRole *
        RowSecurityOptionalExpr * RowSecurityOptionalWithCheck
  )

  RowSecurityOptionalExpr <- Optional(
    USING * '(' * a_expr * ')'
  )

  RowSecurityOptionalWithCheck <- Optional(
    WITH * CHECK * '(' * a_expr * ')'
  )

  RowSecurityDefaultToRole <- Optional(
    TO * role_list
  )

  RowSecurityOptionalToRole <- Optional(
    TO * role_list
  )

  RowSecurityDefaultPermissive <- Optional(
    AS * IDENT
  )

  RowSecurityDefaultForCmd <- Optional(
    FOR * row_security_cmd
  )

  row_security_cmd <- (
    ALL  |
    SELECT |
    INSERT |
    UPDATE |
    DELETE_P
  )

  #****************************************************************************
  #
  #       QUERY:
  #             CREATE ACCESS METHOD name HANDLER handler_name
  #
  #****************************************************************************/

  CreateAmStmt <- (
    CREATE * ACCESS * METHOD * name * TYPE_P * am_type * HANDLER * handler_name
  )

  am_type <- (
    INDEX |
    TABLE
  )

  #****************************************************************************
  #
  #       QUERIES :
  #               CREATE TRIGGER ...
  #
  #****************************************************************************/

  CreateTrigStmt <- (
    CREATE * opt_or_replace * TRIGGER * name * TriggerActionTime * TriggerEvents * ON *
        qualified_name * TriggerReferencing * TriggerForSpec * TriggerWhen *
        EXECUTE * FUNCTION_or_PROCEDURE * func_name * '(' * TriggerFuncArgs * ')' |
    CREATE * opt_or_replace * CONSTRAINT * TRIGGER * name * AFTER * TriggerEvents * ON *
        qualified_name * OptConstrFromTable * ConstraintAttributeSpec *
        FOR * EACH * ROW * TriggerWhen *
        EXECUTE * FUNCTION_or_PROCEDURE * func_name * '(' * TriggerFuncArgs * ')'
  )

  TriggerActionTime <- (
    BEFORE |
    AFTER    |
    INSTEAD * OF
  )

  TriggerEvents <- (
    TriggerOneEvent |
    TriggerEvents * OR * TriggerOneEvent
  )

  TriggerOneEvent <- (
    INSERT                 |
    DELETE_P                 |
    UPDATE                   |
    UPDATE * OF * columnList |
    TRUNCATE
  )

  TriggerReferencing <- Optional(
    REFERENCING * TriggerTransitions
  )

  TriggerTransitions <- (
    TriggerTransition |
    TriggerTransitions * TriggerTransition
  )

  TriggerTransition <- (
    TransitionOldOrNew * TransitionRowOrTable * opt_as * TransitionRelName
  )

  TransitionOldOrNew <- (
    NEW |
    OLD
  )

  TransitionRowOrTable <- (
    TABLE |
    # 
    # According to the standard, lack of a keyword here implies ROW. 
    # Support for that would require prohibiting ROW entirely here, 
    # reserving the keyword ROW, and/or requiring AS (instead of 
    # allowing it to be optional, as the standard specifies) as the 
    # next token.  Requiring ROW seems cleanest and easiest to 
    # explain. 
    # 
    ROW
  )

  TransitionRelName <- (
    ColId
  )

  TriggerForSpec <- Optional(
    FOR * TriggerForOptEach * TriggerForType
  )

  TriggerForOptEach <- Optional(
    EACH
  )

  TriggerForType <- (
    ROW |
    STATEMENT
  )

  TriggerWhen <- Optional(
    WHEN * '(' * a_expr * ')'
  )

  FUNCTION_or_PROCEDURE <- (
    FUNCTION |
    PROCEDURE
  )

  TriggerFuncArgs <- Optional(
    TriggerFuncArg |
    TriggerFuncArgs * ',' * TriggerFuncArg
  )

  TriggerFuncArg <- (
    Iconst |
    FCONST   |
    Sconst   |
    ColLabel
  )

  OptConstrFromTable <- Optional(
    FROM * qualified_name
  )

  ConstraintAttributeSpec <- Optional(
    ConstraintAttributeSpec * ConstraintAttributeElem
  )

  ConstraintAttributeElem <- (
    NOT * DEFERRABLE    |
    DEFERRABLE            |
    INITIALLY * IMMEDIATE |
    INITIALLY * DEFERRED  |
    NOT * VALID           |
    NO * INHERIT
  )


  #****************************************************************************
  #
  #       QUERIES :
  #               CREATE EVENT TRIGGER ...
  #               ALTER EVENT TRIGGER ...
  #
  #****************************************************************************/

  CreateEventTrigStmt <- (
    CREATE * EVENT * TRIGGER * name * ON * ColLabel *
        EXECUTE * FUNCTION_or_PROCEDURE * func_name * '(' * ')' |
    CREATE * EVENT * TRIGGER * name * ON * ColLabel *
        WHEN * event_trigger_when_list *
        EXECUTE * FUNCTION_or_PROCEDURE * func_name * '(' * ')'
  )

  event_trigger_when_list <- (
    event_trigger_when_item |
    event_trigger_when_list * AND * event_trigger_when_item
  )

  event_trigger_when_item <- (
    ColId * IN_P * '(' * event_trigger_value_list * ')'
  )

  event_trigger_value_list <- (
    SCONST |
    event_trigger_value_list * ',' * SCONST
  )

  AlterEventTrigStmt <- (
    ALTER * EVENT * TRIGGER * name * enable_trigger
  )

  enable_trigger <- (
    ENABLE_P         |
    ENABLE_P * REPLICA |
    ENABLE_P * ALWAYS  |
    DISABLE_P
  )

  #****************************************************************************
  #
  #       QUERY :
  #               CREATE ASSERTION ...
  #
  #****************************************************************************/

  CreateAssertionStmt <- (
    CREATE * ASSERTION * any_name * CHECK * '(' * a_expr * ')' * ConstraintAttributeSpec
  )


  #****************************************************************************
  #
  #       QUERY :
  #               define (aggregate,operator,type)
  #
  #****************************************************************************/

  DefineStmt <- (
    CREATE * opt_or_replace * AGGREGATE * func_name * aggr_args * definition |
    CREATE * opt_or_replace * AGGREGATE * func_name * old_aggr_definition      |
    CREATE * OPERATOR * any_operator * definition                              |
    CREATE * TYPE_P * any_name * definition                                    |
    CREATE * TYPE_P * any_name                                                 |
    CREATE * TYPE_P * any_name * AS * '(' * OptTableFuncElementList * ')'      |
    CREATE * TYPE_P * any_name * AS * ENUM_P * '(' * opt_enum_val_list * ')'   |
    CREATE * TYPE_P * any_name * AS * RANGE * definition                       |
    CREATE * TEXT_P * SEARCH * PARSER * any_name * definition                  |
    CREATE * TEXT_P * SEARCH * DICTIONARY * any_name * definition              |
    CREATE * TEXT_P * SEARCH * TEMPLATE * any_name * definition                |
    CREATE * TEXT_P * SEARCH * CONFIGURATION * any_name * definition           |
    CREATE * COLLATION * any_name * definition                                 |
    CREATE * COLLATION * IF_P * NOT * EXISTS * any_name * definition           |
    CREATE * COLLATION * any_name * FROM * any_name                            |
    CREATE * COLLATION * IF_P * NOT * EXISTS * any_name * FROM * any_name
  )

  definition <- (
      '(' * def_list * ')'
  )

  def_list <- (
    def_elem |
    def_list * ',' * def_elem
  )

  def_elem <- (
    ColLabel * '=' * def_arg |
    ColLabel
  )

  # Note: any simple identifier will be returned as a type name! */
  def_arg <- (
    func_type      |
    reserved_keyword |
    qual_all_Op      |
    NumericOnly      |
    Sconst           |
    NONE
  )

  old_aggr_definition <- (
      '(' * old_aggr_list * ')'
  )

  old_aggr_list <- (
    old_aggr_elem |
    old_aggr_list * ',' * old_aggr_elem
  )

  #
  # Must use IDENT here to avoid reduce/reduce conflicts; fortunately none of
  # the item names needed in old aggregate definitions are likely to become
  # SQL keywords.
  #
  old_aggr_elem <- (
    IDENT * '=' * def_arg
  )

  opt_enum_val_list <- Optional(
    enum_val_list
  )

  enum_val_list <- (
    Sconst |
    enum_val_list * ',' * Sconst
  )

  #****************************************************************************
  #
  #   ALTER TYPE enumtype ADD ...
  #
  #****************************************************************************/

  AlterEnumStmt <- (
    ALTER * TYPE_P * any_name * ADD_P * VALUE_P * opt_if_not_exists * Sconst                 |
    ALTER * TYPE_P * any_name * ADD_P * VALUE_P * opt_if_not_exists * Sconst * BEFORE * Sconst |
    ALTER * TYPE_P * any_name * ADD_P * VALUE_P * opt_if_not_exists * Sconst * AFTER * Sconst  |
    ALTER * TYPE_P * any_name * RENAME * VALUE_P * Sconst * TO * Sconst

  )

  opt_if_not_exists <- Optional(
    IF_P * NOT * EXISTS
  )


  #****************************************************************************
  #
  #       QUERIES :
  #               CREATE OPERATOR CLASS ...
  #               CREATE OPERATOR FAMILY ...
  #               ALTER OPERATOR FAMILY ...
  #               DROP OPERATOR CLASS ...
  #               DROP OPERATOR FAMILY ...
  #
  #****************************************************************************/

  CreateOpClassStmt <- (
    CREATE * OPERATOR * CLASS * any_name * opt_default * FOR * TYPE_P * Typename *
        USING * name * opt_opfamily * AS * opclass_item_list
  )

  opclass_item_list <- (
    opclass_item |
    opclass_item_list * ',' * opclass_item
  )

  opclass_item <- (
    OPERATOR * Iconst * any_operator * opclass_purpose * opt_recheck |
    OPERATOR * Iconst * operator_with_argtypes * opclass_purpose *
        opt_recheck                                                    |
    FUNCTION * Iconst * function_with_argtypes                         |
    FUNCTION * Iconst * '(' * type_list * ')' * function_with_argtypes |
    STORAGE * Typename
  )

  opt_default <- Optional(
    DEFAULT
  )

  opt_opfamily <- Optional(
    FAMILY * any_name
  )

  opclass_purpose <- Optional(
    FOR * SEARCH |
    FOR * ORDER * BY * any_name
  )

  opt_recheck <- Optional(
    RECHECK
  )


  CreateOpFamilyStmt <- (
    CREATE * OPERATOR * FAMILY * any_name * USING * name
  )

  AlterOpFamilyStmt <- (
    ALTER * OPERATOR * FAMILY * any_name * USING * name * ADD_P * opclass_item_list |
    ALTER * OPERATOR * FAMILY * any_name * USING * name * DROP * opclass_drop_list
  )

  opclass_drop_list <- (
    opclass_drop |
    opclass_drop_list * ',' * opclass_drop
  )

  opclass_drop <- (
    OPERATOR * Iconst * '(' * type_list * ')' |
    FUNCTION * Iconst * '(' * type_list * ')'
  )


  DropOpClassStmt <- (
    DROP * OPERATOR * CLASS * any_name * USING * name * opt_drop_behavior |
    DROP * OPERATOR * CLASS * IF_P * EXISTS * any_name * USING * name * opt_drop_behavior
  )

  DropOpFamilyStmt <- (
    DROP * OPERATOR * FAMILY * any_name * USING * name * opt_drop_behavior |
    DROP * OPERATOR * FAMILY * IF_P * EXISTS * any_name * USING * name * opt_drop_behavior
  )


  #****************************************************************************
  #
  #       QUERY:
  #
  #       DROP OWNED BY username [, username ...] [ RESTRICT | CASCADE ]
  #       REASSIGN OWNED BY username [, username ...] TO username
  #
  #****************************************************************************/
  DropOwnedStmt <- (
    DROP * OWNED * BY * role_list * opt_drop_behavior
  )

  ReassignOwnedStmt <- (
    REASSIGN * OWNED * BY * role_list * TO * RoleSpec
  )

  #****************************************************************************
  #
  #       QUERY:
  #
  #       DROP itemtype [ IF EXISTS ] itemname [, itemname ...]
  #           [ RESTRICT | CASCADE ]
  #
  #****************************************************************************/

  DropStmt <- (
    DROP * object_type_any_name * IF_P * EXISTS * any_name_list * opt_drop_behavior              |
    DROP * object_type_any_name * any_name_list * opt_drop_behavior                                |
    DROP * drop_type_name * IF_P * EXISTS * name_list * opt_drop_behavior                          |
    DROP * drop_type_name * name_list * opt_drop_behavior                                          |
    DROP * object_type_name_on_any_name * name * ON * any_name * opt_drop_behavior                 |
    DROP * object_type_name_on_any_name * IF_P * EXISTS * name * ON * any_name * opt_drop_behavior |
    DROP * TYPE_P * type_name_list * opt_drop_behavior                                             |
    DROP * TYPE_P * IF_P * EXISTS * type_name_list * opt_drop_behavior                             |
    DROP * DOMAIN_P * type_name_list * opt_drop_behavior                                           |
    DROP * DOMAIN_P * IF_P * EXISTS * type_name_list * opt_drop_behavior                           |
    DROP * INDEX * CONCURRENTLY * any_name_list * opt_drop_behavior                                |
    DROP * INDEX * CONCURRENTLY * IF_P * EXISTS * any_name_list * opt_drop_behavior
  )

  # object types taking any_name/any_name_list */
  object_type_any_name <- (
    TABLE                      |
    SEQUENCE                     |
    VIEW                         |
    MATERIALIZED * VIEW          |
    INDEX                        |
    FOREIGN * TABLE              |
    COLLATION                    |
    CONVERSION_P                 |
    STATISTICS                   |
    TEXT_P * SEARCH * PARSER     |
    TEXT_P * SEARCH * DICTIONARY |
    TEXT_P * SEARCH * TEMPLATE   |
    TEXT_P * SEARCH * CONFIGURATION
  )

  #
  # object types taking name/name_list
  #
  # DROP handles some of them separately
  #

  object_type_name <- (
    drop_type_name |
    DATABASE         |
    ROLE             |
    SUBSCRIPTION     |
    TABLESPACE
  )

  drop_type_name <- (
    ACCESS * METHOD          |
    EVENT * TRIGGER            |
    EXTENSION                  |
    FOREIGN * DATA_P * WRAPPER |
    opt_procedural * LANGUAGE  |
    PUBLICATION                |
    SCHEMA                     |
    SERVER
  )

  # object types attached to a table */
  object_type_name_on_any_name <- (
    POLICY |
    RULE     |
    TRIGGER
  )

  any_name_list <- (
    any_name |
    any_name_list * ',' * any_name
  )

  any_name <- (
    ColId |
    ColId * attrs
  )

  attrs <- (
      '.' * attr_name |
    attrs * '.' * attr_name
  )

  type_name_list <- (
    Typename |
    type_name_list * ',' * Typename
  )

  #****************************************************************************
  #
  #       QUERY:
  #               truncate table relname1, relname2, ...
  #
  #****************************************************************************/

  TruncateStmt <- (
    TRUNCATE * opt_table * relation_expr_list * opt_restart_seqs * opt_drop_behavior
  )

  opt_restart_seqs <- Optional(
    CONTINUE_P * IDENTITY_P |
    RESTART * IDENTITY_P
  )

  #****************************************************************************
  #
  # COMMENT ON <object> IS <text>
  #
  #****************************************************************************/

  CommentStmt <- (
    COMMENT * ON * object_type_any_name * any_name * IS * comment_text                   |
    COMMENT * ON * COLUMN * any_name * IS * comment_text                                   |
    COMMENT * ON * object_type_name * name * IS * comment_text                             |
    COMMENT * ON * TYPE_P * Typename * IS * comment_text                                   |
    COMMENT * ON * DOMAIN_P * Typename * IS * comment_text                                 |
    COMMENT * ON * AGGREGATE * aggregate_with_argtypes * IS * comment_text                 |
    COMMENT * ON * FUNCTION * function_with_argtypes * IS * comment_text                   |
    COMMENT * ON * OPERATOR * operator_with_argtypes * IS * comment_text                   |
    COMMENT * ON * CONSTRAINT * name * ON * any_name * IS * comment_text                   |
    COMMENT * ON * CONSTRAINT * name * ON * DOMAIN_P * any_name * IS * comment_text        |
    COMMENT * ON * object_type_name_on_any_name * name * ON * any_name * IS * comment_text |
    COMMENT * ON * PROCEDURE * function_with_argtypes * IS * comment_text                  |
    COMMENT * ON * ROUTINE * function_with_argtypes * IS * comment_text                    |
    COMMENT * ON * TRANSFORM * FOR * Typename * LANGUAGE * name * IS * comment_text        |
    COMMENT * ON * OPERATOR * CLASS * any_name * USING * name * IS * comment_text          |
    COMMENT * ON * OPERATOR * FAMILY * any_name * USING * name * IS * comment_text         |
    COMMENT * ON * LARGE_P * OBJECT_P * NumericOnly * IS * comment_text                    |
    COMMENT * ON * CAST * '(' * Typename * AS * Typename * ')' * IS * comment_text
  )

  comment_text <- (
    Sconst |
    NULL_P
  )


  #****************************************************************************
  #
  #  SECURITY LABEL [FOR <provider>] ON <object> IS <label>
  #
  #  As with COMMENT ON, <object> can refer to various types of database
  #  objects (e.g. TABLE, COLUMN, etc.).
  #
  #****************************************************************************/

  SecLabelStmt <- (
    SECURITY * LABEL * opt_provider * ON * object_type_any_name * any_name *
        IS * security_label |
    SECURITY * LABEL * opt_provider * ON * COLUMN * any_name *
        IS * security_label |
    SECURITY * LABEL * opt_provider * ON * object_type_name * name *
        IS * security_label |
    SECURITY * LABEL * opt_provider * ON * TYPE_P * Typename *
        IS * security_label |
    SECURITY * LABEL * opt_provider * ON * DOMAIN_P * Typename *
        IS * security_label |
    SECURITY * LABEL * opt_provider * ON * AGGREGATE * aggregate_with_argtypes *
        IS * security_label |
    SECURITY * LABEL * opt_provider * ON * FUNCTION * function_with_argtypes *
        IS * security_label |
    SECURITY * LABEL * opt_provider * ON * LARGE_P * OBJECT_P * NumericOnly *
        IS * security_label |
    SECURITY * LABEL * opt_provider * ON * PROCEDURE * function_with_argtypes *
        IS * security_label |
    SECURITY * LABEL * opt_provider * ON * ROUTINE * function_with_argtypes *
        IS * security_label
  )

  opt_provider <- Optional(
    FOR * NonReservedWord_or_Sconst
  )

  security_label <- (
    Sconst |
    NULL_P
  )

  #****************************************************************************
  #
  #       QUERY:
  #           fetch/move
  #
  #****************************************************************************/

  FetchStmt <- (
    FETCH * fetch_args |
    MOVE * fetch_args
  )

  fetch_args <- (
    cursor_name                                           |
    from_in * cursor_name                                 |
    NEXT * opt_from_in * cursor_name                      |
    PRIOR * opt_from_in * cursor_name                     |
    FIRST_P * opt_from_in * cursor_name                   |
    LAST_P * opt_from_in * cursor_name                    |
    ABSOLUTE_P * SignedIconst * opt_from_in * cursor_name |
    RELATIVE_P * SignedIconst * opt_from_in * cursor_name |
    SignedIconst * opt_from_in * cursor_name              |
    ALL * opt_from_in * cursor_name                       |
    FORWARD * opt_from_in * cursor_name                   |
    FORWARD * SignedIconst * opt_from_in * cursor_name    |
    FORWARD * ALL * opt_from_in * cursor_name             |
    BACKWARD * opt_from_in * cursor_name                  |
    BACKWARD * SignedIconst * opt_from_in * cursor_name   |
    BACKWARD * ALL * opt_from_in * cursor_name
  )

  from_in <- (
    FROM |
    IN_P
  )

  opt_from_in <- Optional(
    from_in
  )


  #****************************************************************************
  #
  # GRANT and REVOKE statements
  #
  #****************************************************************************/

  GrantStmt <- (
    GRANT * privileges * ON * privilege_target * TO * grantee_list *
        opt_grant_grant_option * opt_granted_by
  )

  RevokeStmt <- (
    REVOKE * privileges * ON * privilege_target *
        FROM * grantee_list * opt_granted_by * opt_drop_behavior |
    REVOKE * GRANT * OPTION * FOR * privileges * ON * privilege_target *
        FROM * grantee_list * opt_granted_by * opt_drop_behavior
  )


  #
  # Privilege names are represented as strings; the validity of the privilege
  # names gets checked at execution.  This is a bit annoying but we have little
  # choice because of the syntactic conflict with lists of role names in
  # GRANT/REVOKE.  What's more, we have to call out in the "privilege"
  # production any reserved keywords that need to be usable as privilege names.
  #

  # either ALL [PRIVILEGES] or a list of individual privileges */
  privileges <- (
    privilege_list               |
    ALL                          |
    ALL * PRIVILEGES             |
    ALL * '(' * columnList * ')' |
    ALL * PRIVILEGES * '(' * columnList * ')'
  )

  privilege_list <- (
    privilege |
    privilege_list * ',' * privilege
  )

  privilege <- (
    SELECT * opt_column_list   |
    REFERENCES * opt_column_list |
    CREATE * opt_column_list     |
    ColId * opt_column_list
  )


  # Don't bother trying to fold the first two rules into one using
  # opt_table.  You're going to get conflicts.
  #
  privilege_target <- (
    qualified_name_list                        |
    TABLE * qualified_name_list                  |
    SEQUENCE * qualified_name_list               |
    FOREIGN * DATA_P * WRAPPER * name_list       |
    FOREIGN * SERVER * name_list                 |
    FUNCTION * function_with_argtypes_list       |
    PROCEDURE * function_with_argtypes_list      |
    ROUTINE * function_with_argtypes_list        |
    DATABASE * name_list                         |
    DOMAIN_P * any_name_list                     |
    LANGUAGE * name_list                         |
    LARGE_P * OBJECT_P * NumericOnly_list        |
    SCHEMA * name_list                           |
    TABLESPACE * name_list                       |
    TYPE_P * any_name_list                       |
    ALL * TABLES * IN_P * SCHEMA * name_list     |
    ALL * SEQUENCES * IN_P * SCHEMA * name_list  |
    ALL * FUNCTIONS * IN_P * SCHEMA * name_list  |
    ALL * PROCEDURES * IN_P * SCHEMA * name_list |
    ALL * ROUTINES * IN_P * SCHEMA * name_list
  )


  grantee_list <- (
    grantee |
    grantee_list * ',' * grantee
  )

  grantee <- (
    RoleSpec |
    GROUP_P * RoleSpec
  )


  opt_grant_grant_option <- Optional(
    WITH * GRANT * OPTION
  )

  #****************************************************************************
  #
  # GRANT and REVOKE ROLE statements
  #
  #****************************************************************************/

  GrantRoleStmt <- (
    GRANT * privilege_list * TO * role_list * opt_grant_admin_option * opt_granted_by
  )

  RevokeRoleStmt <- (
    REVOKE * privilege_list * FROM * role_list * opt_granted_by * opt_drop_behavior |
    REVOKE * ADMIN * OPTION * FOR * privilege_list * FROM * role_list * opt_granted_by * opt_drop_behavior
  )

  opt_grant_admin_option <- Optional(
    WITH * ADMIN * OPTION
  )

  opt_granted_by <- Optional(
    GRANTED * BY * RoleSpec
  )

  #****************************************************************************
  #
  # ALTER DEFAULT PRIVILEGES statement
  #
  #****************************************************************************/

  AlterDefaultPrivilegesStmt <- (
    ALTER * DEFAULT * PRIVILEGES * DefACLOptionList * DefACLAction
  )

  DefACLOptionList <- Optional(
    DefACLOptionList * DefACLOption
  )

  DefACLOption <- (
    IN_P * SCHEMA * name_list |
    FOR * ROLE * role_list      |
    FOR * USER * role_list
  )

  #
  # This should match GRANT/REVOKE, except that individual target objects
  # are not mentioned and we only allow a subset of object types.
  #
  DefACLAction <- (
    GRANT * privileges * ON * defacl_privilege_target * TO * grantee_list *
        opt_grant_grant_option |
    REVOKE * privileges * ON * defacl_privilege_target *
        FROM * grantee_list * opt_drop_behavior |
    REVOKE * GRANT * OPTION * FOR * privileges * ON * defacl_privilege_target *
        FROM * grantee_list * opt_drop_behavior
  )

  defacl_privilege_target <- (
    TABLES  |
    FUNCTIONS |
    ROUTINES  |
    SEQUENCES |
    TYPES_P   |
    SCHEMAS
  )


  #****************************************************************************
  #
  #       QUERY: CREATE INDEX
  #
  # Note: we cannot put TABLESPACE clause after WHERE clause unless we are
  # willing to make TABLESPACE a fully reserved word.
  #****************************************************************************/

  IndexStmt <- (
    CREATE * opt_unique * INDEX * opt_concurrently * opt_index_name *
        ON * relation_expr * access_method_clause * '(' * index_params * ')' *
        opt_include * opt_reloptions * OptTableSpace * where_clause |
    CREATE * opt_unique * INDEX * opt_concurrently * IF_P * NOT * EXISTS * name *
        ON * relation_expr * access_method_clause * '(' * index_params * ')' *
        opt_include * opt_reloptions * OptTableSpace * where_clause
  )

  opt_unique <- Optional(
    UNIQUE
  )

  opt_concurrently <- Optional(
    CONCURRENTLY
  )

  opt_index_name <- Optional(
    name
  )

  access_method_clause <- Optional(
    USING * name
  )

  index_params <- (
    index_elem |
    index_params * ',' * index_elem
  )


  index_elem_options <- (
    opt_collate * opt_class * opt_asc_desc * opt_nulls_order |
    opt_collate * any_name * reloptions * opt_asc_desc * opt_nulls_order

  )

  #
  # Index attributes can be either simple column references, or arbitrary
  # expressions in parens.  For backwards-compatibility reasons, we allow
  # an expression that's just a function call to be written without parens.
  #
  index_elem <- (
    ColId * index_elem_options              |
    func_expr_windowless * index_elem_options |
    '(' * a_expr * ')' * index_elem_options
  )

  opt_include <- Optional(
    INCLUDE * '(' * index_including_params * ')'
  )

  index_including_params <- (
    index_elem |
    index_including_params * ',' * index_elem
  )

  opt_collate <- Optional(
    COLLATE * any_name
  )

  opt_class <- Optional(
    any_name
  )

  opt_asc_desc <- Optional(
    ASC |
    DESC
  )

  opt_nulls_order <- Optional(
    NULLS_LA * FIRST_P |
    NULLS_LA * LAST_P
  )


  #****************************************************************************
  #
  #       QUERY:
  #               create [or replace] function <fname>
  #                       [(<type-1> { , <type-n>})]
  #                       returns <type-r>
  #                       as <filename or code in language as appropriate>
  #                       language <lang> [with parameters]
  #
  #****************************************************************************/

  CreateFunctionStmt <- (
    CREATE * opt_or_replace * FUNCTION * func_name * func_args_with_defaults *
        RETURNS * func_return * opt_createfunc_opt_list * opt_routine_body |
    CREATE * opt_or_replace * FUNCTION * func_name * func_args_with_defaults *
        RETURNS * TABLE * '(' * table_func_column_list * ')' * opt_createfunc_opt_list * opt_routine_body |
    CREATE * opt_or_replace * FUNCTION * func_name * func_args_with_defaults *
        opt_createfunc_opt_list * opt_routine_body |
    CREATE * opt_or_replace * PROCEDURE * func_name * func_args_with_defaults *
        opt_createfunc_opt_list * opt_routine_body
  )

  opt_or_replace <- Optional(
    OR * REPLACE
  )

  func_args <- (
      '(' * func_args_list * ')' |
    '(' * ')'
  )

  func_args_list <- (
    func_arg |
    func_args_list * ',' * func_arg
  )

  function_with_argtypes_list <- (
    function_with_argtypes |
    function_with_argtypes_list * ',' * function_with_argtypes
  )

  function_with_argtypes <- (
    func_name * func_args |

    # 
    # Because of reduce/reduce conflicts, we can't use func_name 
    # below, but we can write it out the long way, which actually 
    # allows more cases. 
    # 
    type_func_name_keyword |
    ColId                  |
    ColId * indirection
  )

  #
  # func_args_with_defaults is separate because we only want to accept
  # defaults in CREATE FUNCTION, not in ALTER etc.
  #
  func_args_with_defaults <- (
      '(' * func_args_with_defaults_list * ')' |
    '(' * ')'
  )

  func_args_with_defaults_list <- (
    func_arg_with_default |
    func_args_with_defaults_list * ',' * func_arg_with_default
  )

  #
  # The style with arg_class first is SQL99 standard, but Oracle puts
  # param_name first; accept both since it's likely people will try both
  # anyway.  Don't bother trying to save productions by letting arg_class
  # have an empty alternative ... you'll get shift/reduce conflicts.
  #
  # We can catch over-specified arguments here if we want to,
  # but for now better to silently swallow typmod, etc.
  # - thomas 2000-03-22
  #
  func_arg <- (
    arg_class * param_name * func_type |
    param_name * arg_class * func_type   |
    param_name * func_type               |
    arg_class * func_type                |
    func_type
  )

  # INOUT is SQL99 standard, IN OUT is for Oracle compatibility */
  arg_class <- (
    IN_P       |
    OUT_P        |
    INOUT        |
    IN_P * OUT_P |
    VARIADIC
  )

  #
  # Ideally param_name should be ColId, but that causes too many conflicts.
  #
  param_name <- (
    type_function_name
  )

  func_return <- (
    func_type
  )

  #
  # We would like to make the %TYPE productions here be ColId attrs etc,
  # but that causes reduce/reduce conflicts.  type_function_name
  # is next best choice.
  #
  func_type <- (
    Typename                                |
    type_function_name * attrs * '%' * TYPE_P |
    SETOF * type_function_name * attrs * '%' * TYPE_P
  )

  func_arg_with_default <- (
          func_arg              |
    func_arg * DEFAULT * a_expr |
    func_arg * '=' * a_expr
  )

  # Aggregate args can be most things that function args can be */
  aggr_arg <- (
    func_arg
  )

  #
  # The SQL standard offers no guidance on how to declare aggregate argument
  # lists, since it doesn't have CREATE AGGREGATE etc.  We accept these cases:
  #
  # (*)                                   - normal agg with no args
  # (aggr_arg,...)                        - normal agg with args
  # (ORDER BY aggr_arg,...)               - ordered-set agg with no direct args
  # (aggr_arg,... ORDER BY aggr_arg,...)  - ordered-set agg with direct args
  #
  # The zero-argument case is spelled with '*' for consistency with COUNT(*).
  #
  # An additional restriction is that if the direct-args list ends in a
  # VARIADIC item, the ordered-args list must contain exactly one item that
  # is also VARIADIC with the same type.  This allows us to collapse the two
  # VARIADIC items into one, which is necessary to represent the aggregate in
  # pg_proc.  We check this at the grammar stage so that we can return a list
  # in which the second VARIADIC item is already discarded, avoiding extra work
  # in cases such as DROP AGGREGATE.
  #
  # The return value of this production is a two-element list, in which the
  # first item is a sublist of FunctionParameter nodes (with any duplicate
  # VARIADIC item already dropped, as per above) and the second is an integer
  # Value node, containing -1 if there was no ORDER BY and otherwise the number
  # of argument declarations before the ORDER BY.  (If this Digit is equal
  # to the first sublist's length, then we dropped a duplicate VARIADIC item.)
  # This representation is passed as-is to CREATE AGGREGATE; for operations
  # on existing aggregates, we can just apply extractArgTypes to the first
  # sublist.
  #
  aggr_args <- (
      '(' * '*' * ')'                       |
    '(' * aggr_args_list * ')'              |
    '(' * ORDER * BY * aggr_args_list * ')' |
    '(' * aggr_args_list * ORDER * BY * aggr_args_list * ')'
  )

  aggr_args_list <- (
    aggr_arg |
    aggr_args_list * ',' * aggr_arg
  )

  aggregate_with_argtypes <- (
    func_name * aggr_args
  )

  aggregate_with_argtypes_list <- (
    aggregate_with_argtypes |
    aggregate_with_argtypes_list * ',' * aggregate_with_argtypes
  )

  opt_createfunc_opt_list <- Optional(
    createfunc_opt_list
  )

  createfunc_opt_list <- (
    # Must be at least one to prevent conflict */
        createfunc_opt_item |
    createfunc_opt_list * createfunc_opt_item
  )

  #
  # Options common to both CREATE FUNCTION and ALTER FUNCTION
  #
  common_func_opt_item <- (
    CALLED * ON * NULL_P * INPUT_P         |
    RETURNS * NULL_P * ON * NULL_P * INPUT_P |
    STRICT_P                                 |
    IMMUTABLE                                |
    STABLE                                   |
    VOLATILE                                 |
    EXTERNAL * SECURITY * DEFINER            |
    EXTERNAL * SECURITY * INVOKER            |
    SECURITY * DEFINER                       |
    SECURITY * INVOKER                       |
    LEAKPROOF                                |
    NOT * LEAKPROOF                          |
    COST * NumericOnly                       |
    ROWS * NumericOnly                       |
    SUPPORT * any_name                       |
    FunctionSetResetClause                   |
    PARALLEL * ColId
  )

  createfunc_opt_item <- (
    AS * func_as                       |
    LANGUAGE * NonReservedWord_or_Sconst |
    TRANSFORM * transform_type_list      |
    WINDOW                               |
    common_func_opt_item
  )

  func_as <- (
    Sconst |
    Sconst * ',' * Sconst
  )

  ReturnStmt <- (
    RETURN * a_expr
  )

  opt_routine_body <- Optional(
    ReturnStmt |
    BEGIN_P * ATOMIC * routine_body_stmt_list * END_P
  )

  routine_body_stmt_list <- Optional(
    routine_body_stmt_list * routine_body_stmt * ';'
  )

  routine_body_stmt <- (
    stmt |
    ReturnStmt
  )

  transform_type_list <- (
    FOR * TYPE_P * Typename |
    transform_type_list * ',' * FOR * TYPE_P * Typename
  )

  opt_definition <- Optional(
    WITH * definition
  )

  table_func_column <- (
    param_name * func_type
  )

  table_func_column_list <- (
    table_func_column |
    table_func_column_list * ',' * table_func_column
  )

  #****************************************************************************
  # ALTER FUNCTION / ALTER PROCEDURE / ALTER ROUTINE
  #
  # RENAME and OWNER subcommands are already provided by the generic
  # ALTER infrastructure, here we just specify alterations that can
  # only be applied to functions.
  #
  #****************************************************************************/
  AlterFunctionStmt <- (
    ALTER * FUNCTION * function_with_argtypes * alterfunc_opt_list * opt_restrict |
    ALTER * PROCEDURE * function_with_argtypes * alterfunc_opt_list * opt_restrict  |
    ALTER * ROUTINE * function_with_argtypes * alterfunc_opt_list * opt_restrict
  )

  alterfunc_opt_list <- (
    # At least one option must be specified */
      common_func_opt_item |
    alterfunc_opt_list * common_func_opt_item
  )

  # Ignored, merely for SQL compliance */
  opt_restrict <- Optional(
    RESTRICT
  )


  #****************************************************************************
  #
  #       QUERY:
  #
  #       DROP FUNCTION funcname (arg1, arg2, ...) [ RESTRICT | CASCADE ]
  #       DROP PROCEDURE procname (arg1, arg2, ...) [ RESTRICT | CASCADE ]
  #       DROP ROUTINE routname (arg1, arg2, ...) [ RESTRICT | CASCADE ]
  #       DROP AGGREGATE aggname (arg1, ...) [ RESTRICT | CASCADE ]
  #       DROP OPERATOR opname (leftoperand_typ, rightoperand_typ) [ RESTRICT | CASCADE ]
  #
  #****************************************************************************/

  RemoveFuncStmt <- (
    DROP * FUNCTION * function_with_argtypes_list * opt_drop_behavior                |
    DROP * FUNCTION * IF_P * EXISTS * function_with_argtypes_list * opt_drop_behavior  |
    DROP * PROCEDURE * function_with_argtypes_list * opt_drop_behavior                 |
    DROP * PROCEDURE * IF_P * EXISTS * function_with_argtypes_list * opt_drop_behavior |
    DROP * ROUTINE * function_with_argtypes_list * opt_drop_behavior                   |
    DROP * ROUTINE * IF_P * EXISTS * function_with_argtypes_list * opt_drop_behavior
  )

  RemoveAggrStmt <- (
    DROP * AGGREGATE * aggregate_with_argtypes_list * opt_drop_behavior |
    DROP * AGGREGATE * IF_P * EXISTS * aggregate_with_argtypes_list * opt_drop_behavior
  )

  RemoveOperStmt <- (
    DROP * OPERATOR * operator_with_argtypes_list * opt_drop_behavior |
    DROP * OPERATOR * IF_P * EXISTS * operator_with_argtypes_list * opt_drop_behavior
  )

  oper_argtypes <- (
      '(' * Typename * ')'                |
    '(' * Typename * ',' * Typename * ')' |
    '(' * NONE * ',' * Typename * ')' |             # left * unary
    '(' * Typename * ',' * NONE * ')'               # right * unary
  )

  any_operator <- (
    all_Op |
    ColId * '.' * any_operator
  )

  operator_with_argtypes_list <- (
    operator_with_argtypes |
    operator_with_argtypes_list * ',' * operator_with_argtypes
  )

  operator_with_argtypes <- (
    any_operator * oper_argtypes
  )

  #****************************************************************************
  #
  #       DO <anonymous code block> [ LANGUAGE language ]
  #
  # We use a DefElem list for future extensibility, and to allow flexibility
  # in the clause order.
  #
  #****************************************************************************/

  DoStmt <- (
    DO * dostmt_opt_list
  )

  dostmt_opt_list <- (
    dostmt_opt_item |
    dostmt_opt_list * dostmt_opt_item
  )

  dostmt_opt_item <- (
    Sconst |
    LANGUAGE * NonReservedWord_or_Sconst
  )

  #****************************************************************************
  #
  #       CREATE CAST / DROP CAST
  #
  #****************************************************************************/

  CreateCastStmt <- (
    CREATE * CAST * '(' * Typename * AS * Typename * ')' *
        WITH * FUNCTION * function_with_argtypes * cast_context |
    CREATE * CAST * '(' * Typename * AS * Typename * ')' *
        WITHOUT * FUNCTION * cast_context |
    CREATE * CAST * '(' * Typename * AS * Typename * ')' *
        WITH * INOUT * cast_context
  )

  cast_context <- Optional(
    AS * IMPLICIT_P |
    AS * ASSIGNMENT
  )


  DropCastStmt <- (
    DROP * CAST * opt_if_exists * '(' * Typename * AS * Typename * ')' * opt_drop_behavior
  )

  opt_if_exists <- Optional(
    IF_P * EXISTS
  )


  #****************************************************************************
  #
  #       CREATE TRANSFORM / DROP TRANSFORM
  #
  #****************************************************************************/

  CreateTransformStmt <- (
    CREATE * opt_or_replace * TRANSFORM * FOR * Typename * LANGUAGE * name * '(' * transform_element_list * ')'
  )

  transform_element_list <- (
    FROM * SQL_P * WITH * FUNCTION * function_with_argtypes * ',' * TO * SQL_P * WITH * FUNCTION * function_with_argtypes |
    TO * SQL_P * WITH * FUNCTION * function_with_argtypes * ',' * FROM * SQL_P * WITH * FUNCTION * function_with_argtypes   |
    FROM * SQL_P * WITH * FUNCTION * function_with_argtypes                                                                 |
    TO * SQL_P * WITH * FUNCTION * function_with_argtypes
  )


  DropTransformStmt <- (
    DROP * TRANSFORM * opt_if_exists * FOR * Typename * LANGUAGE * name * opt_drop_behavior
  )


  #****************************************************************************
  #
  #       QUERY:
  #
  #       REINDEX [ (options) ] type [CONCURRENTLY] <name>
  #****************************************************************************/

  ReindexStmt <- (
    REINDEX * reindex_target_type * opt_concurrently * qualified_name                                 |
    REINDEX * reindex_target_multitable * opt_concurrently * name                                       |
    REINDEX * '(' * utility_option_list * ')' * reindex_target_type * opt_concurrently * qualified_name |
    REINDEX * '(' * utility_option_list * ')' * reindex_target_multitable * opt_concurrently * name
  )
  reindex_target_type <- (
    INDEX |
    TABLE
  )
  reindex_target_multitable <- (
    SCHEMA |
    SYSTEM_P |
    DATABASE
  )

  #****************************************************************************
  #
  # ALTER TABLESPACE
  #
  #****************************************************************************/

  AlterTblSpcStmt <- (
    ALTER * TABLESPACE * name * SET * reloptions |
    ALTER * TABLESPACE * name * RESET * reloptions
  )

  #****************************************************************************
  #
  # ALTER THING name RENAME TO newname
  #
  #****************************************************************************/

  RenameStmt <- (
    ALTER * AGGREGATE * aggregate_with_argtypes * RENAME * TO * name                                    |
    ALTER * COLLATION * any_name * RENAME * TO * name                                                     |
    ALTER * CONVERSION_P * any_name * RENAME * TO * name                                                  |
    ALTER * DATABASE * name * RENAME * TO * name                                                          |
    ALTER * DOMAIN_P * any_name * RENAME * TO * name                                                      |
    ALTER * DOMAIN_P * any_name * RENAME * CONSTRAINT * name * TO * name                                  |
    ALTER * FOREIGN * DATA_P * WRAPPER * name * RENAME * TO * name                                        |
    ALTER * FUNCTION * function_with_argtypes * RENAME * TO * name                                        |
    ALTER * GROUP_P * RoleId * RENAME * TO * RoleId                                                       |
    ALTER * opt_procedural * LANGUAGE * name * RENAME * TO * name                                         |
    ALTER * OPERATOR * CLASS * any_name * USING * name * RENAME * TO * name                               |
    ALTER * OPERATOR * FAMILY * any_name * USING * name * RENAME * TO * name                              |
    ALTER * POLICY * name * ON * qualified_name * RENAME * TO * name                                      |
    ALTER * POLICY * IF_P * EXISTS * name * ON * qualified_name * RENAME * TO * name                      |
    ALTER * PROCEDURE * function_with_argtypes * RENAME * TO * name                                       |
    ALTER * PUBLICATION * name * RENAME * TO * name                                                       |
    ALTER * ROUTINE * function_with_argtypes * RENAME * TO * name                                         |
    ALTER * SCHEMA * name * RENAME * TO * name                                                            |
    ALTER * SERVER * name * RENAME * TO * name                                                            |
    ALTER * SUBSCRIPTION * name * RENAME * TO * name                                                      |
    ALTER * TABLE * relation_expr * RENAME * TO * name                                                    |
    ALTER * TABLE * IF_P * EXISTS * relation_expr * RENAME * TO * name                                    |
    ALTER * SEQUENCE * qualified_name * RENAME * TO * name                                                |
    ALTER * SEQUENCE * IF_P * EXISTS * qualified_name * RENAME * TO * name                                |
    ALTER * VIEW * qualified_name * RENAME * TO * name                                                    |
    ALTER * VIEW * IF_P * EXISTS * qualified_name * RENAME * TO * name                                    |
    ALTER * MATERIALIZED * VIEW * qualified_name * RENAME * TO * name                                     |
    ALTER * MATERIALIZED * VIEW * IF_P * EXISTS * qualified_name * RENAME * TO * name                     |
    ALTER * INDEX * qualified_name * RENAME * TO * name                                                   |
    ALTER * INDEX * IF_P * EXISTS * qualified_name * RENAME * TO * name                                   |
    ALTER * FOREIGN * TABLE * relation_expr * RENAME * TO * name                                          |
    ALTER * FOREIGN * TABLE * IF_P * EXISTS * relation_expr * RENAME * TO * name                          |
    ALTER * TABLE * relation_expr * RENAME * opt_column * name * TO * name                                |
    ALTER * TABLE * IF_P * EXISTS * relation_expr * RENAME * opt_column * name * TO * name                |
    ALTER * VIEW * qualified_name * RENAME * opt_column * name * TO * name                                |
    ALTER * VIEW * IF_P * EXISTS * qualified_name * RENAME * opt_column * name * TO * name                |
    ALTER * MATERIALIZED * VIEW * qualified_name * RENAME * opt_column * name * TO * name                 |
    ALTER * MATERIALIZED * VIEW * IF_P * EXISTS * qualified_name * RENAME * opt_column * name * TO * name |
    ALTER * TABLE * relation_expr * RENAME * CONSTRAINT * name * TO * name                                |
    ALTER * TABLE * IF_P * EXISTS * relation_expr * RENAME * CONSTRAINT * name * TO * name                |
    ALTER * FOREIGN * TABLE * relation_expr * RENAME * opt_column * name * TO * name                      |
    ALTER * FOREIGN * TABLE * IF_P * EXISTS * relation_expr * RENAME * opt_column * name * TO * name      |
    ALTER * RULE * name * ON * qualified_name * RENAME * TO * name                                        |
    ALTER * TRIGGER * name * ON * qualified_name * RENAME * TO * name                                     |
    ALTER * EVENT * TRIGGER * name * RENAME * TO * name                                                   |
    ALTER * ROLE * RoleId * RENAME * TO * RoleId                                                          |
    ALTER * USER * RoleId * RENAME * TO * RoleId                                                          |
    ALTER * TABLESPACE * name * RENAME * TO * name                                                        |
    ALTER * STATISTICS * any_name * RENAME * TO * name                                                    |
    ALTER * TEXT_P * SEARCH * PARSER * any_name * RENAME * TO * name                                      |
    ALTER * TEXT_P * SEARCH * DICTIONARY * any_name * RENAME * TO * name                                  |
    ALTER * TEXT_P * SEARCH * TEMPLATE * any_name * RENAME * TO * name                                    |
    ALTER * TEXT_P * SEARCH * CONFIGURATION * any_name * RENAME * TO * name                               |
    ALTER * TYPE_P * any_name * RENAME * TO * name                                                        |
    ALTER * TYPE_P * any_name * RENAME * ATTRIBUTE * name * TO * name * opt_drop_behavior
  )

  opt_column <- Optional(
    COLUMN
  )

  opt_set_data <- Optional(
    SET * DATA_P
  )

  #****************************************************************************
  #
  # ALTER THING name DEPENDS ON EXTENSION name
  #
  #****************************************************************************/

  AlterObjectDependsStmt <- (
    ALTER * FUNCTION * function_with_argtypes * opt_no * DEPENDS * ON * EXTENSION * name  |
    ALTER * PROCEDURE * function_with_argtypes * opt_no * DEPENDS * ON * EXTENSION * name   |
    ALTER * ROUTINE * function_with_argtypes * opt_no * DEPENDS * ON * EXTENSION * name     |
    ALTER * TRIGGER * name * ON * qualified_name * opt_no * DEPENDS * ON * EXTENSION * name |
    ALTER * MATERIALIZED * VIEW * qualified_name * opt_no * DEPENDS * ON * EXTENSION * name |
    ALTER * INDEX * qualified_name * opt_no * DEPENDS * ON * EXTENSION * name
  )

  opt_no <- Optional(
    NO
  )

  #****************************************************************************
  #
  # ALTER THING name SET SCHEMA name
  #
  #****************************************************************************/

  AlterObjectSchemaStmt <- (
    ALTER * AGGREGATE * aggregate_with_argtypes * SET * SCHEMA * name                |
    ALTER * COLLATION * any_name * SET * SCHEMA * name                                 |
    ALTER * CONVERSION_P * any_name * SET * SCHEMA * name                              |
    ALTER * DOMAIN_P * any_name * SET * SCHEMA * name                                  |
    ALTER * EXTENSION * name * SET * SCHEMA * name                                     |
    ALTER * FUNCTION * function_with_argtypes * SET * SCHEMA * name                    |
    ALTER * OPERATOR * operator_with_argtypes * SET * SCHEMA * name                    |
    ALTER * OPERATOR * CLASS * any_name * USING * name * SET * SCHEMA * name           |
    ALTER * OPERATOR * FAMILY * any_name * USING * name * SET * SCHEMA * name          |
    ALTER * PROCEDURE * function_with_argtypes * SET * SCHEMA * name                   |
    ALTER * ROUTINE * function_with_argtypes * SET * SCHEMA * name                     |
    ALTER * TABLE * relation_expr * SET * SCHEMA * name                                |
    ALTER * TABLE * IF_P * EXISTS * relation_expr * SET * SCHEMA * name                |
    ALTER * STATISTICS * any_name * SET * SCHEMA * name                                |
    ALTER * TEXT_P * SEARCH * PARSER * any_name * SET * SCHEMA * name                  |
    ALTER * TEXT_P * SEARCH * DICTIONARY * any_name * SET * SCHEMA * name              |
    ALTER * TEXT_P * SEARCH * TEMPLATE * any_name * SET * SCHEMA * name                |
    ALTER * TEXT_P * SEARCH * CONFIGURATION * any_name * SET * SCHEMA * name           |
    ALTER * SEQUENCE * qualified_name * SET * SCHEMA * name                            |
    ALTER * SEQUENCE * IF_P * EXISTS * qualified_name * SET * SCHEMA * name            |
    ALTER * VIEW * qualified_name * SET * SCHEMA * name                                |
    ALTER * VIEW * IF_P * EXISTS * qualified_name * SET * SCHEMA * name                |
    ALTER * MATERIALIZED * VIEW * qualified_name * SET * SCHEMA * name                 |
    ALTER * MATERIALIZED * VIEW * IF_P * EXISTS * qualified_name * SET * SCHEMA * name |
    ALTER * FOREIGN * TABLE * relation_expr * SET * SCHEMA * name                      |
    ALTER * FOREIGN * TABLE * IF_P * EXISTS * relation_expr * SET * SCHEMA * name      |
    ALTER * TYPE_P * any_name * SET * SCHEMA * name
  )

  #****************************************************************************
  #
  # ALTER OPERATOR name SET define
  #
  #****************************************************************************/

  AlterOperatorStmt <- (
    ALTER * OPERATOR * operator_with_argtypes * SET * '(' * operator_def_list * ')'
  )

  operator_def_list <- (
    operator_def_elem |
    operator_def_list * ',' * operator_def_elem
  )

  operator_def_elem <- (
    ColLabel * '=' * NONE |
    ColLabel * '=' * operator_def_arg
  )

  # must be similar enough to def_arg to avoid reduce/reduce conflicts */
  operator_def_arg <- (
    func_type      |
    reserved_keyword |
    qual_all_Op      |
    NumericOnly      |
    Sconst
  )

  #****************************************************************************
  #
  # ALTER TYPE name SET define
  #
  # We repurpose ALTER OPERATOR's version of "definition" here
  #
  #****************************************************************************/

  AlterTypeStmt <- (
    ALTER * TYPE_P * any_name * SET * '(' * operator_def_list * ')'
  )

  #****************************************************************************
  #
  # ALTER THING name OWNER TO newname
  #
  #****************************************************************************/

  AlterOwnerStmt <- (
    ALTER * AGGREGATE * aggregate_with_argtypes * OWNER * TO * RoleSpec       |
    ALTER * COLLATION * any_name * OWNER * TO * RoleSpec                        |
    ALTER * CONVERSION_P * any_name * OWNER * TO * RoleSpec                     |
    ALTER * DATABASE * name * OWNER * TO * RoleSpec                             |
    ALTER * DOMAIN_P * any_name * OWNER * TO * RoleSpec                         |
    ALTER * FUNCTION * function_with_argtypes * OWNER * TO * RoleSpec           |
    ALTER * opt_procedural * LANGUAGE * name * OWNER * TO * RoleSpec            |
    ALTER * LARGE_P * OBJECT_P * NumericOnly * OWNER * TO * RoleSpec            |
    ALTER * OPERATOR * operator_with_argtypes * OWNER * TO * RoleSpec           |
    ALTER * OPERATOR * CLASS * any_name * USING * name * OWNER * TO * RoleSpec  |
    ALTER * OPERATOR * FAMILY * any_name * USING * name * OWNER * TO * RoleSpec |
    ALTER * PROCEDURE * function_with_argtypes * OWNER * TO * RoleSpec          |
    ALTER * ROUTINE * function_with_argtypes * OWNER * TO * RoleSpec            |
    ALTER * SCHEMA * name * OWNER * TO * RoleSpec                               |
    ALTER * TYPE_P * any_name * OWNER * TO * RoleSpec                           |
    ALTER * TABLESPACE * name * OWNER * TO * RoleSpec                           |
    ALTER * STATISTICS * any_name * OWNER * TO * RoleSpec                       |
    ALTER * TEXT_P * SEARCH * DICTIONARY * any_name * OWNER * TO * RoleSpec     |
    ALTER * TEXT_P * SEARCH * CONFIGURATION * any_name * OWNER * TO * RoleSpec  |
    ALTER * FOREIGN * DATA_P * WRAPPER * name * OWNER * TO * RoleSpec           |
    ALTER * SERVER * name * OWNER * TO * RoleSpec                               |
    ALTER * EVENT * TRIGGER * name * OWNER * TO * RoleSpec                      |
    ALTER * PUBLICATION * name * OWNER * TO * RoleSpec                          |
    ALTER * SUBSCRIPTION * name * OWNER * TO * RoleSpec
  )


  #****************************************************************************
  #
  # CREATE PUBLICATION name [ FOR TABLE ] [ WITH options ]
  #
  #****************************************************************************/

  CreatePublicationStmt <- (
    CREATE * PUBLICATION * name * opt_publication_for_tables * opt_definition
  )

  opt_publication_for_tables <- Optional(
    publication_for_tables
  )

  publication_for_tables <- (
    FOR * TABLE * relation_expr_list |
    FOR * ALL * TABLES
  )


  #****************************************************************************
  #
  # ALTER PUBLICATION name SET ( options )
  #
  # ALTER PUBLICATION name ADD TABLE table [, table2]
  #
  # ALTER PUBLICATION name DROP TABLE table [, table2]
  #
  # ALTER PUBLICATION name SET TABLE table [, table2]
  #
  #****************************************************************************/

  AlterPublicationStmt <- (
    ALTER * PUBLICATION * name * SET * definition                 |
    ALTER * PUBLICATION * name * ADD_P * TABLE * relation_expr_list |
    ALTER * PUBLICATION * name * SET * TABLE * relation_expr_list   |
    ALTER * PUBLICATION * name * DROP * TABLE * relation_expr_list
  )

  #****************************************************************************
  #
  # CREATE SUBSCRIPTION name ...
  #
  #****************************************************************************/

  CreateSubscriptionStmt <- (
    CREATE * SUBSCRIPTION * name * CONNECTION * Sconst * PUBLICATION * name_list * opt_definition
  )

  #****************************************************************************
  #
  # ALTER SUBSCRIPTION name ...
  #
  #****************************************************************************/

  AlterSubscriptionStmt <- (
    ALTER * SUBSCRIPTION * name * SET * definition                               |
    ALTER * SUBSCRIPTION * name * CONNECTION * Sconst                              |
    ALTER * SUBSCRIPTION * name * REFRESH * PUBLICATION * opt_definition           |
    ALTER * SUBSCRIPTION * name * ADD_P * PUBLICATION * name_list * opt_definition |
    ALTER * SUBSCRIPTION * name * DROP * PUBLICATION * name_list * opt_definition  |
    ALTER * SUBSCRIPTION * name * SET * PUBLICATION * name_list * opt_definition   |
    ALTER * SUBSCRIPTION * name * ENABLE_P                                         |
    ALTER * SUBSCRIPTION * name * DISABLE_P
  )

  #****************************************************************************
  #
  # DROP SUBSCRIPTION [ IF EXISTS ] name
  #
  #****************************************************************************/

  DropSubscriptionStmt <- (
    DROP * SUBSCRIPTION * name * opt_drop_behavior |
    DROP * SUBSCRIPTION * IF_P * EXISTS * name * opt_drop_behavior
  )

  #****************************************************************************
  #
  #       QUERY:  Define Rewrite Rule
  #
  #****************************************************************************/

  RuleStmt <- (
    CREATE * opt_or_replace * RULE * name * AS *
        ON * event * TO * qualified_name * where_clause *
        DO * opt_instead * RuleActionList
  )

  RuleActionList <- (
    NOTHING      |
    RuleActionStmt |
    '(' * RuleActionMulti * ')'
  )

  # the thrashing around here is to discard "empty" statements... */
  RuleActionMulti <- (
    RuleActionMulti * ';' * RuleActionStmtOrEmpty |
    RuleActionStmtOrEmpty
  )

  RuleActionStmt <- (
    SelectStmt |
    InsertStmt   |
    UpdateStmt   |
    DeleteStmt   |
    NotifyStmt
  )

  RuleActionStmtOrEmpty <- Optional(
    RuleActionStmt
  )

  event <- (
    SELECT |
    UPDATE   |
    DELETE_P |
    INSERT
  )

  opt_instead <- Optional(
    INSTEAD |
    ALSO
  )


  #****************************************************************************
  #
  #       QUERY:
  #               NOTIFY <identifier> can appear both in rule bodies and
  #               as a query-level command
  #
  #****************************************************************************/

  NotifyStmt <- (
    NOTIFY * ColId * notify_payload
  )

  notify_payload <- Optional(
      ',' * Sconst
  )

  ListenStmt <- (
    LISTEN * ColId
  )

  UnlistenStmt <- (
    UNLISTEN * ColId |
    UNLISTEN * '*'
  )


  #****************************************************************************
  #
  #       Transactions:
  #
  #       BEGIN / COMMIT / ROLLBACK
  #       (also older versions END / ABORT)
  #
  #****************************************************************************/

  TransactionStmt <- (
    ABORT_P * opt_transaction * opt_transaction_chain  |
    START * TRANSACTION * transaction_mode_list_or_empty |
    COMMIT * opt_transaction * opt_transaction_chain     |
    ROLLBACK * opt_transaction * opt_transaction_chain   |
    SAVEPOINT * ColId                                    |
    RELEASE * SAVEPOINT * ColId                          |
    RELEASE * ColId                                      |
    ROLLBACK * opt_transaction * TO * SAVEPOINT * ColId  |
    ROLLBACK * opt_transaction * TO * ColId              |
    PREPARE * TRANSACTION * Sconst                       |
    COMMIT * PREPARED * Sconst                           |
    ROLLBACK * PREPARED * Sconst
  )

  TransactionStmtLegacy <- (
    BEGIN_P * opt_transaction * transaction_mode_list_or_empty |
    END_P * opt_transaction * opt_transaction_chain
  )

  opt_transaction <- Optional(
    WORK |
    TRANSACTION
  )

  transaction_mode_item <- (
    ISOLATION * LEVEL * iso_level |
    READ * ONLY                     |
    READ * WRITE                    |
    DEFERRABLE                      |
    NOT * DEFERRABLE
  )

  # Syntax with commas is SQL-spec, without commas is Postgres historical */
  transaction_mode_list <- (
    transaction_mode_item                             |
    transaction_mode_list * ',' * transaction_mode_item |
    transaction_mode_list * transaction_mode_item
  )

  transaction_mode_list_or_empty <- Optional(
    transaction_mode_list
  )

  opt_transaction_chain <- Optional(
    AND * CHAIN |
    AND * NO * CHAIN
  )


  #****************************************************************************
  #
  #   QUERY:
  #       CREATE [ OR REPLACE ] [ TEMP ] VIEW <viewname> '('target-list ')'
  #           AS <query> [ WITH [ CASCADED | LOCAL ] CHECK OPTION ]
  #
  #****************************************************************************/

  ViewStmt <- (
    CREATE * OptTemp * VIEW * qualified_name * opt_column_list * opt_reloptions *
        AS * SelectStmt * opt_check_option |
    CREATE * OR * REPLACE * OptTemp * VIEW * qualified_name * opt_column_list * opt_reloptions *
        AS * SelectStmt * opt_check_option |
    CREATE * OptTemp * RECURSIVE * VIEW * qualified_name * '(' * columnList * ')' * opt_reloptions *
        AS * SelectStmt * opt_check_option |
    CREATE * OR * REPLACE * OptTemp * RECURSIVE * VIEW * qualified_name * '(' * columnList * ')' * opt_reloptions *
        AS * SelectStmt * opt_check_option
  )

  opt_check_option <- Optional(
    WITH * CHECK * OPTION          |
    WITH * CASCADED * CHECK * OPTION |
    WITH * LOCAL * CHECK * OPTION
  )

  #****************************************************************************
  #
  #       QUERY:
  #               LOAD "filename"
  #
  #****************************************************************************/

  LoadStmt <- (
    LOAD * file_name
  )


  #****************************************************************************
  #
  #       CREATE DATABASE
  #
  #****************************************************************************/

  CreatedbStmt <- (
    CREATE * DATABASE * name * opt_with * createdb_opt_list
  )

  createdb_opt_list <- Optional(
    createdb_opt_items
  )

  createdb_opt_items <- (
    createdb_opt_item |
    createdb_opt_items * createdb_opt_item
  )

  createdb_opt_item <- (
    createdb_opt_name * opt_equal * SignedIconst        |
    createdb_opt_name * opt_equal * opt_boolean_or_string |
    createdb_opt_name * opt_equal * DEFAULT
  )

  #
  # Ideally we'd use ColId here, but that causes shift/reduce conflicts against
  # the ALTER DATABASE SET/RESET syntaxes.  Instead call out specific keywords
  # we need, and allow IDENT so that database option names don't have to be
  # parser keywords unless they are already keywords for other reasons.
  #
  # XXX this coding technique is fragile since if someone makes a formerly
  # non-keyword option name into a keyword and forgets to add it here, the
  # option will silently break.  Best defense is to provide a regression test
  # exercising every such option, at least at the syntax level.
  #
  createdb_opt_name <- (
    IDENT            |
    CONNECTION * LIMIT |
    ENCODING           |
    LOCATION           |
    OWNER              |
    TABLESPACE         |
    TEMPLATE
  )

  #
  #   Though the equals sign doesn't match other WITH options, pg_dump uses
  #   equals for backward compatibility, and it doesn't seem worth removing it.
  #
  opt_equal <- Optional(
      '='
  )


  #****************************************************************************
  #
  #       ALTER DATABASE
  #
  #****************************************************************************/

  AlterDatabaseStmt <- (
    ALTER * DATABASE * name * WITH * createdb_opt_list |
    ALTER * DATABASE * name * createdb_opt_list          |
    ALTER * DATABASE * name * SET * TABLESPACE * name
  )

  AlterDatabaseSetStmt <- (
    ALTER * DATABASE * name * SetResetClause
  )


  #****************************************************************************
  #
  #       DROP DATABASE [ IF EXISTS ] dbname [ [ WITH ] ( options ) ]
  #
  # This is implicitly CASCADE, no need for drop behavior
  #****************************************************************************/

  DropdbStmt <- (
    DROP * DATABASE * name                                         |
    DROP * DATABASE * IF_P * EXISTS * name                           |
    DROP * DATABASE * name * opt_with * '(' * drop_option_list * ')' |
    DROP * DATABASE * IF_P * EXISTS * name * opt_with * '(' * drop_option_list * ')'
  )

  drop_option_list <- (
    drop_option |
    drop_option_list * ',' * drop_option
  )

  #
  # Currently only the FORCE option is supported, but the syntax is designed
  # to be extensible so that we can add more options in the future if required.
  #
  drop_option <- (
    FORCE
  )

  #****************************************************************************
  #
  #       ALTER COLLATION
  #
  #****************************************************************************/

  AlterCollationStmt <- (
    ALTER * COLLATION * any_name * REFRESH * VERSION_P
  )


  #****************************************************************************
  #
  #       ALTER SYSTEM
  #
  # This is used to change configuration parameters persistently.
  #****************************************************************************/

  AlterSystemStmt <- (
    ALTER * SYSTEM_P * SET * generic_set |
    ALTER * SYSTEM_P * RESET * generic_reset
  )


  #****************************************************************************
  #
  # Manipulate a domain
  #
  #****************************************************************************/

  CreateDomainStmt <- (
    CREATE * DOMAIN_P * any_name * opt_as * Typename * ColQualList
  )

  AlterDomainStmt <- (
    # ALTER DOMAIN <domain> {SET DEFAULT <expr>|DROP DEFAULT} */
      ALTER * DOMAIN_P * any_name * alter_column_default |

    # ALTER DOMAIN <domain> DROP NOT NULL */ 
    ALTER * DOMAIN_P * any_name * DROP * NOT * NULL_P |

    # ALTER DOMAIN <domain> SET NOT NULL */ 
    ALTER * DOMAIN_P * any_name * SET * NOT * NULL_P |

    # ALTER DOMAIN <domain> ADD CONSTRAINT ... */ 
    ALTER * DOMAIN_P * any_name * ADD_P * TableConstraint |

    # ALTER DOMAIN <domain> DROP CONSTRAINT <name> [RESTRICT|CASCADE] */ 
    ALTER * DOMAIN_P * any_name * DROP * CONSTRAINT * name * opt_drop_behavior |

    # ALTER DOMAIN <domain> DROP CONSTRAINT IF EXISTS <name> [RESTRICT|CASCADE] */ 
    ALTER * DOMAIN_P * any_name * DROP * CONSTRAINT * IF_P * EXISTS * name * opt_drop_behavior |

    # ALTER DOMAIN <domain> VALIDATE CONSTRAINT <name> */ 
    ALTER * DOMAIN_P * any_name * VALIDATE * CONSTRAINT * name

  )

  opt_as <- Optional(
    AS
  )


  #****************************************************************************
  #
  # Manipulate a text search dictionary or configuration
  #
  #****************************************************************************/

  AlterTSDictionaryStmt <- (
    ALTER * TEXT_P * SEARCH * DICTIONARY * any_name * definition
  )

  AlterTSConfigurationStmt <- (
    ALTER * TEXT_P * SEARCH * CONFIGURATION * any_name * ADD_P * MAPPING * FOR * name_list * any_with * any_name_list               |
    ALTER * TEXT_P * SEARCH * CONFIGURATION * any_name * ALTER * MAPPING * FOR * name_list * any_with * any_name_list                 |
    ALTER * TEXT_P * SEARCH * CONFIGURATION * any_name * ALTER * MAPPING * REPLACE * any_name * any_with * any_name                   |
    ALTER * TEXT_P * SEARCH * CONFIGURATION * any_name * ALTER * MAPPING * FOR * name_list * REPLACE * any_name * any_with * any_name |
    ALTER * TEXT_P * SEARCH * CONFIGURATION * any_name * DROP * MAPPING * FOR * name_list                                             |
    ALTER * TEXT_P * SEARCH * CONFIGURATION * any_name * DROP * MAPPING * IF_P * EXISTS * FOR * name_list
  )

  # Use this if TIME or ORDINALITY after WITH should be taken as an identifier */
  any_with <- (
    WITH |
    WITH_LA
  )


  #****************************************************************************
  #
  # Manipulate a conversion
  #
  #       CREATE [DEFAULT] CONVERSION <conversion_name>
  #       FOR <encoding_name> TO <encoding_name> FROM <func_name>
  #
  #****************************************************************************/

  CreateConversionStmt <- (
    CREATE * opt_default * CONVERSION_P * any_name * FOR * Sconst *
        TO * Sconst * FROM * any_name
  )

  #****************************************************************************
  #
  #       QUERY:
  #               CLUSTER [VERBOSE] <qualified_name> [ USING <index_name> ]
  #               CLUSTER [ (options) ] <qualified_name> [ USING <index_name> ]
  #               CLUSTER [VERBOSE]
  #               CLUSTER [VERBOSE] <index_name> ON <qualified_name> (for pre-8.3)
  #
  #****************************************************************************/

  ClusterStmt <- (
    CLUSTER * opt_verbose * qualified_name * cluster_index_specification                   |
    CLUSTER * '(' * utility_option_list * ')' * qualified_name * cluster_index_specification |
    CLUSTER * opt_verbose                                                                    |

    # kept for pre-8.3 compatibility */ 
    CLUSTER * opt_verbose * name * ON * qualified_name
  )

  cluster_index_specification <- Optional(
    USING * name
  )


  #****************************************************************************
  #
  #       QUERY:
  #               VACUUM
  #               ANALYZE
  #
  #****************************************************************************/

  VacuumStmt <- (
    VACUUM * opt_full * opt_freeze * opt_verbose * opt_analyze * opt_vacuum_relation_list |
    VACUUM * '(' * utility_option_list * ')' * opt_vacuum_relation_list
  )

  AnalyzeStmt <- (
    analyze_keyword * opt_verbose * opt_vacuum_relation_list |
    analyze_keyword * '(' * utility_option_list * ')' * opt_vacuum_relation_list
  )

  utility_option_list <- (
    utility_option_elem |
    utility_option_list * ',' * utility_option_elem
  )

  analyze_keyword <- (
    ANALYZE |
    ANALYSE # British
  )

  utility_option_elem <- (
    utility_option_name * utility_option_arg
  )

  utility_option_name <- (
    NonReservedWord |
    analyze_keyword
  )

  utility_option_arg <- Optional(
    opt_boolean_or_string |
    NumericOnly
  )

  opt_analyze <- Optional(
    analyze_keyword
  )

  opt_verbose <- Optional(
    VERBOSE
  )

  opt_full <- Optional(
    FULL
  )

  opt_freeze <- Optional(
    FREEZE
  )

  opt_name_list <- Optional(
      '(' * name_list * ')'
  )

  vacuum_relation <- (
    qualified_name * opt_name_list
  )

  vacuum_relation_list <- (
    vacuum_relation |
    vacuum_relation_list * ',' * vacuum_relation
  )

  opt_vacuum_relation_list <- Optional(
    vacuum_relation_list
  )


  #****************************************************************************
  #
  #       QUERY:
  #               EXPLAIN [ANALYZE] [VERBOSE] query
  #               EXPLAIN ( options ) query
  #
  #****************************************************************************/

  ExplainStmt <- (
    EXPLAIN * ExplainableStmt                               |
    EXPLAIN * analyze_keyword * opt_verbose * ExplainableStmt |
    EXPLAIN * VERBOSE * ExplainableStmt                       |
    EXPLAIN * '(' * utility_option_list * ')' * ExplainableStmt
  )

  ExplainableStmt <- (
    SelectStmt       |
    InsertStmt         |
    UpdateStmt         |
    DeleteStmt         |
    DeclareCursorStmt  |
    CreateAsStmt       |
    CreateMatViewStmt  |
    RefreshMatViewStmt |
    ExecuteStmt                 # by * default * all * are $$=$1 */
  )

  #****************************************************************************
  #
  #       QUERY:
  #               PREPARE <plan_name> [(args, ...)] AS <query>
  #
  #****************************************************************************/

  PrepareStmt <- (
    PREPARE * name * prep_type_clause * AS * PreparableStmt
  )

  prep_type_clause <- Optional(
      '(' * type_list * ')'
  )

  PreparableStmt <- (
    SelectStmt |
    InsertStmt   |
    UpdateStmt   |
    DeleteStmt                  # by * default * all * are $$=$1 */
  )

  #****************************************************************************
  #
  # EXECUTE <plan_name> [(params, ...)]
  # CREATE TABLE <name> AS EXECUTE <plan_name> [(params, ...)]
  #
  #****************************************************************************/

  ExecuteStmt <- (
    EXECUTE * name * execute_param_clause |
    CREATE * OptTemp * TABLE * create_as_target * AS *
        EXECUTE * name * execute_param_clause * opt_with_data |
    CREATE * OptTemp * TABLE * IF_P * NOT * EXISTS * create_as_target * AS *
        EXECUTE * name * execute_param_clause * opt_with_data
  )

  execute_param_clause <- Optional(
      '(' * expr_list * ')'
  )

  #****************************************************************************
  #
  #       QUERY:
  #               DEALLOCATE [PREPARE] <plan_name>
  #
  #****************************************************************************/

  DeallocateStmt <- (
    DEALLOCATE * name         |
    DEALLOCATE * PREPARE * name |
    DEALLOCATE * ALL            |
    DEALLOCATE * PREPARE * ALL
  )

  #****************************************************************************
  #
  #       QUERY:
  #               INSERT STATEMENTS
  #
  #****************************************************************************/

  InsertStmt <- (
    opt_with_clause * INSERT * INTO * insert_target * insert_rest *
        opt_on_conflict * returning_clause
  )

  #
  # Can't easily make AS optional here, because VALUES in insert_rest would
  # have a shift/reduce conflict with VALUES as an optional alias.  We could
  # easily allow unreserved_keywords as optional aliases, but that'd be an odd
  # divergence from other places.  So just require AS for now.
  #
  insert_target <- (
    qualified_name |
    qualified_name * AS * ColId
  )

  insert_rest <- (
    SelectStmt                                                                       |
    OVERRIDING * override_kind * VALUE_P * SelectStmt                                  |
    '(' * insert_column_list * ')' * SelectStmt                                        |
    '(' * insert_column_list * ')' * OVERRIDING * override_kind * VALUE_P * SelectStmt |
    DEFAULT * VALUES
  )

  override_kind <- (
    USER |
    SYSTEM_P
  )

  insert_column_list <- (
    insert_column_item |
    insert_column_list * ',' * insert_column_item
  )

  insert_column_item <- (
    ColId * opt_indirection
  )

  opt_on_conflict <- Optional(
    ON * CONFLICT * opt_conf_expr * DO * UPDATE * SET * set_clause_list * where_clause |
    ON * CONFLICT * opt_conf_expr * DO * NOTHING
  )

  opt_conf_expr <- Optional(
      '(' * index_params * ')' * where_clause |
    ON * CONSTRAINT * name
  )

  returning_clause <- Optional(
    RETURNING * target_list
  )


  #****************************************************************************
  #
  #       QUERY:
  #               DELETE STATEMENTS
  #
  #****************************************************************************/

  DeleteStmt <- (
    opt_with_clause * DELETE_P * FROM * relation_expr_opt_alias *
        using_clause * where_or_current_clause * returning_clause
  )

  using_clause <- Optional(
    USING * from_list
  )


  #****************************************************************************
  #
  #       QUERY:
  #               LOCK TABLE
  #
  #****************************************************************************/

  LockStmt <- (
    LOCK_P * opt_table * relation_expr_list * opt_lock * opt_nowait
  )

  opt_lock <- Optional(
    IN_P * lock_type * MODE
  )

  lock_type <- (
    ACCESS * SHARE           |
    ROW * SHARE                |
    ROW * EXCLUSIVE            |
    SHARE * UPDATE * EXCLUSIVE |
    SHARE                      |
    SHARE * ROW * EXCLUSIVE    |
    EXCLUSIVE                  |
    ACCESS * EXCLUSIVE
  )

  opt_nowait <- Optional(
    NOWAIT
  )

  opt_nowait_or_skip <- Optional(
    NOWAIT |
    SKIP * LOCKED
  )


  #****************************************************************************
  #
  #       QUERY:
  #               UpdateStmt (UPDATE)
  #
  #****************************************************************************/

  UpdateStmt <- (
    opt_with_clause * UPDATE * relation_expr_opt_alias *
        SET * set_clause_list *
        from_clause *
        where_or_current_clause *
        returning_clause
  )

  set_clause_list <- (
    set_clause |
    set_clause_list * ',' * set_clause
  )

  set_clause <- (
    set_target * '=' * a_expr |
    '(' * set_target_list * ')' * '=' * a_expr
  )

  set_target <- (
    ColId * opt_indirection
  )

  set_target_list <- (
    set_target |
    set_target_list * ',' * set_target
  )


  #****************************************************************************
  #
  #       QUERY:
  #               CURSOR STATEMENTS
  #
  #****************************************************************************/
  DeclareCursorStmt <- (
    DECLARE * cursor_name * cursor_options * CURSOR * opt_hold * FOR * SelectStmt
  )

  cursor_name <- (
    name
  )

  cursor_options <- Optional(
    cursor_options * NO * SCROLL |
    cursor_options * SCROLL      |
    cursor_options * BINARY      |
    cursor_options * ASENSITIVE  |
    cursor_options * INSENSITIVE
  )

  opt_hold <- Optional(
    WITH * HOLD |
    WITHOUT * HOLD
  )

  #****************************************************************************
  #
  #       QUERY:
  #               SELECT STATEMENTS
  #
  #****************************************************************************/

  # A complete SELECT statement looks like this.
  #
  # The rule returns either a single SelectStmt node or a tree of them,
  # representing a set-operation tree.
  #
  # There is an ambiguity when a sub-SELECT is within an a_expr and there
  # are excess parentheses: do the parentheses belong to the sub-SELECT or
  # to the surrounding a_expr?  We don't really care, but bison wants to know.
  # To resolve the ambiguity, we are careful to define the grammar so that
  # the decision is staved off as long as possible: as long as we can keep
  # absorbing parentheses into the sub-SELECT, we will do so, and only when
  # it's no longer possible to do that will we decide that parens belong to
  # the expression.   For example, in "SELECT (((SELECT 2)) + 3)" the extra
  # parentheses are treated as part of the sub-select.  The necessity of doing
  # it that way is shown by "SELECT (((SELECT 2)) UNION SELECT 2)".   Had we
  # parsed "((SELECT 2))" as an a_expr, it'd be too late to go back to the
  # SELECT viewpoint when we see the UNION.
  #
  # This approach is implemented by defining a nonterminal select_with_parens,
  # which represents a SELECT with at least one outer layer of parentheses,
  # and being careful to use select_with_parens, never '(' SelectStmt ')',
  # in the expression grammar.  We will then have shift-reduce conflicts
  # which we can resolve in favor of always treating '(' <select> ')' as
  # a select_with_parens.  To resolve the conflicts, the productions that
  # conflict with the select_with_parens productions are manually given
  # precedences lower than the precedence of ')', thereby ensuring that we
  # shift ')' (and then reduce to select_with_parens) rather than trying to
  # reduce the inner <select> nonterminal to something else.  We use UMINUS
  # precedence for this, which is a fairly arbitrary choice.
  #
  # To be able to define select_with_parens itself without ambiguity, we need
  # a nonterminal select_no_parens that represents a SELECT structure with no
  # outermost parentheses.  This is a little bit tedious, but it works.
  #
  # In non-expression contexts, we use SelectStmt which can represent a SELECT
  # with or without outer parentheses.
  #

  SelectStmt <- (
    select_no_parens   | #  # %prec * UMINUS
    select_with_parens   #  # %prec * UMINUS
  )

  select_with_parens <- (
      '(' * select_no_parens * ')' |
    '(' * select_with_parens * ')'
  )

  #
  # This rule parses the equivalent of the standard's <query expression>.
  # The duplicative productions are annoying, but hard to get rid of without
  # creating shift/reduce conflicts.
  #
  #   The locking clause (FOR UPDATE etc) may be before or after LIMIT/OFFSET.
  #   In <=7.2.X, LIMIT/OFFSET had to be after FOR UPDATE
  #   We now support both orderings, but prefer LIMIT/OFFSET before the locking
  # clause.
  #   2002-08-28 bjm
  #
  select_no_parens <- (
    simple_select                                                                       |
    select_clause * sort_clause                                                           |
    select_clause * opt_sort_clause * for_locking_clause * opt_select_limit               |
    select_clause * opt_sort_clause * select_limit * opt_for_locking_clause               |
    with_clause * select_clause                                                           |
    with_clause * select_clause * sort_clause                                             |
    with_clause * select_clause * opt_sort_clause * for_locking_clause * opt_select_limit |
    with_clause * select_clause * opt_sort_clause * select_limit * opt_for_locking_clause
  )

  select_clause <- (
    simple_select |
    select_with_parens
  )

  #
  # This rule parses SELECT statements that can appear within set operations,
  # including UNION, INTERSECT and EXCEPT.  '(' and ')' can be used to specify
  # the ordering of the set operations.   Without '(' and ')' we want the
  # operations to be ordered per the precedence specs at the head of this file.
  #
  # As with select_no_parens, simple_select cannot have outer parentheses,
  # but can have parenthesized subclauses.
  #
  # It might appear that we could fold the first two alternatives into one
  # by using opt_distinct_clause.  However, that causes a shift/reduce conflict
  # against INSERT ... SELECT ... ON CONFLICT.  We avoid the ambiguity by
  # requiring SELECT DISTINCT [ON] to be followed by a non-empty target_list.
  #
  # Note that sort clauses cannot be included at this level --- SQL requires
  #       SELECT foo UNION SELECT bar ORDER BY baz
  # to be parsed as
  #       (SELECT foo UNION SELECT bar) ORDER BY baz
  # not
  #       SELECT foo UNION (SELECT bar ORDER BY baz)
  # Likewise for WITH, FOR UPDATE and LIMIT.  Therefore, those clauses are
  # described as part of the select_no_parens production, not simple_select.
  # This does not limit functionality, because you can reintroduce these
  # clauses inside parentheses.
  #
  # NOTE: only the leftmost component SelectStmt should have INTO.
  # However, this is not checked by the grammar; parse analysis must check it.
  #
  simple_select <- (
    SELECT * opt_all_clause * opt_target_list *
        into_clause * from_clause * where_clause *
        group_clause * having_clause * window_clause |
    SELECT * distinct_clause * target_list *
        into_clause * from_clause * where_clause *
        group_clause * having_clause * window_clause           |
    values_clause                                              |
    TABLE * relation_expr                                      |
    select_clause * UNION * set_quantifier * select_clause     |
    select_clause * INTERSECT * set_quantifier * select_clause |
    select_clause * EXCEPT * set_quantifier * select_clause
  )

  #
  # SQL standard WITH clause looks like:
  #
  # WITH [ RECURSIVE ] <query name> [ (<column>,...) ]
  #       AS (query) [ SEARCH or CYCLE clause ]
  #
  # Recognizing WITH_LA here allows a CTE to be named TIME or ORDINALITY.
  #
  with_clause <- (
    WITH * cte_list  |
    WITH_LA * cte_list |
    WITH * RECURSIVE * cte_list
  )

  cte_list <- (
    common_table_expr |
    cte_list * ',' * common_table_expr
  )

  common_table_expr <- (
    name * opt_name_list * AS * opt_materialized * '(' * PreparableStmt * ')' * opt_search_clause * opt_cycle_clause
  )

  opt_materialized <- Optional(
    MATERIALIZED |
    NOT * MATERIALIZED
  )

  opt_search_clause <- Optional(
    SEARCH * DEPTH * FIRST_P * BY * columnList * SET * ColId |
    SEARCH * BREADTH * FIRST_P * BY * columnList * SET * ColId
  )

  opt_cycle_clause <- Optional(
    CYCLE * columnList * SET * ColId * TO * AexprConst * DEFAULT * AexprConst * USING * ColId |
    CYCLE * columnList * SET * ColId * USING * ColId
  )

  opt_with_clause <- Optional(
    with_clause
  )

  into_clause <- Optional(
    INTO * OptTempTableName
  )

  #
  # Redundancy here is needed to avoid shift/reduce conflicts,
  # since TEMP is not a reserved word.  See also OptTemp.
  #
  OptTempTableName <- (
    TEMPORARY * opt_table * qualified_name        |
    TEMP * opt_table * qualified_name               |
    LOCAL * TEMPORARY * opt_table * qualified_name  |
    LOCAL * TEMP * opt_table * qualified_name       |
    GLOBAL * TEMPORARY * opt_table * qualified_name |
    GLOBAL * TEMP * opt_table * qualified_name      |
    UNLOGGED * opt_table * qualified_name           |
    TABLE * qualified_name                          |
    qualified_name
  )

  opt_table <- Optional(
    TABLE
  )

  set_quantifier <- Optional(
    ALL |
    DISTINCT
  )

  # We use (NIL) as a placeholder to indicate that all target expressions
  # should be placed in the DISTINCT list during parsetree analysis.
  #
  distinct_clause <- (
    DISTINCT |
    DISTINCT * ON * '(' * expr_list * ')'
  )

  opt_all_clause <- Optional(
    ALL
  )

  opt_distinct_clause <- (
    distinct_clause |
    opt_all_clause
  )

  opt_sort_clause <- Optional(
    sort_clause
  )

  sort_clause <- (
    ORDER * BY * sortby_list
  )

  sortby_list <- (
    sortby |
    sortby_list * ',' * sortby
  )

  sortby <- (
    a_expr * USING * qual_all_Op * opt_nulls_order |
    a_expr * opt_asc_desc * opt_nulls_order
  )


  select_limit <- (
    limit_clause * offset_clause |
    offset_clause * limit_clause   |
    limit_clause                   |
    offset_clause
  )

  opt_select_limit <- Optional(
    select_limit
  )

  limit_clause <- (
    LIMIT * select_limit_value                           |
    LIMIT * select_limit_value * ',' * select_offset_value |

    # SQL:2008 syntax */ 
    # to avoid shift/reduce conflicts, handle the optional value with 
    # a separate production rather than an opt_ expression.  The fact 
    # that ONLY is fully reserved means that this way, we defer any 
    # decision about what rule reduces ROW or ROWS to the point where 
    # we can see the ONLY token in the lookahead slot. 
    # 
    FETCH * first_or_next * select_fetch_first_value * row_or_rows * ONLY        |
    FETCH * first_or_next * select_fetch_first_value * row_or_rows * WITH * TIES |
    FETCH * first_or_next * row_or_rows * ONLY                                   |
    FETCH * first_or_next * row_or_rows * WITH * TIES
  )

  offset_clause <- (
    OFFSET * select_offset_value |

    # SQL:2008 syntax */ 
    OFFSET * select_fetch_first_value * row_or_rows
  )

  select_limit_value <- (
    a_expr |
    ALL
  )

  select_offset_value <- (
    a_expr
  )

  #
  # Allowing full expressions without parentheses causes various parsing
  # problems with the trailing ROW/ROWS key words.  SQL spec only calls for
  # <simple value specification>, which is either a literal or a parameter (but
  # an <SQL parameter reference> could be an identifier, bringing up conflicts
  # with ROW/ROWS). We solve this by leveraging the presence of ONLY (see above)
  # to determine whether the expression is missing rather than trying to make it
  # optional in this rule.
  #
  # c_expr covers almost all the spec-required cases (and more), but it doesn't
  # cover signed numeric literals, which are allowed by the spec. So we include
  # those here explicitly. We need FCONST as well as ICONST because values that
  # don't fit in the platform's "long", but do fit in bigint, should still be
  # accepted here. (This is possible in 64-bit Windows as well as all 32-bit
  # builds.)
  #
  select_fetch_first_value <- (
    c_expr           |
    '+' * I_or_F_const |
    '-' * I_or_F_const
  )

  I_or_F_const <- (
    Iconst |
    FCONST
  )

  # noise words */
  row_or_rows <- (
    ROW |
    ROWS
  )

  first_or_next <- (
    FIRST_P |
    NEXT
  )


  #
  # This syntax for group_clause tries to follow the spec quite closely.
  # However, the spec allows only column references, not expressions,
  # which introduces an ambiguity between implicit row constructors
  # (a,b) and lists of column references.
  #
  # We handle this by using the a_expr production for what the spec calls
  # <ordinary grouping set>, which in the spec represents either one column
  # reference or a parenthesized list of column references. Then, we check the
  # top node of the a_expr to see if it's an implicit RowExpr, and if so, just
  # grab and use the list, discarding the node. (this is done in parse analysis,
  # not here)
  #
  # (we abuse the row_format field of RowExpr to distinguish implicit and
  # explicit row constructors; it's debatable if anyone sanely wants to use them
  # in a group clause, but if they have a reason to, we make it possible.)
  #
  # Each item in the group_clause list is either an expression tree or a
  # GroupingSet node of some type.
  #
  group_clause <- Optional(
    GROUP_P * BY * set_quantifier * group_by_list
  )

  group_by_list <- (
    group_by_item |
    group_by_list * ',' * group_by_item
  )

  group_by_item <- (
    a_expr           |
    empty_grouping_set |
    cube_clause        |
    rollup_clause      |
    grouping_sets_clause
  )

  empty_grouping_set <- (
      '(' * ')'
  )

  #
  # These hacks rely on setting precedence of CUBE and ROLLUP below that of '(',
  # so that they shift in these rules rather than reducing the conflicting
  # unreserved_keyword rule.
  #

  rollup_clause <- (
    ROLLUP * '(' * expr_list * ')'
  )

  cube_clause <- (
    CUBE * '(' * expr_list * ')'
  )

  grouping_sets_clause <- (
    GROUPING * SETS * '(' * group_by_list * ')'
  )

  having_clause <- Optional(
    HAVING * a_expr
  )

  for_locking_clause <- (
    for_locking_items |
    FOR * READ * ONLY
  )

  opt_for_locking_clause <- Optional(
    for_locking_clause
  )

  for_locking_items <- (
    for_locking_item |
    for_locking_items * for_locking_item
  )

  for_locking_item <- (
    for_locking_strength * locked_rels_list * opt_nowait_or_skip
  )

  for_locking_strength <- (
    FOR * UPDATE          |
    FOR * NO * KEY * UPDATE |
    FOR * SHARE             |
    FOR * KEY * SHARE
  )

  locked_rels_list <- Optional(
    OF * qualified_name_list
  )


  #
  # We should allow ROW '(' expr_list ')' too, but that seems to require
  # making VALUES a fully reserved word, which will probably break more apps
  # than allowing the noise-word is worth.
  #
  values_clause <- (
    VALUES * '(' * expr_list * ')' |
    values_clause * ',' * '(' * expr_list * ')'
  )


  #****************************************************************************
  #
  #   clauses common to all Optimizable Stmts:
  #       from_clause     - allow list of both JOIN expressions and table names
  #       where_clause    - qualifications for joins or restrictions
  #
  #****************************************************************************/

  from_clause <- Optional(
    FROM * from_list
  )

  from_list <- (
    table_ref |
    from_list * ',' * table_ref
  )

  #
  # table_ref is where an alias clause can be attached.
  #
  table_ref <- (
    relation_expr * opt_alias_clause                    |
    relation_expr * opt_alias_clause * tablesample_clause |
    func_table * func_alias_clause                        |
    LATERAL_P * func_table * func_alias_clause            |
    xmltable * opt_alias_clause                           |
    LATERAL_P * xmltable * opt_alias_clause               |
    select_with_parens * opt_alias_clause                 |
    LATERAL_P * select_with_parens * opt_alias_clause     |
    joined_table                                          |
    '(' * joined_table * ')' * alias_clause
  )


  #
  # It may seem silly to separate joined_table from table_ref, but there is
  # method in SQL's madness: if you don't do it this way you get reduce-
  # reduce conflicts, because it's not clear to the parser generator whether
  # to expect alias_clause after ')' or not.  For the same reason we must
  # treat 'JOIN' and 'join_type JOIN' separately, rather than allowing
  # join_type to expand to empty; if we try it, the parser generator can't
  # figure out when to reduce an empty join_type right after table_ref.
  #
  # Note that a CROSS JOIN is the same as an unqualified
  # INNER JOIN, and an INNER JOIN/ON has the same shape
  # but a qualification expression to limit membership.
  # A NATURAL JOIN implicitly matches column names between
  # tables and the shape is determined by which columns are
  # in common. We'll collect columns during the later transformations.
  #

  joined_table <- (
      '(' * joined_table * ')'                           |
    table_ref * CROSS * JOIN * table_ref                 |
    table_ref * join_type * JOIN * table_ref * join_qual |
    table_ref * JOIN * table_ref * join_qual             |
    table_ref * NATURAL * join_type * JOIN * table_ref   |
    table_ref * NATURAL * JOIN * table_ref
  )

  alias_clause <- (
    AS * ColId * '(' * name_list * ')' |
    AS * ColId                           |
    ColId * '(' * name_list * ')'        |
    ColId
  )

  opt_alias_clause <- Optional(
    alias_clause
  )

  #
  # The alias clause after JOIN ... USING only accepts the AS ColId spelling,
  # per SQL standard.  (The grammar could parse the other variants, but they
  # don't seem to be useful, and it might lead to parser problems in the
  # future.)
  #
  opt_alias_clause_for_join_using <- Optional(
    AS * ColId
  )

  #
  # func_alias_clause can include both an Alias and a coldeflist, so we make it
  # return a 2-element list that gets disassembled by calling production.
  #
  func_alias_clause <- Optional(
    alias_clause                                |
    AS * '(' * TableFuncElementList * ')'         |
    AS * ColId * '(' * TableFuncElementList * ')' |
    ColId * '(' * TableFuncElementList * ')'
  )

  join_type <- (
    FULL * opt_outer |
    LEFT * opt_outer   |
    RIGHT * opt_outer  |
    INNER_P
  )

  # OUTER is just noise... */
  opt_outer <- Optional(
    OUTER_P
  )

  # JOIN qualification clauses
  # Possibilities are:
  #   USING ( column list ) [ AS alias ]
  #                         allows only unqualified column names,
  #                         which must match between tables.
  #   ON expr allows more general qualifications.
  #
  # We return USING as a two-element List (the first item being a sub-List
  # of the common column names, and the second either an Alias item or NULL).
  # An ON-expr will not be a List, so it can be told apart that way.
  #

  join_qual <- (
    USING * '(' * name_list * ')' * opt_alias_clause_for_join_using |
    ON * a_expr
  )


  relation_expr <- (
    qualified_name      |
    qualified_name * '*'  |
    ONLY * qualified_name |
    ONLY * '(' * qualified_name * ')'
  )


  relation_expr_list <- (
    relation_expr |
    relation_expr_list * ',' * relation_expr
  )


  #
  # Given "UPDATE foo set set ...", we have to decide without looking any
  # further ahead whether the first "set" is an alias or the UPDATE's SET
  # keyword.  Since "set" is allowed as a column name both interpretations
  # are feasible.  We resolve the shift/reduce conflict by giving the first
  # relation_expr_opt_alias production a higher precedence than the SET token
  # has, causing the parser to prefer to reduce, in effect assuming that the
  # SET is not an alias.
  #
  relation_expr_opt_alias <- (
    relation_expr         | #  # %prec * UMINUS
    relation_expr * ColId |
    relation_expr * AS * ColId
  )

  #
  # TABLESAMPLE decoration in a FROM item
  #
  tablesample_clause <- (
    TABLESAMPLE * func_name * '(' * expr_list * ')' * opt_repeatable_clause
  )

  opt_repeatable_clause <- Optional(
    REPEATABLE * '(' * a_expr * ')'
  )

  #
  # func_table represents a function invocation in a FROM list. It can be
  # a plain function call, like "foo(...)", or a ROWS FROM expression with
  # one or more function calls, "ROWS FROM (foo(...), bar(...))",
  # optionally with WITH ORDINALITY attached.
  # In the ROWS FROM syntax, a column definition list can be given for each
  # function, for example:
  #     ROWS FROM (foo() AS (foo_res_a text, foo_res_b text),
  #                bar() AS (bar_res_a text, bar_res_b text))
  # It's also possible to attach a column definition list to the RangeFunction
  # as a whole, but that's handled by the table_ref production.
  #
  func_table <- (
    func_expr_windowless * opt_ordinality |
    ROWS * FROM * '(' * rowsfrom_list * ')' * opt_ordinality
  )

  rowsfrom_item <- (
    func_expr_windowless * opt_col_def_list
  )

  rowsfrom_list <- (
    rowsfrom_item |
    rowsfrom_list * ',' * rowsfrom_item
  )

  opt_col_def_list <- Optional(
    AS * '(' * TableFuncElementList * ')'
  )

  opt_ordinality <- Optional(
    WITH_LA * ORDINALITY
  )


  where_clause <- Optional(
    WHERE * a_expr
  )

  # variant for UPDATE and DELETE */
  where_or_current_clause <- Optional(
    WHERE * a_expr |
    WHERE * CURRENT_P * OF * cursor_name
  )


  OptTableFuncElementList <- Optional(
    TableFuncElementList
  )

  TableFuncElementList <- (
    TableFuncElement |
    TableFuncElementList * ',' * TableFuncElement
  )

  TableFuncElement <- (
    ColId * Typename * opt_collate_clause
  )

  #
  # XMLTABLE
  #
  xmltable <- (
    XMLTABLE * '(' *
        c_expr * xmlexists_argument * COLUMNS * xmltable_column_list * ')' |
    XMLTABLE * '(' * XMLNAMESPACES * '(' * xml_namespace_list * ')' * ',' *
        c_expr * xmlexists_argument * COLUMNS * xmltable_column_list * ')'
  )

  xmltable_column_list <- (
    xmltable_column_el |
    xmltable_column_list * ',' * xmltable_column_el
  )

  xmltable_column_el <- (
    ColId * Typename                             |
    ColId * Typename * xmltable_column_option_list |
    ColId * FOR * ORDINALITY
  )

  xmltable_column_option_list <- (
    xmltable_column_option_el |
    xmltable_column_option_list * xmltable_column_option_el
  )

  xmltable_column_option_el <- (
    IDENT * b_expr |
    DEFAULT * b_expr |
    NOT * NULL_P     |
    NULL_P
  )

  xml_namespace_list <- (
    xml_namespace_el |
    xml_namespace_list * ',' * xml_namespace_el
  )

  xml_namespace_el <- (
    b_expr * AS * ColLabel |
    DEFAULT * b_expr
  )

  #****************************************************************************
  #
  #   Type syntax
  #       SQL introduces a large amount of type-specific syntax.
  #       Define individual clauses to handle these cases, and use
  #        the generic case to handle regular type-extensible Postgres syntax.
  #       - thomas 1997-10-10
  #
  #****************************************************************************/

  Typename <- (
    SimpleTypename * opt_array_bounds       |
    SETOF * SimpleTypename * opt_array_bounds |

    # SQL standard syntax, currently only one-dimensional */ 
    SimpleTypename * ARRAY * '[' * Iconst * ']'         |
    SETOF * SimpleTypename * ARRAY * '[' * Iconst * ']' |
    SimpleTypename * ARRAY                              |
    SETOF * SimpleTypename * ARRAY
  )

  opt_array_bounds <- Optional(
    opt_array_bounds * '[' * ']' |
    opt_array_bounds * '[' * Iconst * ']'
  )

  SimpleTypename <- (
    GenericType                |
    Numeric                      |
    Bit                          |
    Character                    |
    ConstDatetime                |
    ConstInterval * opt_interval |
    ConstInterval * '(' * Iconst * ')'
  )

  # We have a separate ConstTypename to allow defaulting fixed-length
  # types such as CHAR() and BIT() to an unspecified length.
  # SQL9x requires that these default to a length of one, but this
  # makes no sense for constructs like CHAR 'hi' and BIT '0101',
  # where there is an obvious better choice to make.
  # Note that ConstInterval is not included here since it must
  # be pushed up higher in the rules to accommodate the postfix
  # options (e.g. INTERVAL '1' YEAR). Likewise, we have to handle
  # the generic-type-name case in AexprConst to avoid premature
  # reduce/reduce conflicts against function names.
  #
  ConstTypename <- (
    Numeric      |
    ConstBit       |
    ConstCharacter |
    ConstDatetime
  )

  #
  # GenericType covers all type names that don't have special syntax mandated
  # by the standard, including qualified names.  We also allow type modifiers.
  # To avoid parsing conflicts against function invocations, the modifiers
  # have to be shown as expr_list here, but parse analysis will only accept
  # constants for them.
  #
  GenericType <- (
    type_function_name * opt_type_modifiers |
    type_function_name * attrs * opt_type_modifiers
  )

  opt_type_modifiers <- Optional(
      '(' * expr_list * ')'
  )

  #
  # SQL numeric data types
  #
  Numeric <- (
    INT_P                        |
    INTEGER                        |
    SMALLINT                       |
    BIGINT                         |
    REAL                           |
    FLOAT_P * opt_float            |
    DOUBLE_P * PRECISION           |
    DECIMAL_P * opt_type_modifiers |
    DEC * opt_type_modifiers       |
    NUMERIC * opt_type_modifiers   |
    BOOLEAN_P
  )

  opt_float <- Optional(
      '(' * Iconst * ')'
  )

  #
  # SQL bit-field data types
  # The following implements BIT() and BIT VARYING().
  #
  Bit <- (
    BitWithLength |
    BitWithoutLength
  )

  # ConstBit is like Bit except "BIT" defaults to unspecified length */
  # See notes for ConstCharacter, which addresses same issue for "CHAR" */
  ConstBit <- (
    BitWithLength |
    BitWithoutLength
  )

  BitWithLength <- (
    BIT * opt_varying * '(' * expr_list * ')'
  )

  BitWithoutLength <- (
    BIT * opt_varying
  )


  #
  # SQL character data types
  # The following implements CHAR() and VARCHAR().
  #
  Character <- (
    CharacterWithLength |
    CharacterWithoutLength
  )

  ConstCharacter <- (
    CharacterWithLength |
    CharacterWithoutLength
  )

  CharacterWithLength <- (
    character * '(' * Iconst * ')'
  )

  CharacterWithoutLength <- (
    character
  )

  character <- (
    CHARACTER * opt_varying          |
    CHAR_P * opt_varying               |
    VARCHAR                            |
    NATIONAL * CHARACTER * opt_varying |
    NATIONAL * CHAR_P * opt_varying    |
    NCHAR * opt_varying
  )

  opt_varying <- Optional(
    VARYING
  )

  #
  # SQL date/time types
  #
  ConstDatetime <- (
    TIMESTAMP * '(' * Iconst * ')' * opt_timezone |
    TIMESTAMP * opt_timezone                        |
    TIME * '(' * Iconst * ')' * opt_timezone        |
    TIME * opt_timezone
  )

  ConstInterval <- (
    INTERVAL
  )

  opt_timezone <- Optional(
    WITH_LA * TIME * ZONE |
    WITHOUT * TIME * ZONE
  )

  opt_interval <- Optional(
              YEAR_P              |
    MONTH_P                       |
    DAY_P                         |
    HOUR_P                        |
    MINUTE_P                      |
    interval_second               |
    YEAR_P * TO * MONTH_P         |
    DAY_P * TO * HOUR_P           |
    DAY_P * TO * MINUTE_P         |
    DAY_P * TO * interval_second  |
    HOUR_P * TO * MINUTE_P        |
    HOUR_P * TO * interval_second |
    MINUTE_P * TO * interval_second
  )

  interval_second <- (
    SECOND_P |
    SECOND_P * '(' * Iconst * ')'
  )


  #****************************************************************************
  #
  #   expression grammar
  #
  #****************************************************************************/

  #
  # General expressions
  # This is the heart of the expression syntax.
  #
  # We have two expression types: a_expr is the unrestricted kind, and
  # b_expr is a subset that must be used in some places to avoid shift/reduce
  # conflicts.  For example, we can't do BETWEEN as "BETWEEN a_expr AND a_expr"
  # because that use of AND conflicts with AND as a boolean operator.  So,
  # b_expr is used in BETWEEN and we remove boolean keywords from b_expr.
  #
  # Note that '(' a_expr ')' is a b_expr, so an unrestricted expression can
  # always be used by surrounding it with parens.
  #
  # c_expr is all the productions that are common to a_expr and b_expr;
  # it's factored out just to eliminate redundant coding.
  #
  # Be careful of productions involving more than one terminal token.
  # By default, bison will assign such productions the precedence of their
  # last terminal, but in nearly all cases you want it to be the precedence
  # of the first terminal instead; otherwise you will not get the behavior
  # you expect!  So we use %prec annotations freely to set precedences.
  #
  a_expr <- (
    c_expr                     |
    a_expr * TYPECAST * Typename |
    a_expr * COLLATE * any_name  |
    a_expr * AT * TIME * ZONE * a_expr             | # %prec * AT

    # 
    # These operators must be called out explicitly in order to make use 
    # of bison's automatic operator-precedence handling.  All other 
    # operator names are handled by the generic productions using "Op", 
    # below; and all those operators will have the same precedence. 
    # 
    # If you add more explicitly-known operators, be sure to add them 
    # also to b_expr and to the MathOp list below. 
    # 
    '+' * a_expr                     | # %prec * UMINUS
    '-' * a_expr                     | # %prec * UMINUS
    a_expr * '+' * a_expr            |
    a_expr * '-' * a_expr            |
    a_expr * '*' * a_expr            |
    a_expr * '/' * a_expr            |
    a_expr * '%' * a_expr            |
    a_expr * '^' * a_expr            |
    a_expr * '<' * a_expr            |
    a_expr * '>' * a_expr            |
    a_expr * '=' * a_expr            |
    a_expr * LESS_EQUALS * a_expr    |
    a_expr * GREATER_EQUALS * a_expr |
    a_expr * NOT_EQUALS * a_expr     |
    a_expr * qual_Op * a_expr              | # %prec * Op
    qual_Op * a_expr                     | # %prec * Op
    a_expr * AND * a_expr |
    a_expr * OR * a_expr  |
    NOT * a_expr          |
    NOT_LA * a_expr                      | # %prec * NOT
    a_expr * LIKE * a_expr |
    a_expr * LIKE * a_expr * ESCAPE * a_expr                   | # %prec * LIKE
    a_expr * NOT_LA * LIKE * a_expr                          | # %prec * NOT_LA
    a_expr * NOT_LA * LIKE * a_expr * ESCAPE * a_expr            | # %prec * NOT_LA
    a_expr * ILIKE * a_expr |
    a_expr * ILIKE * a_expr * ESCAPE * a_expr                  | # %prec * ILIKE
    a_expr * NOT_LA * ILIKE * a_expr                         | # %prec * NOT_LA
    a_expr * NOT_LA * ILIKE * a_expr * ESCAPE * a_expr           | # %prec * NOT_LA
    a_expr * SIMILAR * TO * a_expr                           | # %prec * SIMILAR
    a_expr * SIMILAR * TO * a_expr * ESCAPE * a_expr             | # %prec * SIMILAR
    a_expr * NOT_LA * SIMILAR * TO * a_expr                    | # %prec * NOT_LA
    a_expr * NOT_LA * SIMILAR * TO * a_expr * ESCAPE * a_expr      | # %prec * NOT_LA


    # NullTest clause 
    # Define SQL-style Null test clause. 
    # Allow two forms described in the standard: 
    #  a IS NULL 
    #  a IS NOT NULL 
    # Allow two SQL extensions 
    #  a ISNULL 
    #  a NOTNULL 
    # 
    a_expr * IS * NULL_P                           | # %prec * IS
    a_expr * ISNULL |
    a_expr * IS * NOT * NULL_P                       | # %prec * IS
    a_expr * NOTNULL     |
    row * OVERLAPS * row |
    a_expr * IS * TRUE_P                           | # %prec * IS
    a_expr * IS * NOT * TRUE_P                       | # %prec * IS
    a_expr * IS * FALSE_P                          | # %prec * IS
    a_expr * IS * NOT * FALSE_P                      | # %prec * IS
    a_expr * IS * UNKNOWN                          | # %prec * IS
    a_expr * IS * NOT * UNKNOWN                      | # %prec * IS
    a_expr * IS * DISTINCT * FROM * a_expr             | # %prec * IS
    a_expr * IS * NOT * DISTINCT * FROM * a_expr         | # %prec * IS
    a_expr * BETWEEN * opt_asymmetric * b_expr * AND * a_expr        | # %prec * BETWEEN
    a_expr * NOT_LA * BETWEEN * opt_asymmetric * b_expr * AND * a_expr  | # %prec * NOT_LA
    a_expr * BETWEEN * SYMMETRIC * b_expr * AND * a_expr             | # %prec * BETWEEN
    a_expr * NOT_LA * BETWEEN * SYMMETRIC * b_expr * AND * a_expr      | # %prec * NOT_LA
    a_expr * IN_P * in_expr |
    a_expr * subquery_Op * sub_type * select_with_parens     | # %prec * Op
    a_expr * subquery_Op * sub_type * '(' * a_expr * ')'         | # %prec * Op
    UNIQUE * select_with_parens |
    a_expr * IS * DOCUMENT_P                   | # %prec * IS
    a_expr * IS * NOT * DOCUMENT_P               | # %prec * IS
    a_expr * IS * NORMALIZED                               | # %prec * IS
    a_expr * IS * unicode_normal_form * NORMALIZED           | # %prec * IS
    a_expr * IS * NOT * NORMALIZED                           | # %prec * IS
    a_expr * IS * NOT * unicode_normal_form * NORMALIZED       | # %prec * IS
    DEFAULT
  )

  #
  # Restricted expressions
  #
  # b_expr is a subset of the complete expression syntax defined by a_expr.
  #
  # Presently, AND, NOT, IS, and IN are the a_expr keywords that would
  # cause trouble in the places where b_expr is used.  For simplicity, we
  # just eliminate all the boolean-keyword-operator productions from b_expr.
  #
  b_expr <- (
    c_expr                     |
    b_expr * TYPECAST * Typename |
    '+' * b_expr                     | # %prec * UMINUS
    '-' * b_expr                     | # %prec * UMINUS
    b_expr * '+' * b_expr            |
    b_expr * '-' * b_expr            |
    b_expr * '*' * b_expr            |
    b_expr * '/' * b_expr            |
    b_expr * '%' * b_expr            |
    b_expr * '^' * b_expr            |
    b_expr * '<' * b_expr            |
    b_expr * '>' * b_expr            |
    b_expr * '=' * b_expr            |
    b_expr * LESS_EQUALS * b_expr    |
    b_expr * GREATER_EQUALS * b_expr |
    b_expr * NOT_EQUALS * b_expr     |
    b_expr * qual_Op * b_expr              | # %prec * Op
    qual_Op * b_expr                     | # %prec * Op
    b_expr * IS * DISTINCT * FROM * b_expr         | # %prec * IS
    b_expr * IS * NOT * DISTINCT * FROM * b_expr     | # %prec * IS
    b_expr * IS * DOCUMENT_P                   | # %prec * IS
    b_expr * IS * NOT * DOCUMENT_P             # %prec * IS
  )

  #
  # Productions that can be used in both a_expr and b_expr.
  #
  # Note: productions that refer recursively to a_expr or b_expr mostly
  # cannot appear here.   However, it's OK to refer to a_exprs that occur
  # inside parentheses, such as function arguments; that cannot introduce
  # ambiguity to the b_expr syntax.
  #
  c_expr <- (
    columnref                          |
    AexprConst                           |
    PARAM * opt_indirection              |
    '(' * a_expr * ')' * opt_indirection |
    case_expr                            |
    func_expr                            |
    select_with_parens             | # %prec * UMINUS
    select_with_parens * indirection |
    EXISTS * select_with_parens      |
    ARRAY * select_with_parens       |
    ARRAY * array_expr               |
    explicit_row                     |
    implicit_row                     |
    GROUPING * '(' * expr_list * ')'
  )

  func_application <- (
    func_name * '(' * ')'                                                                    |
    func_name * '(' * func_arg_list * opt_sort_clause * ')'                                  |
    func_name * '(' * VARIADIC * func_arg_expr * opt_sort_clause * ')'                       |
    func_name * '(' * func_arg_list * ',' * VARIADIC * func_arg_expr * opt_sort_clause * ')' |
    func_name * '(' * ALL * func_arg_list * opt_sort_clause * ')'                            |
    func_name * '(' * DISTINCT * func_arg_list * opt_sort_clause * ')'                       |
    func_name * '(' * '*' * ')'
  )


  #
  # func_expr and its cousin func_expr_windowless are split out from c_expr just
  # so that we have classifications for "everything that is a function call or
  # looks like one".  This isn't very important, but it saves us having to
  # document which variants are legal in places like "FROM function()" or the
  # backwards-compatible functional-index syntax for CREATE INDEX.
  # (Note that many of the special SQL functions wouldn't actually make any
  # sense as functional index entries, but we ignore that consideration here.)
  #
  func_expr <- (
    func_application * within_group_clause * filter_clause * over_clause |
    func_expr_common_subexpr
  )

  #
  # As func_expr but does not accept WINDOW functions directly
  # (but they can still be contained in arguments for functions etc).
  # Use this when window expressions are not allowed, where needed to
  # disambiguate the grammar (e.g. in CREATE INDEX).
  #
  func_expr_windowless <- (
    func_application |
    func_expr_common_subexpr
  )

  #
  # Special expressions that are considered to be functions.
  #
  func_expr_common_subexpr <- (
    COLLATION * FOR * '(' * a_expr * ')'                                              |
    CURRENT_DATE                                                                        |
    CURRENT_TIME                                                                        |
    CURRENT_TIME * '(' * Iconst * ')'                                                   |
    CURRENT_TIMESTAMP                                                                   |
    CURRENT_TIMESTAMP * '(' * Iconst * ')'                                              |
    LOCALTIME                                                                           |
    LOCALTIME * '(' * Iconst * ')'                                                      |
    LOCALTIMESTAMP                                                                      |
    LOCALTIMESTAMP * '(' * Iconst * ')'                                                 |
    CURRENT_ROLE                                                                        |
    CURRENT_USER                                                                        |
    SESSION_USER                                                                        |
    USER                                                                                |
    CURRENT_CATALOG                                                                     |
    CURRENT_SCHEMA                                                                      |
    CAST * '(' * a_expr * AS * Typename * ')'                                           |
    EXTRACT * '(' * extract_list * ')'                                                  |
    NORMALIZE * '(' * a_expr * ')'                                                      |
    NORMALIZE * '(' * a_expr * ',' * unicode_normal_form * ')'                          |
    OVERLAY * '(' * overlay_list * ')'                                                  |
    OVERLAY * '(' * func_arg_list_opt * ')'                                             |
    POSITION * '(' * position_list * ')'                                                |
    SUBSTRING * '(' * substr_list * ')'                                                 |
    SUBSTRING * '(' * func_arg_list_opt * ')'                                           |
    TREAT * '(' * a_expr * AS * Typename * ')'                                          |
    TRIM * '(' * BOTH * trim_list * ')'                                                 |
    TRIM * '(' * LEADING * trim_list * ')'                                              |
    TRIM * '(' * TRAILING * trim_list * ')'                                             |
    TRIM * '(' * trim_list * ')'                                                        |
    NULLIF * '(' * a_expr * ',' * a_expr * ')'                                          |
    COALESCE * '(' * expr_list * ')'                                                    |
    GREATEST * '(' * expr_list * ')'                                                    |
    LEAST * '(' * expr_list * ')'                                                       |
    XMLCONCAT * '(' * expr_list * ')'                                                   |
    XMLELEMENT * '(' * NAME_P * ColLabel * ')'                                          |
    XMLELEMENT * '(' * NAME_P * ColLabel * ',' * xml_attributes * ')'                   |
    XMLELEMENT * '(' * NAME_P * ColLabel * ',' * expr_list * ')'                        |
    XMLELEMENT * '(' * NAME_P * ColLabel * ',' * xml_attributes * ',' * expr_list * ')' |
    XMLEXISTS * '(' * c_expr * xmlexists_argument * ')'                                 |
    XMLFOREST * '(' * xml_attribute_list * ')'                                          |
    XMLPARSE * '(' * document_or_content * a_expr * xml_whitespace_option * ')'         |
    XMLPI * '(' * NAME_P * ColLabel * ')'                                               |
    XMLPI * '(' * NAME_P * ColLabel * ',' * a_expr * ')'                                |
    XMLROOT * '(' * a_expr * ',' * xml_root_version * opt_xml_root_standalone * ')'     |
    XMLSERIALIZE * '(' * document_or_content * a_expr * AS * SimpleTypename * ')'
  )

  #
  # SQL/XML support
  #
  xml_root_version <- (
    VERSION_P * a_expr |
    VERSION_P * NO * VALUE_P
  )

  opt_xml_root_standalone <- Optional(
      ',' * STANDALONE_P * YES_P |
    ',' * STANDALONE_P * NO      |
    ',' * STANDALONE_P * NO * VALUE_P
  )

  xml_attributes <- (
    XMLATTRIBUTES * '(' * xml_attribute_list * ')'
  )

  xml_attribute_list <- (
    xml_attribute_el |
    xml_attribute_list * ',' * xml_attribute_el
  )

  xml_attribute_el <- (
    a_expr * AS * ColLabel |
    a_expr
  )

  document_or_content <- (
    DOCUMENT_P |
    CONTENT_P
  )

  xml_whitespace_option <- Optional(
    PRESERVE * WHITESPACE_P |
    STRIP_P * WHITESPACE_P
  )

  # We allow several variants for SQL and other compatibility. */
  xmlexists_argument <- (
    PASSING * c_expr                  |
    PASSING * c_expr * xml_passing_mech |
    PASSING * xml_passing_mech * c_expr |
    PASSING * xml_passing_mech * c_expr * xml_passing_mech
  )

  xml_passing_mech <- (
    BY * REF |
    BY * VALUE_P
  )


  #
  # Aggregate decoration clauses
  #
  within_group_clause <- Optional(
    WITHIN * GROUP_P * '(' * sort_clause * ')'
  )

  filter_clause <- Optional(
    FILTER * '(' * WHERE * a_expr * ')'
  )


  #
  # Window Definitions
  #
  window_clause <- Optional(
    WINDOW * window_definition_list
  )

  window_definition_list <- (
    window_definition |
    window_definition_list * ',' * window_definition
  )

  window_definition <- (
    ColId * AS * window_specification
  )

  over_clause <- Optional(
    OVER * window_specification |
    OVER * ColId
  )

  window_specification <- (
      '(' * opt_existing_window_name * opt_partition_clause *
        opt_sort_clause * opt_frame_clause * ')'
  )

  #
  # If we see PARTITION, RANGE, ROWS or GROUPS as the first token after the '('
  # of a window_specification, we want the assumption to be that there is
  # no existing_window_name; but those keywords are unreserved and so could
  # be ColIds.  We fix this by making them have the same precedence as IDENT
  # and giving the empty production here a slightly higher precedence, so
  # that the shift/reduce conflict is resolved in favor of reducing the rule.
  # These keywords are thus precluded from being an existing_window_name but
  # are not reserved for any other purpose.
  #
  opt_existing_window_name <- Optional(
    ColId            # %prec * Op
  )

  opt_partition_clause <- Optional(
    PARTITION * BY * expr_list
  )

  #
  # For frame clauses, we return a WindowDef, but only some fields are used:
  # frameOptions, startOffset, and endOffset.
  #
  opt_frame_clause <- Optional(
    RANGE * frame_extent * opt_window_exclusion_clause |
    ROWS * frame_extent * opt_window_exclusion_clause    |
    GROUPS * frame_extent * opt_window_exclusion_clause
  )

  frame_extent <- (
    frame_bound |
    BETWEEN * frame_bound * AND * frame_bound
  )

  #
  # This is used for both frame start and frame end, with output set up on
  # the assumption it's frame start; the frame_extent productions must reject
  # invalid cases.
  #
  frame_bound <- (
    UNBOUNDED * PRECEDING |
    UNBOUNDED * FOLLOWING   |
    CURRENT_P * ROW         |
    a_expr * PRECEDING      |
    a_expr * FOLLOWING
  )

  opt_window_exclusion_clause <- Optional(
    EXCLUDE * CURRENT_P * ROW |
    EXCLUDE * GROUP_P           |
    EXCLUDE * TIES              |
    EXCLUDE * NO * OTHERS
  )


  #
  # Supporting nonterminals for expressions.
  #

  # Explicit row production.
  #
  # SQL99 allows an optional ROW keyword, so we can now do single-element rows
  # without conflicting with the parenthesized a_expr production.  Without the
  # ROW keyword, there must be more than one a_expr inside the parens.
  #
  row <- (
    ROW * '(' * expr_list * ')' |
    ROW * '(' * ')'               |
    '(' * expr_list * ',' * a_expr * ')'
  )

  explicit_row <- (
    ROW * '(' * expr_list * ')' |
    ROW * '(' * ')'
  )

  implicit_row <- (
      '(' * expr_list * ',' * a_expr * ')'
  )

  sub_type <- (
    ANY |
    SOME  |
    ALL
  )

  all_Op <- (
    Op |
    MathOp
  )

  MathOp <- (
      '+'          |
    '-'            |
    '*'            |
    '/'            |
    '%'            |
    '^'            |
    '<'            |
    '>'            |
    '='            |
    LESS_EQUALS    |
    GREATER_EQUALS |
    NOT_EQUALS
  )

  qual_Op <- (
    Op |
    OPERATOR * '(' * any_operator * ')'
  )

  qual_all_Op <- (
    all_Op |
    OPERATOR * '(' * any_operator * ')'
  )

  subquery_Op <- (
    all_Op                            |
    OPERATOR * '(' * any_operator * ')' |
    LIKE                                |
    NOT_LA * LIKE                       |
    ILIKE                               |
    NOT_LA * ILIKE

  # cannot put SIMILAR TO here, because SIMILAR TO is a hack.
  # the regular expression is preprocessed by a function (similar_to_escape),
  # and the ~ operator for posix regular expressions is used.
  #        x SIMILAR TO y     <-    x ~ similar_to_escape(y)
  # this transformation is made on the fly by the parser upwards.
  # however the SubLink structure which handles any/some/all stuff
  # is not ready for such a thing.
  #
  )

  expr_list <- (
    a_expr |
    expr_list * ',' * a_expr
  )

  # function arguments can have names */
  func_arg_list <- (
    func_arg_expr |
    func_arg_list * ',' * func_arg_expr
  )

  func_arg_expr <- (
    a_expr                           |
    param_name * COLON_EQUALS * a_expr |
    param_name * EQUALS_GREATER * a_expr
  )

  func_arg_list_opt <- Optional(
    func_arg_list
  )

  type_list <- (
    Typename |
    type_list * ',' * Typename
  )

  array_expr <- (
      '[' * expr_list * ']'     |
    '[' * array_expr_list * ']' |
    '[' * ']'
  )

  array_expr_list <- (
    array_expr |
    array_expr_list * ',' * array_expr
  )


  extract_list <- (
    extract_arg * FROM * a_expr
  )

  # Allow delimited string Sconst in extract_arg as an SQL extension.
  # - thomas 2001-04-12
  #
  extract_arg <- (
    IDENT  |
    YEAR_P   |
    MONTH_P  |
    DAY_P    |
    HOUR_P   |
    MINUTE_P |
    SECOND_P |
    Sconst
  )

  unicode_normal_form <- (
    NFC |
    NFD   |
    NFKC  |
    NFKD
  )

  # OVERLAY() arguments */
  overlay_list <- (
    a_expr * PLACING * a_expr * FROM * a_expr * FOR * a_expr |
    a_expr * PLACING * a_expr * FROM * a_expr
  )

  # position_list uses b_expr not a_expr to avoid conflict with general IN */
  position_list <- (
    b_expr * IN_P * b_expr
  )

  #
  # SUBSTRING() arguments
  #
  # Note that SQL:1999 has both
  #     text FROM int FOR int
  # and
  #     text FROM pattern FOR escape
  #
  # In the parser we map them both to a call to the substring() function and
  # rely on type resolution to pick the right one.
  #
  # In SQL:2003, the second variant was changed to
  #     text SIMILAR pattern ESCAPE escape
  # We could in theory map that to a different function internally, but
  # since we still support the SQL:1999 version, we don't.  However,
  # ruleutils.c will reverse-list the call in the newer style.
  #
  substr_list <- (
    a_expr * FROM * a_expr * FOR * a_expr |
    a_expr * FOR * a_expr * FROM * a_expr   |
    a_expr * FROM * a_expr                  |
    a_expr * FOR * a_expr                   |
    a_expr * SIMILAR * a_expr * ESCAPE * a_expr
  )

  trim_list <- (
    a_expr * FROM * expr_list |
    FROM * expr_list            |
    expr_list
  )

  in_expr <- (
    select_with_parens |
    '(' * expr_list * ')'
  )

  #
  # Define SQL-style CASE clause.
  # - Full specification
  #   CASE WHEN a = b THEN c ... ELSE d END
  # - Implicit argument
  #   CASE a WHEN b THEN c ... ELSE d END
  #
  case_expr <- (
    CASE * case_arg * when_clause_list * case_default * END_P
  )

  when_clause_list <- (
    # There must be at least one */
      when_clause |
    when_clause_list * when_clause
  )

  when_clause <- (
    WHEN * a_expr * THEN * a_expr
  )

  case_default <- Optional(
    ELSE * a_expr
  )

  case_arg <- Optional(
    a_expr
  )

  columnref <- (
    ColId |
    ColId * indirection
  )

  indirection_el <- (
      '.' * attr_name  |
    '.' * '*'          |
    '[' * a_expr * ']' |
    '[' * opt_slice_bound * ':' * opt_slice_bound * ']'
  )

  opt_slice_bound <- Optional(
    a_expr
  )

  indirection <- (
    indirection_el |
    indirection * indirection_el
  )

  opt_indirection <- Optional(
    opt_indirection * indirection_el
  )

  opt_asymmetric <- Optional(
    ASYMMETRIC
  )


  #****************************************************************************
  #
  #   target list for SELECT
  #
  #****************************************************************************/

  opt_target_list <- Optional(
    target_list
  )

  target_list <- (
    target_el |
    target_list * ',' * target_el
  )

  target_el <- (
    a_expr * AS * ColLabel |
    a_expr * BareColLabel    |
    a_expr                   |
    '*'
  )


  #****************************************************************************
  #
  #   Names and constants
  #
  #****************************************************************************/

  qualified_name_list <- (
    qualified_name |
    qualified_name_list * ',' * qualified_name
  )

  #
  # The production for a qualified relation name has to exactly match the
  # production for a qualified func_name, because in a FROM clause we cannot
  # tell which we are parsing until we see what comes after it ('(' for a
  # func_name, something else for a relation). Therefore we allow 'indirection'
  # which may contain subscripts, and reject that case in the C code.
  #
  qualified_name <- (
    ColId |
    ColId * indirection
  )

  name_list <- (
    name |
    name_list * ',' * name
  )


  name <- ColId

  attr_name <- ColLabel

  file_name <- Sconst

  #
  # The production for a qualified func_name has to exactly match the
  # production for a qualified columnref, because we cannot tell which we
  # are parsing until we see what comes after it ('(' or Sconst for a func_name,
  # anything else for a columnref).  Therefore we allow 'indirection' which
  # may contain subscripts, and reject that case in the C code.  (If we
  # ever implement SQL99-like methods, such syntax may actually become legal!)
  #
  func_name <- (
    type_function_name |
    ColId * indirection
  )


  #
  # Constants
  #
  AexprConst <- (
    Iconst                                                         |
    FCONST                                                           |
    Sconst                                                           |
    BCONST                                                           |
    XCONST                                                           |
    func_name * Sconst                                               |
    func_name * '(' * func_arg_list * opt_sort_clause * ')' * Sconst |
    ConstTypename * Sconst                                           |
    ConstInterval * Sconst * opt_interval                            |
    ConstInterval * '(' * Iconst * ')' * Sconst                      |
    TRUE_P                                                           |
    FALSE_P                                                          |
    NULL_P
  )

  Iconst <- ICONST
  Sconst <- SCONST

  SignedIconst <- (
    Iconst     |
    '+' * Iconst |
    '-' * Iconst
  )

  # Role specifications */
  RoleId <- (
    RoleSpec
  )

  RoleSpec <- (
    NonReservedWord |
    CURRENT_ROLE    |
    CURRENT_USER    |
    SESSION_USER
  )

  role_list <- (
    RoleSpec |
    role_list * ',' * RoleSpec
  )


  #****************************************************************************
  #
  # PL/pgSQL extensions
  #
  # You'd think a PL/pgSQL "expression" should be just an a_expr, but
  # historically it can include just about anything that can follow SELECT.
  # Therefore the returned struct is a SelectStmt.
  #****************************************************************************/

  PLpgSQL_Expr <- (
    opt_distinct_clause * opt_target_list *
        from_clause * where_clause *
        group_clause * having_clause * window_clause *
        opt_sort_clause * opt_select_limit * opt_for_locking_clause
  )

  #
  # PL/pgSQL Assignment statement: name opt_indirection := PLpgSQL_Expr
  #

  PLAssignStmt <- (
    plassign_target * opt_indirection * plassign_equals * PLpgSQL_Expr
  )

  plassign_target <- (
    ColId |
    PARAM
  )

  plassign_equals <- (
    COLON_EQUALS |
    '='
  )


  #
  # Name classification hierarchy.
  #
  # IDENT is the lexeme returned by the lexer for identifiers that match
  # no known keyword.  In most cases, we can accept certain keywords as
  # names, not only IDENTs.   We prefer to accept as many such keywords
  # as possible to minimize the impact of "reserved words" on programmers.
  # So, we divide names into several possible classes.  The classification
  # is chosen in part to make keywords acceptable as names wherever possible.
  #

  # Column identifier --- names that can be column, table, etc names.
  #
  ColId <- (
    IDENT              |
    unreserved_keyword |
    col_name_keyword
  )

  # Type/function identifier --- names that can be type or function names.
  #
  type_function_name <- (
    IDENT                  |
    unreserved_keyword     |
    type_func_name_keyword
  )

  # Any not-fully-reserved word --- these names can be, eg, role names.
  #
  NonReservedWord <- (
    IDENT                  |
    unreserved_keyword     |
    col_name_keyword       |
    type_func_name_keyword
  )

  # Column label --- allowed labels in "AS" clauses.
  # This presently includes *all* Postgres keywords.
  #
  ColLabel <- (
    IDENT                  |
    unreserved_keyword     |
    col_name_keyword       |
    type_func_name_keyword |
    reserved_keyword
  )

  # Bare column label --- names that can be column labels without writing "AS".
  # This classification is orthogonal to the other keyword categories.
  #
  BareColLabel <- (
    IDENT              |
    bare_label_keyword 
  )



