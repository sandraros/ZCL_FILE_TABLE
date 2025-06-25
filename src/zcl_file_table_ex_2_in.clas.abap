CLASS zcl_file_table_ex_2_in DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_mapped_field,
        field_of_external_data TYPE string,
        field_of_internal_data TYPE string,
      END OF ty_mapped_field.
    TYPES ty_mapped_fields TYPE STANDARD TABLE OF ty_mapped_field WITH DEFAULT KEY.

    METHODS convert
      IMPORTING external_data        TYPE ANY TABLE
                VALUE(mapped_fields) TYPE ty_mapped_fields OPTIONAL
      EXPORTING internal_data        TYPE ANY TABLE.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_field_type,
        fieldname TYPE fieldname,
        component type abap_compdescr,
        rtts_of_component type ref to cl_abap_typedescr,
        " Table name of table-based field type
        typetable TYPE tabname,
        " Field name of table-based field type
        typefield TYPE fieldname,
        " RTTI of table-based field type
        type_rtts  type ref to cl_abap_typedescr,
        " Table name of currency or quantity field if field is type CURR or QUAN
        reftable  TYPE tabname,
        " Field name of currency or quantity field if field is type CURR or QUAN
        reffield  TYPE fieldname,
        datatype  TYPE datatype_d,
        refindex  TYPE sytabix,
      END OF ty_field_type.
    TYPES ty_field_types TYPE STANDARD TABLE OF ty_field_type WITH DEFAULT KEY.

    DATA rtts_of_external_data_lines TYPE REF TO cl_abap_datadescr.
    DATA log                         TYPE string_table.
    DATA fields_of_internal_data     TYPE ty_field_types.

  METHODs ANALYZE_table_fields
  IMPORTING table TYPE any table
  RETURNING VALUE(fields_of_internal_data) type ty_field_types.

    METHODS convert_structure
      IMPORTING line_of_external_data TYPE any
      EXPORTING line_of_internal_data TYPE any.
    METHODS get_mapping_by_name
      IMPORTING
        external_data    TYPE ANY TABLE
        internal_data    TYPE ANY TABLE
      RETURNING
        value(rt_result) TYPE ty_mapped_fields.
ENDCLASS.


CLASS zcl_file_table_ex_2_in IMPLEMENTATION.
  METHOD ANALYZE_tablE_fields.
    DATA(helpid) = VALUE string( ).
    " TODO: variable is assigned but never used (ABAP cleaner)

    DATA(rtts_of_table) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( table ) ).
    data(rtts_of_table_lines) = rtts_of_table->get_table_line_type( ).

    IF rtts_of_table_lines->kind <> cl_abap_typedescr=>kind_struct.
      RAISE EXCEPTION TYPE zcx_file_table_no_check
        EXPORTING
          text = 'Input parameter STRUCTURE of ZCL_FILE_TABLE_EX_2_IN=>ANALYZE_STRUCTURE_FIELDS must be a structure'.
    ENDIF.

    DATA(rtts_of_structure) = CAST cl_abap_structdescr( rtts_of_table_lines ).

data ref_to_data type ref to data.
create data ref_to_data like line of table.
assign ref_to_data->* to FIELD-SYMBOL(<structure>).

    fields_of_internal_data = VALUE ty_field_types( ).
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE <structure> TO FIELD-SYMBOL(<field>).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      DESCRIBE FIELD <field> HELP-ID helpid.

      DATA(field_of_internal_data) = VALUE ty_field_type( ).
      field_of_internal_data-component = rtts_of_structure->components[ sy-index ].
      field_of_internal_data-rtts_of_component = rtts_of_structure->get_component_type( field_of_internal_data-component-name ).

      IF helpid CS '-'.
        SPLIT helpid AT '-'
              INTO field_of_internal_data-typetable
                   field_of_internal_data-typefield.
        cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = field_of_internal_data-typetable
                                             RECEIVING  p_descr_ref    = field_of_internal_data-type_rtts
                                             EXCEPTIONS type_not_found = 1                " Type with name p_name could not be found
                                                        OTHERS         = 2 ).
        IF sy-subrc <> 0.
*         MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*           WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
        field_of_internal_data-type_rtts->get_ddic_object( RECEIVING  p_object     = DATA(ddic_object)
                                                           EXCEPTIONS not_found    = 1                " Type could not be found
                                                                      no_ddic_type = 2                " Typ is not a dictionary type
                                                                      OTHERS       = 3 ).
        DATA(ddic_field) = REF #( ddic_object[ fieldname = field_of_internal_data-typefield ] OPTIONAL ).
        IF ddic_field IS BOUND.
        field_of_internal_data-reftable = ddic_field->reftable.
        field_of_internal_data-reffield = ddic_field->reffield.
        field_of_internal_data-datatype = ddic_field->dtyp.
        field_of_internal_data-refindex = line_index( rtts_of_structure->components[ name = ddic_field->reffield ] ).
        endif.
      ELSE.
      ENDIF.
      INSERT field_of_internal_data INTO TABLE fields_of_internal_data.
*get_ddic_field_list
    ENDDO.
  ENDMETHOD.

  METHOD convert.
    TYPES ty_ref_to_data TYPE REF TO data.

    FIELD-SYMBOLS <line_of_internal_data> TYPE any.
    FIELD-SYMBOLS <line_of_external_data> TYPE any.

    " TODO: parameter INTERNAL_DATA is never cleared or assigned (ABAP cleaner)

    DATA(rtts_of_external_data) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( external_data ) ).
    rtts_of_external_data_lines = rtts_of_external_data->get_table_line_type( ).

    DATA(rtts_of_internal_data) = CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( internal_data ) ).
    DATA(rtts_of_internal_data_lines) = rtts_of_internal_data->get_table_line_type( ).
    DATA(ref_to_line_of_internal_data) = value TY_REF_TO_DATA( ).
    CREATE DATA ref_to_line_of_internal_data LIKE LINE OF internal_data.
    ASSIGN ref_to_line_of_internal_data->* TO <line_of_internal_data>.

    DATA(ref_to_line_of_external_data) = value TY_REF_TO_DATA( ).
    CREATE DATA ref_to_line_of_external_data LIKE LINE OF external_data.
    ASSIGN ref_to_line_of_external_data->* TO <line_of_external_data>.

    IF mapped_fields IS INITIAL.
      mapped_fields = get_mapping_by_name(
external_data = external_data
internal_data = internal_data ).
      CASE rtts_of_internal_data_lines->kind.
        WHEN rtts_of_internal_data_lines->kind_struct.
*        mapping-type_of_line_of_internal_data = analyze_structure_fields( structure = <line_of_internal_data> ).
*when rtts_of_external_data_lines->kind_table.
        WHEN OTHERS.
          " TODO
          " raise exception type zcx_file_table exporting text = ''(001).
      ENDCASE.
    ENDIF.

*    mapping-type_of_line_of_external_data = analyze_structure_fields( structure = <line_of_external_data> ).

    CASE rtts_of_external_data_lines->kind.
      WHEN rtts_of_external_data_lines->kind_struct.
*when rtts_of_external_data_lines->kind_table.
      WHEN OTHERS.
        " TODO
        " raise exception type zcx_file_table exporting text = ''(001).
    ENDCASE.

    LOOP AT external_data ASSIGNING <line_of_external_data>.
      CASE rtts_of_external_data_lines->kind.
        WHEN rtts_of_external_data_lines->kind_struct.
*          convert_structure( exporting line_of_external_data = <line_of_external_data>
*                                       mapping               = mapping
*                             importing line_of_internal_data = <line_of_internal_data> ).
*when rtts_of_external_data_lines->kind_table.
*  TODO: lines of table should all be STRING
      ENDCASE.
      INSERT <line_of_internal_data> INTO TABLE internal_data.
    ENDLOOP.
  ENDMETHOD.

  METHOD convert_structure.
    DO.
      ASSIGN COMPONENT sy-index OF STRUCTURE line_of_external_data TO FIELD-SYMBOL(<field_of_external_data>).
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      " LOOP AT EXTERNAL_data ASSIGNING FIELD-SYMBOL(<line_of_external_data>).
      "       § déterminer à quelle colonne de table ce champ correspond
      "       § vérifier que la longueur ne dépasse pas
      "       § si la colonne de table est de type curr, chercher la valeur du champ cuky
      "       § conversion externe vers interne via rs_conv_ex_2_in. transmettre la valeur du champ cuky au paramètre curr.
      "       § toute erreur est ajoutée à la log
      DATA(field_of_internal_data) = REF #( fields_of_internal_data[ sy-index ] ).
      DATA(currency) = VALUE waers( ).
      IF field_of_internal_data->typetable IS INITIAL.
        " todo
      ELSE.
        DATA ls_table_field TYPE tabfield.
        ls_table_field-tabname   = field_of_internal_data->typetable.
        ls_table_field-fieldname = field_of_internal_data->typefield.
        IF     field_of_internal_data->datatype  = 'CURR'
           AND field_of_internal_data->refindex <> 0.
          ASSIGN COMPONENT field_of_internal_data->refindex OF STRUCTURE line_of_external_data TO FIELD-SYMBOL(<reffield_of_external_data>).
          ASSERT sy-subrc = 0.
          currency = CONV #( <reffield_of_external_data> ).
        ENDIF.
      ENDIF.
*    1.234.567,89
*X   1,234,567.89
*Y   1 234 567,89
*      PERFORM set_number_format USING 'X'.
data lv_internal type string.
      CALL FUNCTION 'RS_CONV_EX_2_IN'
        EXPORTING
          input_external               = <field_of_external_data>
          table_field                  = ls_table_field
          currency                     = currency
          no_tcurx                     = 'X'            " 'X' -> if no entry in TCURX, 2 decimals
        IMPORTING
          output_internal              = lv_internal                 " Field contents in internal format
        EXCEPTIONS
          input_not_numerical          = 1                " Non-numeric input
          too_many_decimals            = 2                " Too many places after the decimal point
          more_than_one_sign           = 3                " +/- sign input multiple times
          ill_thousand_separator_dist  = 4                " Separation between thousands not = 0 mod 3
          too_many_digits              = 5                " Too many characters in whole number field
          sign_for_unsigned            = 6                " No +/- signs allowed
          too_large                    = 7                " Value too large
          too_small                    = 8                " Value too small
          invalid_date_format          = 9                " Invalid date format
          invalid_date                 = 10               " Invalid date
          invalid_time_format          = 11               " Invalid time format
          invalid_time                 = 12               " Invalid time
          invalid_hex_digit            = 13               " Incorrect hex character
          unexpected_error             = 14               " Unintercepted error
          invalid_fieldname            = 15               " Field not in Dictionary
          field_and_descr_incompatible = 16               " Output field does not match Dictionary description
          input_too_long               = 17               " Input longer than output length of output field
          no_decimals                  = 18               " Decimal separator without subsequent decimal places
          invalid_float                = 19               " Invalid floating point entry
          conversion_exit_error        = 20               " Error in conversion exit
          OTHERS                       = 21.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(error_message).
        INSERT error_message INTO TABLE log.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD get_mapping_by_name.
    DATA all_fields TYPE ty_field_types.

    DATA(fields_of_external_data) = analyze_table_fields( external_data ).
    DATA(fields_of_internal_data) = analyze_table_fields( internal_data ).

    INSERT LINES OF fields_of_external_data INTO TABLE all_fields.
    INSERT LINES OF fields_of_internal_data INTO TABLE all_fields.
    SORT all_fields BY fieldname.

    DATA(previous_fieldname) = VALUE string( ).
    LOOP AT all_fields REFERENCE INTO DATA(field).
      IF field->fieldname = previous_fieldname.
        " Two fields in the external and internal tables have the same name
        data mapped_field type ty_mapped_field.
        mapped_field-field_of_external_data = field->fieldname.
        mapped_field-field_of_external_data = field->fieldname.
        INSERT mapped_field INTO TABLE rt_result.
      ENDIF.
      previous_fieldname = field->fieldname.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
