CLASS zcl_file_table DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS convert_external_to_internal
      IMPORTING external_data TYPE ANY TABLE
      EXPORTING internal_data TYPE ANY TABLE.
ENDCLASS.


CLASS zcl_file_table IMPLEMENTATION.
  METHOD convert_external_to_internal.
    DATA(ex_2_in) = NEW zcl_file_table_ex_2_in( ).
    ex_2_in->convert( EXPORTING external_data = external_data
                      IMPORTING internal_data = internal_data ).
  ENDMETHOD.
ENDCLASS.
