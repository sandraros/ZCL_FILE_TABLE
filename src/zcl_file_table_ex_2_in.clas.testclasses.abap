*"* use this source file for your ABAP unit test classes

CLASS ltc_app DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS first_test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_app IMPLEMENTATION.
  METHOD first_test.
    TYPES:
      BEGIN OF ty_line_in_external_format,
        price    TYPE string,
        currency TYPE string,
      END OF ty_line_in_external_format.
    TYPES ty_table_in_external_format TYPE STANDARD TABLE OF ty_line_in_external_format WITH DEFAULT KEY.
    TYPES:
      BEGIN OF ty_line_in_internal_format,
        price    TYPE sflight-price,
        currency TYPE sflight-currency,
      END OF ty_line_in_internal_format.
    TYPES ty_table_in_internal_format TYPE STANDARD TABLE OF ty_line_in_internal_format WITH DEFAULT KEY.

    DATA(ex_2_in) = NEW zcl_file_table_ex_2_in( ).

    DATA(table_in_external_format) = VALUE ty_table_in_external_format( ( price = '20' currency = 'COP' ) ).
    DATA(table_in_internal_format) = VALUE ty_table_in_internal_format( ).
    ex_2_in->convert( EXPORTING external_data = table_in_external_format
                      IMPORTING internal_data = table_in_internal_format ).
    cl_abap_unit_assert=>assert_equals( act = table_in_internal_format
                                        exp = VALUE ty_table_in_internal_format( ( price = '0.2' currency = 'COP' ) ) ).
  ENDMETHOD.
ENDCLASS.
