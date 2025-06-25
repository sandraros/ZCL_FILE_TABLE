*"* use this source file for your ABAP unit test classes
class ltc_app definition final for testing
  duration short
  risk level harmless.

  private section.
    methods:
      first_test for testing raising cx_static_check.
endclass.


class ltc_app implementation.

  method first_test.
    TYPES:
      BEGIN OF ts_flight,
        carrid     TYPE string,
        connid     TYPE string,
        fldate     TYPE string,
        price      TYPE string,
        currency   TYPE string,
        planetype  TYPE string,
        seatsmax   TYPE string,
        seatsocc   TYPE string,
        paymentsum TYPE string,
        seatsmax_b TYPE string,
        seatsocc_b TYPE string,
        seatsmax_f TYPE string,
        seatsocc_f TYPE string,
      END OF ts_flight.
    TYPES tt_flight TYPE STANDARD TABLE OF ts_flight WITH DEFAULT KEY.

    DATA(source_values) = VALUE tt_flight( ( carrid     = 'AA'
                                             connid     = '0017'
                                             fldate     = '20171219'
                                             price      = '422.94 '
                                             currency   = 'USD'
                                             planetype  = '747-400'
                                             seatsmax   = '385'
                                             seatsocc   = '371'
                                             paymentsum = '191334.22 '
                                             seatsmax_b = '31'
                                             seatsocc_b = '28'
                                             seatsmax_f = '21'
                                             seatsocc_f = '21' ) ).
    types tt_table_demo type standard table of zfile_table_demo with DEFAULT KEY.
    data(target_values) = value tt_table_demo( ).
    zcl_file_table=>convert_external_to_internal( exporting external_data = source_values
*                                             target_table_name = 'SFLIGHT'
                                                         importing internal_data = target_values ).

    cl_abap_unit_assert=>fail( 'Implement your first test here' ).
  endmethod.

endclass.
