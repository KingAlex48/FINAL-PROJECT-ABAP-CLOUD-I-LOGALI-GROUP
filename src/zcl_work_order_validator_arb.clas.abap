CLASS zcl_work_order_validator_arb DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      validate_create_order IMPORTING iv_customer_id   TYPE zde_customer_id_arb
                                      iv_technician_id TYPE zde_technician_id_arb
                                      iv_priority      TYPE zde_priority_code_arb
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,
      validate_update_order IMPORTING iv_status          TYPE zde_status_code_arb
                                      iv_priority        TYPE zde_priority_code_arb
                                      iv_status_original TYPE zde_status_code_arb
                            RETURNING VALUE(rv_valid)    TYPE abap_bool,
      validate_delete_order IMPORTING iv_work_order_id TYPE zde_work_orderid_arb
                                      iv_status        TYPE zde_status_code_arb
                            RETURNING VALUE(rv_valid)  TYPE abap_bool,
      validate_status_and_priority IMPORTING iv_status       TYPE zde_status_code_arb
                                             iv_priority     TYPE zde_priority_code_arb
                                   RETURNING VALUE(rv_valid) TYPE abap_bool,
      validate_authority  IMPORTING iv_work_order_id TYPE zde_work_orderid_arb
                                    iv_actvt         TYPE c
                          RETURNING VALUE(rv_valid)  TYPE abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS: c_valid_st_pe  TYPE c LENGTH 2 VALUE 'PE'.

ENDCLASS.



CLASS ZCL_WORK_ORDER_VALIDATOR_ARB IMPLEMENTATION.


  METHOD validate_authority.

    AUTHORITY-CHECK OBJECT 'ZAO_WO'
    ID 'ZAF_WO' FIELD iv_work_order_id
    ID 'ACTVT'  FIELD iv_actvt.

    IF sy-subrc EQ 0.
      rv_valid = abap_true.
      EXIT.
    ELSE.
      rv_valid = abap_false.
      EXIT.
    ENDIF.

  ENDMETHOD.


  METHOD validate_create_order.

    rv_valid = abap_true.

    SELECT SINGLE FROM ztarb_customer
           FIELDS @abap_true
           WHERE customer_id = @iv_customer_id
           INTO @DATA(lv_cust_valid).

    IF lv_cust_valid <> abap_true.
      rv_valid = abap_false.
      EXIT.
    ENDIF.

    SELECT SINGLE FROM ztarb_technician
              FIELDS @abap_true
              WHERE technician_id = @iv_technician_id
              INTO @DATA(lv_tech_valid).

    IF lv_tech_valid <> abap_true.
      rv_valid = abap_false.
      EXIT.
    ENDIF.

    IF NOT me->validate_status_and_priority( iv_status   = ' '
                                             iv_priority = iv_priority ).
      rv_valid = abap_false.
      EXIT.
    ENDIF.



  ENDMETHOD.


  METHOD validate_delete_order.

    rv_valid = abap_true.

    SELECT SINGLE FROM ztarb_wo_hist
           FIELDS @abap_true
           WHERE work_order_id = @iv_work_order_id
           AND change_description NE 'ACTUALIZADO'
           INTO @DATA(lv_wo_exists).

    IF iv_status <> c_valid_st_pe OR lv_wo_exists <> abap_true.
      rv_valid = abap_false.
      EXIT.
    ENDIF.

  ENDMETHOD.


  METHOD validate_status_and_priority.

    rv_valid = abap_true.

    IF iv_status IS NOT INITIAL
    AND iv_status <> ''.
      SELECT SINGLE FROM ztarb_status
             FIELDS @abap_true
             WHERE status_code = @iv_status
             INTO @DATA(lv_stat_valid).

      IF lv_stat_valid <> abap_true.
        rv_valid = abap_false.
        EXIT.
      ENDIF.
    ENDIF.

    IF iv_priority IS NOT INITIAL
        AND iv_priority <> ''.
      SELECT SINGLE FROM ztarb_priority
             FIELDS @abap_true
             WHERE priority_code = @iv_priority
             INTO @DATA(lv_prior_valid).

      IF lv_prior_valid <> abap_true.
        rv_valid = abap_false.
        EXIT.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD validate_update_order.

    rv_valid = abap_true.

    IF iv_status_original <> c_valid_st_pe.
      rv_valid = abap_false.
      EXIT.
    ENDIF.

    IF NOT me->validate_status_and_priority( iv_status   = iv_status
                                             iv_priority = iv_priority ).
      rv_valid = abap_false.
      EXIT.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
