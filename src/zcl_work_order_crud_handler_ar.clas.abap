CLASS zcl_work_order_crud_handler_ar DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS: create_work_order IMPORTING it_ztwork_order_arb       TYPE ztt_work_order_arb
                               EXPORTING et_ztwork_order_arb_error TYPE ztt_wo_error_arb ,
      read_work_order   IMPORTING iv_work_order    TYPE zde_work_orderid_arb OPTIONAL
                                  iv_customer_id   TYPE zde_customer_id_arb  OPTIONAL
                                  iv_status        TYPE zde_status_arb       OPTIONAL
                                  iv_creation_date TYPE zde_modif_date_arb   OPTIONAL
                        EXPORTING et_read_wo_arb   TYPE ztt_wo_error_arb,
      update_work_order IMPORTING it_ztwo_arb_update TYPE ztt_work_order_arb
                        EXPORTING et_ztwo_update     TYPE ztt_wo_error_arb,
      delete_work_order IMPORTING it_ztwo_arb_delete TYPE ztt_work_order_arb
                        EXPORTING et_ztwo_arb_delete TYPE ztt_wo_error_arb,
      create_work_order_hist IMPORTING iv_work_order  TYPE zde_work_orderid_arb
                                       iv_change_desc TYPE zde_change_desc_arb.


  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS: c_valid_st_pe     TYPE c LENGTH 2 VALUE 'PE',
               c_msg_no_auth_cre TYPE string VALUE  'Not authorized to create',
               c_msg_no_auth_dis TYPE string VALUE  'Not authorized to display',
               c_msg_no_auth_cha TYPE string VALUE 'Not authorized to change',
               c_msg_no_auth_del TYPE string VALUE 'Not authorized to delete',
               c_msg_inv_ctp     TYPE string VALUE  'Invalid customer ID, technician ID or priority',
               c_msg_inv_sop     TYPE string VALUE 'Invalid Status or Priority',
               c_msg_inv_sta     TYPE string VALUE 'Invalid Status',
               c_msg_not_created TYPE string VALUE  'Record not created',
               c_msg_rec_no_chan TYPE string VALUE 'Record not changed',
               c_msg_rec_no_del  TYPE string VALUE 'Record not deleted',
               c_msg_wo_block    TYPE string VALUE 'Work order ID locked',
               c_msg_created     TYPE c LENGTH 50 VALUE   'CREATED',
               c_msg_updated     TYPE c LENGTH 50 VALUE  'UPDATED',
               c_msg_deleted     TYPE c LENGTH 50 VALUE 'DELETED'.


ENDCLASS.

CLASS zcl_work_order_crud_handler_ar IMPLEMENTATION.


  METHOD create_work_order .

    DATA(lo_validator) = NEW zcl_work_order_validator_arb(  ).
    DATA(lt_ztwork_order_arb_aux) = it_ztwork_order_arb .
    DATA ls_zswork_order_arb_error TYPE zsarb_wo_error .

    LOOP AT lt_ztwork_order_arb_aux ASSIGNING FIELD-SYMBOL(<fs_ztwork_order_arb_aux>).

*
*      IF lo_validator->validate_authority( iv_work_order_id =
*         <fs_ztwork_order_arb_aux>-work_order_id
*          iv_actvt = '01' ) = abap_false.
*        ls_zswork_order_arb_error-message = c_msg_no_auth_cre .
*        APPEND ls_zswork_order_arb_error TO et_ztwork_order_arb_error.
*        CONTINUE.
*      ENDIF.

      SELECT FROM ztarb_work_order
      FIELDS MAX(  work_order_id )
      INTO @DATA(lv_last_work_order_id).
      <fs_ztwork_order_arb_aux>-client = sy-mandt.
      <fs_ztwork_order_arb_aux>-work_order_id = lv_last_work_order_id + 1.

      IF lo_validator->validate_create_order(   iv_customer_id =   <fs_ztwork_order_arb_aux>-customer_id
                                                iv_technician_id = <fs_ztwork_order_arb_aux>-technician_id
                                                iv_priority =      <fs_ztwork_order_arb_aux>-priority ) = abap_false.

        MOVE-CORRESPONDING <fs_ztwork_order_arb_aux> TO ls_zswork_order_arb_error.
        ls_zswork_order_arb_error-message = c_msg_inv_ctp.
        APPEND ls_zswork_order_arb_error TO et_ztwork_order_arb_error.
        CONTINUE.
      ENDIF.

      INSERT ztarb_work_order FROM @<fs_ztwork_order_arb_aux>.
      IF sy-subrc EQ 0.
        me->create_work_order_hist( iv_work_order = <fs_ztwork_order_arb_aux>-work_order_id
        iv_change_desc = c_msg_created ).

      ELSE.
        MOVE-CORRESPONDING <fs_ztwork_order_arb_aux> TO ls_zswork_order_arb_error.
        ls_zswork_order_arb_error-message = c_msg_not_created .
        APPEND ls_zswork_order_arb_error TO et_ztwork_order_arb_error.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD read_work_order.

    DATA lv_where_conditions TYPE string.
    DATA lx_dynamic_osql TYPE REF TO cx_root.
    DATA(lo_validator) = NEW zcl_work_order_validator_arb(  ).
    DATA ls_zswork_order_arb_error TYPE zsarb_wo_error .

*    IF lo_validator->validate_authority( iv_work_order_id  = iv_work_order
*                                          iv_actvt = '03' ) = abap_false.
*      ls_zswork_order_arb_error-message =  c_msg_no_auth_dis .
*      APPEND ls_zswork_order_arb_error TO et_read_wo_arb .
*      EXIT.
*    ENDIF.

    IF iv_work_order IS NOT INITIAL
    AND iv_work_order <> '0000000000'.
      lv_where_conditions = |work_order_id = { iv_work_order }|.
    ENDIF.

    IF iv_customer_id IS NOT INITIAL.
      IF lv_where_conditions IS INITIAL.
        lv_where_conditions = |customer_id = { iv_customer_id }|.
      ELSE.
        lv_where_conditions = |{ lv_where_conditions } and customer_id  = { iv_customer_id }|.
      ENDIF.
    ENDIF.

    IF iv_status IS NOT INITIAL.
      IF lv_where_conditions IS INITIAL.
        lv_where_conditions = |status = { iv_status }|.
      ELSE.
        lv_where_conditions = |{ lv_where_conditions } and status = { iv_status }  |.
      ENDIF.
    ENDIF.

    IF iv_creation_date IS NOT INITIAL.
      IF lv_where_conditions IS INITIAL.
        lv_where_conditions = |creation_date = { iv_creation_date }|.
      ELSE.
        lv_where_conditions = |{ lv_where_conditions } and creation_date = { iv_creation_date }|.
      ENDIF.
    ENDIF.

    TRY.

        SELECT FROM ztarb_work_order
               FIELDS *
               WHERE (lv_where_conditions)
               INTO TABLE @DATA(lt_read_work_order).


      CATCH cx_sy_dynamic_osql_syntax
            cx_sy_dynamic_osql_semantics
            cx_sy_dynamic_osql_error INTO lx_dynamic_osql.
        RETURN.
    ENDTRY.

  ENDMETHOD.


  METHOD update_work_order.

    DATA(lt_ztwo_update_aux_arb) = it_ztwo_arb_update.
    DATA(lo_validator_update) = NEW zcl_work_order_validator_arb(  ).
    DATA ls_zswork_order_arb_error TYPE zsarb_wo_error .

    LOOP AT lt_ztwo_update_aux_arb ASSIGNING FIELD-SYMBOL(<fs_ztwo_update_aux_arb>).
*
*      IF lo_validator_update->validate_authority( iv_work_order_id  =
*      <fs_ztwo_update_aux_arb>-work_order_id
*      iv_actvt = '02' ) = abap_false.
*
*        MOVE-CORRESPONDING <fs_ztwo_update_aux_arb> TO ls_zswork_order_arb_error.
*        ls_zswork_order_arb_error-message = c_msg_no_auth_cha .
*        APPEND ls_zswork_order_arb_error TO et_ztwo_update.
*        CONTINUE.
*      ENDIF.

      TRY.
          DATA(lo_lock_object) = cl_abap_lock_object_factory=>get_instance( EXPORTING
          iv_name = 'EZ_WORK_ORDER' ).

        CATCH cx_abap_lock_failure.

      ENDTRY.

      DATA lt_parameter TYPE if_abap_lock_object=>tt_parameter.

      lt_parameter = VALUE #( ( name = 'work_order_id'
                                value = REF #( <fs_ztwo_update_aux_arb>-work_order_id ) ) ).

      TRY.
          lo_lock_object->enqueue( it_parameter = lt_parameter ).

          SELECT SINGLE FROM ztarb_work_order
                 FIELDS *
                 WHERE work_order_id EQ @<fs_ztwo_update_aux_arb>-work_order_id
                 INTO @DATA(ls_zswork_order_arb).


          IF lo_validator_update->validate_update_order(
          iv_work_order_id = ls_zswork_order_arb-work_order_id
          iv_status_original = ls_zswork_order_arb-status
          iv_status = <fs_ztwo_update_aux_arb>-status
          iv_priority = <fs_ztwo_update_aux_arb>-priority ) = abap_false.

            MOVE-CORRESPONDING <fs_ztwo_update_aux_arb> TO ls_zswork_order_arb_error.
            ls_zswork_order_arb_error-message = c_msg_inv_sop .
            APPEND ls_zswork_order_arb_error TO et_ztwo_update.
            CONTINUE.
          ENDIF.

          IF <fs_ztwo_update_aux_arb>-status IS NOT INITIAL
         AND <fs_ztwo_update_aux_arb>-status <> ''.
            ls_zswork_order_arb-status = <fs_ztwo_update_aux_arb>-status.
          ENDIF.


          IF <fs_ztwo_update_aux_arb>-priority IS NOT INITIAL
          AND <fs_ztwo_update_aux_arb>-priority <> ''.
            ls_zswork_order_arb-priority = <fs_ztwo_update_aux_arb>-priority.
          ENDIF.

          UPDATE ztarb_work_order FROM @ls_zswork_order_arb.

          IF sy-subrc NE 0.
            MOVE-CORRESPONDING <fs_ztwo_update_aux_arb> TO ls_zswork_order_arb_error.
            ls_zswork_order_arb_error-message =   c_msg_rec_no_chan  .
            APPEND ls_zswork_order_arb_error TO et_ztwo_update.
            CONTINUE.
          ENDIF.

          me->create_work_order_hist( iv_work_order = ls_zswork_order_arb-work_order_id
                                       iv_change_desc = c_msg_updated ).

        CATCH cx_abap_foreign_lock cx_abap_lock_failure.
          MOVE-CORRESPONDING <fs_ztwo_update_aux_arb> TO ls_zswork_order_arb_error.
          ls_zswork_order_arb_error-message =  c_msg_wo_block.
          APPEND ls_zswork_order_arb_error TO et_ztwo_update.
          CONTINUE.
      ENDTRY.

      TRY.
          lo_lock_object->dequeue( it_parameter = lt_parameter ).

        CATCH cx_abap_lock_failure.
      ENDTRY.

    ENDLOOP.


  ENDMETHOD.



  METHOD delete_work_order.

    DATA(lo_validator_delete) = NEW zcl_work_order_validator_arb(  ).
    DATA(lt_ztwo_del_aux_arb) = it_ztwo_arb_delete.
    DATA ls_zswork_order_arb_error TYPE zsarb_wo_error .

    LOOP AT lt_ztwo_del_aux_arb ASSIGNING FIELD-SYMBOL(<fs_ztwo_del_aux_arb>).

*      IF lo_validator_delete->validate_authority( iv_work_order_id  =
*      <fs_ztwo_del_aux_arb>-work_order_id
*      iv_actvt = '06' ) = abap_false.
*
*        MOVE-CORRESPONDING <fs_ztwo_del_aux_arb> TO ls_zswork_order_arb_error.
*        ls_zswork_order_arb_error-message = c_msg_no_auth_del.
*        APPEND ls_zswork_order_arb_error TO et_ztwo_arb_delete.
*        CONTINUE.
*      ENDIF.

      TRY.
          DATA(lo_lock_object) = cl_abap_lock_object_factory=>get_instance( EXPORTING
          iv_name = 'EZ_WORK_ORDER' ).

        CATCH cx_abap_lock_failure.
      ENDTRY.

      DATA lt_parameter TYPE if_abap_lock_object=>tt_parameter.

      lt_parameter = VALUE #( ( name = 'work_order_id'
                                value = REF #( <fs_ztwo_del_aux_arb>-work_order_id ) ) ).

      TRY.
          lo_lock_object->enqueue( it_parameter = lt_parameter ).

          SELECT SINGLE FROM ztarb_work_order
                 FIELDS *
                 WHERE work_order_id EQ @<fs_ztwo_del_aux_arb>-work_order_id
                 INTO @DATA(ls_ztwork_order_arb).


          IF lo_validator_delete->validate_delete_order( iv_work_order_id =
          ls_ztwork_order_arb-work_order_id
          iv_status =
          ls_ztwork_order_arb-status ) = abap_false.

            MOVE-CORRESPONDING <fs_ztwo_del_aux_arb> TO ls_zswork_order_arb_error.
            ls_zswork_order_arb_error-message = c_msg_inv_sta.
            APPEND ls_zswork_order_arb_error TO et_ztwo_arb_delete.
            CONTINUE.
          ENDIF.

          DELETE ztarb_work_order FROM @ls_ztwork_order_arb.

          IF sy-subrc NE 0.
            MOVE-CORRESPONDING <fs_ztwo_del_aux_arb> TO ls_zswork_order_arb_error.
            ls_zswork_order_arb_error-message = c_msg_rec_no_del .
            APPEND ls_zswork_order_arb_error TO et_ztwo_arb_delete .
            CONTINUE.
          ENDIF.
          me->create_work_order_hist( iv_work_order = ls_ztwork_order_arb-work_order_id
                                                       iv_change_desc = c_msg_deleted  ).

        CATCH cx_abap_foreign_lock cx_abap_lock_failure.
          MOVE-CORRESPONDING <fs_ztwo_del_aux_arb> TO ls_zswork_order_arb_error.
          ls_zswork_order_arb_error-message = c_msg_wo_block .
          APPEND ls_zswork_order_arb_error TO et_ztwo_arb_delete .
          CONTINUE.
      ENDTRY.
      TRY.
          lo_lock_object->dequeue( it_parameter = lt_parameter ).
        CATCH cx_abap_lock_failure.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD create_work_order_hist.

    SELECT FROM ztarb_wo_hist
           FIELDS MAX( history_id )
           INTO @DATA(lv_last_history_id).

    DATA(ls_ztwork_arb_hist_aux) = VALUE zsarb_wo_hist( client = sy-mandt
                                         history_id = lv_last_history_id + 1
                                         work_order_id = iv_work_order
                                         modification_date = cl_abap_context_info=>get_system_date( )
                                         change_description = iv_change_desc ).
    INSERT ztarb_wo_hist FROM @ls_ztwork_arb_hist_aux.


  ENDMETHOD.



ENDCLASS.
