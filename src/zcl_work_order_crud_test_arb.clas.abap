 CLASS zcl_work_order_crud_test_arb DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

   PUBLIC SECTION.

     METHODS:
       test_create_work_order EXPORTING et_ztwork_order_arb_error   TYPE ztt_wo_error_arb,
       test_read_work_order   EXPORTING et_read_work_order          TYPE ztt_wo_error_arb,
       test_update_work_order EXPORTING et_zt_work_order_arb_update TYPE ztt_wo_error_arb,
       test_delete_work_order EXPORTING et_ztwork_order_arb_delete  TYPE ztt_wo_error_arb.



     INTERFACES if_oo_adt_classrun .
   PROTECTED SECTION.
   PRIVATE SECTION.
 ENDCLASS.



 CLASS zcl_work_order_crud_test_arb IMPLEMENTATION.

   METHOD test_create_work_order.

     DATA(lo_work_order) = NEW zcl_work_order_crud_handler_ar( ).
*    DELETE FROM ztarb_work_order.
*    DELETE FROM ztarb_wo_hist.

     DATA(lt_ztwork_order_arb) = VALUE ztt_work_order_arb( ( customer_id = '0001'
                                                           technician_id = '1001'
                                                           creation_date = '20250813'
                                                           priority = 'A'
                                                           description = 'Update program' )
                                                         ( customer_id = '1002'
                                                           technician_id = '2000'
                                                           creation_date = '20250812'
                                                           priority = 'B'
                                                           description = 'Update program' )
                                                         ( customer_id = '1003'
                                                           technician_id = '3000'
                                                           creation_date = '20250810'
                                                           priority = 'A'
                                                           description = 'Update program' ) ).

     lo_work_order->create_work_order(  EXPORTING it_ztwork_order_arb = lt_ztwork_order_arb
                                        IMPORTING et_ztwork_order_arb_error = DATA(lt_ztwork_order_arb_error) ).

     et_ztwork_order_arb_error = lt_ztwork_order_arb_error.

   ENDMETHOD.



   METHOD if_oo_adt_classrun~main.
     me->test_create_work_order( IMPORTING et_ztwork_order_arb_error = DATA(lt_ztwork_order_arb_error) ).
     out->write( data = lt_ztwork_order_arb_error
                 name = 'Invalid records' ).


     me->test_update_work_order( IMPORTING et_zt_work_order_arb_update = DATA(lt_ztwo_update_arb_error) ).
     out->write( data = lt_ztwo_update_arb_error
                 name = 'Records with update errors' ).

   ENDMETHOD.



   METHOD test_read_work_order.

     DATA(lo_read_work_order) = NEW zcl_work_order_crud_handler_ar(  ).

     lo_read_work_order->read_work_order( EXPORTING iv_work_order = '000000000'
                                                    iv_customer_id = '8'
                                                    iv_status = 'PE'
                                                    iv_creation_date = '00000000'
                                           IMPORTING et_read_wo_arb  = DATA(lt_read_work_order) ).
     et_read_work_order = lt_read_work_order.

   ENDMETHOD.


   METHOD test_update_work_order.

     DATA(lo_test_update_work_order) = NEW zcl_work_order_crud_handler_ar( ).

     DATA(lt_ztwork_order_arb_update) = VALUE ztt_work_order_arb( (  work_order_id = '00000007'
                                                                     status = 'CO'
                                                                     priority = ' ' ) ).

     lo_test_update_work_order->update_work_order( EXPORTING it_ztwo_arb_update = lt_ztwork_order_arb_update
                                                   IMPORTING et_ztwo_update = DATA(lt_ztwo_update_error) ).
     et_zt_work_order_arb_update = lt_ztwo_update_error.


   ENDMETHOD.


      METHOD test_delete_work_order.

     DATA(lo_test_delete_work_order) = NEW zcl_work_order_crud_handler_ar( ).

     DATA(lt_ztwork_order_arb_delete) = VALUE ztt_work_order_arb( (  work_order_id = '00000004'
                                                                   ) ).

     lo_test_delete_work_order->delete_work_order( EXPORTING it_ztwo_arb_delete = lt_ztwork_order_arb_delete
                                                   IMPORTING et_ztwo_arb_delete = DATA(lt_ztwork_order_delete) ).
     et_ztwork_order_arb_delete  = lt_ztwork_order_delete.


   ENDMETHOD.


 ENDCLASS.
