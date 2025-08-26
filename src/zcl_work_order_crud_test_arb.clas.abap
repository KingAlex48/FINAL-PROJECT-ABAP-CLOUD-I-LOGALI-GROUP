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

*     DATA(lo_work_order) = NEW zcl_work_order_crud_handler_ar( ).
*
*
*     DATA(lt_ztwork_order_arb) = VALUE ztt_work_order_arb( (
*                                                           customer_id = '00000003'
*                                                           technician_id = '0000002C'
*                                                           creation_date = '20250803'
*                                                           status = 'PE'
*                                                           priority = 'A'
*                                                           description = 'Update firewall rules for SAP connections' )
*                                                         (
*                                                           customer_id = '00000008'
*                                                           technician_id = '0000003A'
*                                                           creation_date = '20250804'
*                                                           status = 'PE'
*                                                           priority = 'B'
*                                                           description = 'Fix bug in custom Z-report' )
*                                                         (
*                                                           customer_id = '00000005'
*                                                           technician_id = '0000002A'
*                                                           creation_date = '20250805'
*                                                           status = 'PE'
*                                                           priority = 'C'
*                                                           description = 'Enhance sales order interface' )
*                                                         (
*                                                           customer_id = '00000002'
*                                                           technician_id = '0000001B'
*                                                           creation_date = '20250806'
*                                                           status = 'PE'
*                                                           priority = 'A'
*                                                           description = 'Backup and restore SAP HANA database' )
*                                                           (
*                                                           customer_id = '00000004'
*                                                           technician_id = '0000001C'
*                                                           creation_date = '20250807'
*                                                           status = 'CO'
*                                                           priority = 'B'
*                                                           description = 'Patch vulnerability in SAP application server' )
*                                                           (
*                                                           customer_id = '00000001'
*                                                           technician_id = '0000002C'
*                                                           creation_date = '20250801'
*                                                           status = 'PE'
*                                                           priority = 'A'
*                                                           description = 'Configure user role authorization in SAP' )
*                                                         (
*                                                           customer_id = '00000002'
*                                                           technician_id = '00000060'
*                                                           creation_date = '20250802'
*                                                           status = 'PE'
*                                                           priority = 'A'
*                                                           description = 'Optimize slow queries in production system' ) ).
*
*     lo_work_order->create_work_order(  EXPORTING it_ztwork_order_arb = lt_ztwork_order_arb
*                                        IMPORTING et_ztwork_order_arb_error = DATA(lt_ztwork_order_arb_error) ).
*
*     et_ztwork_order_arb_error = lt_ztwork_order_arb_error.

   ENDMETHOD.



   METHOD if_oo_adt_classrun~main.
     me->test_create_work_order( IMPORTING et_ztwork_order_arb_error = DATA(lt_ztwork_order_arb_error) ).
     out->write( data = lt_ztwork_order_arb_error
                 name = 'Invalid records' ).


     me->test_update_work_order( IMPORTING et_zt_work_order_arb_update = DATA(lt_ztwo_update_arb_error) ).
     out->write( data = lt_ztwo_update_arb_error
                 name = 'Records with update errors' ).

     me->test_read_work_order(
        IMPORTING et_read_work_order = DATA(lt_read_work_order) ).
     out->write( data =  lt_read_work_order
                 name = 'Work orders found' ).

   ENDMETHOD.



   METHOD test_read_work_order.

     DATA(lo_read_work_order) = NEW zcl_work_order_crud_handler_ar(  ).

     lo_read_work_order->read_work_order( EXPORTING iv_work_order = '0000000001'
                                                    iv_customer_id = '00000003'
                                                    iv_status = 'PE'
                                                    iv_creation_date = '20250803'
                                           IMPORTING et_read_wo_arb  = DATA(lt_read_work_order) ).
     et_read_work_order = lt_read_work_order.



   ENDMETHOD.


   METHOD test_update_work_order.

*     DATA(lo_test_update_work_order) = NEW zcl_work_order_crud_handler_ar( ).
*
*     DATA(lt_ztwork_order_arb_update) = VALUE ztt_work_order_arb( (  work_order_id = '0000000005'
*                                                                     status = 'CO'
*                                                                     priority = 'B'   ) ).
*
*
*     lo_test_update_work_order->update_work_order( EXPORTING it_ztwo_arb_update = lt_ztwork_order_arb_update
*                                                   IMPORTING et_ztwo_update = DATA(lt_ztwo_update_error) ).
*     et_zt_work_order_arb_update = lt_ztwo_update_error.


   ENDMETHOD.


   METHOD test_delete_work_order.
*
*     DATA(lo_test_delete_work_order) = NEW zcl_work_order_crud_handler_ar( ).
*
*     DATA(lt_ztwork_order_arb_delete) = VALUE ztt_work_order_arb( (  work_order_id = '0000000004'
*                                                                   ) ).
*
*     lo_test_delete_work_order->delete_work_order( EXPORTING it_ztwo_arb_delete = lt_ztwork_order_arb_delete
*                                                   IMPORTING et_ztwo_arb_delete = DATA(lt_ztwork_order_delete) ).
*     et_ztwork_order_arb_delete  = lt_ztwork_order_delete.


   ENDMETHOD.


 ENDCLASS.
