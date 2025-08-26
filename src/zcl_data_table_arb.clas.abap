CLASS zcl_data_table_arb DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_data_table_arb IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    INSERT ztarb_customer FROM TABLE @( VALUE #( (       customer_id = '00000001'
                                                            address = 'Reforma Mexico City'
                                                            name = 'Carlos Villanueva'
                                                            phone = '5551456637' )
                                                          ( customer_id = '00000002'
                                                            address = 'Av. Chapultepec Guadalajara Jalisco'
                                                            name = 'Santiago Gomez'
                                                            phone = '3331456697' )
                                                          ( customer_id = '00000003'
                                                            address = 'Av. Madero Monterrey Nuevo Leon'
                                                            name = 'Andres Rodriguez'
                                                            phone = '8111404637' )
                                                            ( customer_id = '00000004'
                                                            address = 'Angelopolis Puebla de Zaragoza'
                                                            name = 'Mitzy Acevedo'
                                                            phone = '2221759631' )
                                                            ( customer_id = '00000005'
                                                            address = 'Venustiano Carranza Queretaro '
                                                            name = 'Pedro Vasquez'
                                                            phone = '4421563426' )  ) ) .

    IF sy-subrc EQ 0.
      out->write( |Customer table data inserted successfully| ).
    ELSE.
      out->write( |ERROR Customer table  | ).
    ENDIF.

    INSERT ztarb_technician FROM TABLE @( VALUE #( ( technician_id = '0000001A'
                                                     name  = 'Julian Alvarez'
                                                     specialty = 'ABAP'        )
                                                   ( technician_id = '0000001C'
                                                     name  = 'Andrea Garcia'
                                                     specialty = 'Ciberseguridad'        )
                                                   ( technician_id = '0000002A'
                                                     name  = 'Yair Rodriguez'
                                                     specialty = 'ABAP'        )
                                                   ( technician_id = '0000001B'
                                                     name  = 'Pablo Medina'
                                                     specialty = 'Database Admin'        )
                                                   ( technician_id = '0000002C'
                                                     name  = 'Sofia Guitierrez'
                                                     specialty = 'Ciberseguridad'        ) ) ) .
    IF sy-subrc EQ 0.
      out->write( | Technician table data inserted successfully | ).
    ELSE.
      out->write( | ERROR Technician table  | ).
    ENDIF.

    INSERT ztarb_priority FROM TABLE @( VALUE #(  ( priority_code  =     'A'
                                                     priority_description = 'High'   )
                                                   ( priority_code  =     'B'
                                                     priority_description = 'Low'     ) ) ) .
    IF sy-subrc EQ 0.
      out->write( | Priority table data inserted successfully | ).
    ELSE.
      out->write( | ERROR Priority table  | ).
    ENDIF.

    INSERT ztarb_status FROM TABLE @( VALUE #( ( status_code = 'PE'
                                                 status_description =  'Pending'      )
                                              ( status_code = 'CO'
                                                 status_description =  'Completed'      ) ) ) .
    IF sy-subrc EQ 0.
      out->write( | Status table data inserted successfully | ).
    ELSE.
      out->write( | ERROR Status table  | ).
    ENDIF.



*    DELETE FROM ztarb_work_order.


    IF sy-subrc EQ 0.
      out->write( | Work order table data deleted successfully | ).
    ELSE.
      out->write( | ERROR  Work order table  | ).
    ENDIF.



*        DELETE FROM ztarb_wo_hist.

    IF sy-subrc EQ 0.
      out->write( | Work order history table data deleted successfully | ).
    ELSE.
      out->write( | ERROR  Work order history table  | ).
    ENDIF.



  ENDMETHOD.



ENDCLASS.
