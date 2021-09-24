*&---------------------------------------------------------------------*
*&  Include           Z_METADATA_COMMAND
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  EXIT_PROGRAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form exit_program .


  clear g_okcode.

  call method container1->free.
  if container2 is not initial.
    call method container2->free.
  endif.
  if container3 is not initial.
    call method container3->free.
  endif.
  if container4 is not initial.
    call method container4->free.
  endif.
  if container5 is not initial.
    call method container5->free.
  endif.
  if container6 is not initial.
    call method container6->free.
  endif.
  if container7 is not initial.
    call method container7->free.
  endif.
  if container8 is not initial.
    call method container8->free.
  endif.
  if container9 is not initial.
    call method container9->free.
  endif.
  if container10 is not initial.
    call method container10->free.
  endif.
  if container11 is not initial.
    call method container11->free.
  endif.
  if container12 is not initial.
    call method container12->free.
  endif.
  if container13 is not initial.
    call method container13->free.
  endif.

  free gr_table1.
  if gr_table2 is not initial.
    free gr_table2.
  endif.
  if gr_table3 is not initial.
    free gr_table3.
  endif.
  if gr_table4 is not initial.
    free gr_table4.
  endif.
  if gr_table5 is not initial.
    free gr_table5.
  endif.
  if gr_table6 is not initial.
    free gr_table6.
  endif.
  if gr_table7 is not initial.
    free gr_table7.
  endif.
  if gr_table8 is not initial.
    free gr_table8.
  endif.
  if gr_table9 is not initial.
    free gr_table9.
  endif.
  if gr_table10 is not initial.
    free gr_table10.
  endif.
  if gr_table11 is not initial.
    free gr_table11.
  endif.
  if gr_table12 is not initial.
    free gr_table12.
  endif.
  if gr_table13 is not initial.
    free gr_table13.
  endif.

  set screen 0.
  leave screen.


endform.



*&---------------------------------------------------------------------*
*&      Form  D0100_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_G_OKCODE  text
*----------------------------------------------------------------------*
form d0100_user_command  changing c_okcode type sy-ucomm.

  data: l_okcode type sy-ucomm.

  l_okcode = c_okcode.

  case l_okcode.

    when 'BACK'.
      perform exit_program.
    when 'EXIT'.
      perform exit_program.
    when 'CANC'.
      perform exit_program.
    when others.
      exit.
  endcase.

endform.
