*&---------------------------------------------------------------------*
*& Report Z_METADATA_QUERY
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report z_metadata_query.

data: gr_table1  type ref to cl_gui_alv_grid,   "cl_salv_table,
      gr_table2  type ref to cl_gui_alv_grid,   "cl_salv_table,
      gr_table3  type ref to cl_gui_alv_grid,   "cl_salv_table,
      gr_table4  type ref to cl_gui_alv_grid,   "cl_salv_table,
      gr_table5  type ref to cl_gui_alv_grid,   "cl_salv_table,
      gr_table6  type ref to cl_gui_alv_grid.   "cl_salv_table.

data: container1  type ref to cl_gui_custom_container,
      container2  type ref to cl_gui_custom_container,
      container3  type ref to cl_gui_custom_container,
      container4  type ref to cl_gui_custom_container,
      container5  type ref to cl_gui_custom_container,
      container6  type ref to cl_gui_custom_container.


data: gt_fieldcatalog1  type lvc_t_fcat,
      gt_fieldcatalog2  type lvc_t_fcat,
      gt_fieldcatalog3  type lvc_t_fcat,
      gt_fieldcatalog4  type lvc_t_fcat,
      gt_fieldcatalog5  type lvc_t_fcat,
      gt_fieldcatalog6  type lvc_t_fcat.

data: field_catalog type slis_t_fieldcat_alv.

data: g_okcode type syucomm.



selection-screen begin of screen 1001.

parameters: p_query TYPE RZD1_S_COMPDIR-COMPID obligatory.

*SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE block.
*
*SELECTION-SCREEN BEGIN OF LINE.
*
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK block.
selection-screen end of screen 1001.



start-of-selection.

  call screen 1001.
  if p_query is not initial.
    call method zcl_metadata_query=>get_query_details
      exporting
        p_query                = p_query
      importing
        p_variable             = data(i_variable)
        p_dimension            = data(i_dimension)
        p_restrected           = data(i_restrected)
        p_formula              = data(i_formula)
        p_formula_dependencies = data(i_formula_dependencies)
        p_all_dependecies      = data(i_all_dependecies)
        .

  endif.

end-of-selection.

  if p_query is not initial.
    perform display_grids.
  else.
    set screen 0.
    leave screen.
  endif.

*&---------------------------------------------------------------------*
*&      Form  DISPLAY_GRIDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_grids .

  call screen 100.

endform.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAB_APPL'
constants: begin of c_tab_appl,
             tab1  like sy-ucomm value 'TAB_APPL_FC1',
             tab2  like sy-ucomm value 'TAB_APPL_FC2',
             tab3  like sy-ucomm value 'TAB_APPL_FC3',
             tab4  like sy-ucomm value 'TAB_APPL_FC4',
             tab5  like sy-ucomm value 'TAB_APPL_FC5',
             tab6  like sy-ucomm value 'TAB_APPL_FC6',
           end of c_tab_appl.
*&SPWIZARD: DATA FOR TABSTRIP 'TAB_APPL'
controls:  tab_appl type tabstrip.
data: begin of g_tab_appl,
        subscreen   like sy-dynnr,
        prog        like sy-repid value sy-repid,
        pressed_tab like sy-ucomm value c_tab_appl-tab1,
      end of g_tab_appl.

*&SPWIZARD: OUTPUT MODULE FOR TS 'TAB_APPL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
module tab_appl_active_tab_set output.
  tab_appl-activetab = g_tab_appl-pressed_tab.
  case g_tab_appl-pressed_tab.
    when c_tab_appl-tab1.
      g_tab_appl-subscreen = '0101'.
    when c_tab_appl-tab2.
      g_tab_appl-subscreen = '0102'.
    when c_tab_appl-tab3.
      g_tab_appl-subscreen = '0103'.
    when c_tab_appl-tab4.
      g_tab_appl-subscreen = '0104'.
    when c_tab_appl-tab5.
      g_tab_appl-subscreen = '0105'.
    when c_tab_appl-tab6.
      g_tab_appl-subscreen = '0106'.
    when others.
* do nothing.
  endcase.
  set pf-status 'D0100'.

endmodule.
*&SPWIZARD: INPUT MODULE FOR TS 'TAB_APPL'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: GETS ACTIVE TAB
module tab_appl_active_tab_get input.
  g_okcode = sy-ucomm.
  case g_okcode.
    when c_tab_appl-tab1.
      g_tab_appl-pressed_tab = c_tab_appl-tab1.
    when c_tab_appl-tab2.
      g_tab_appl-pressed_tab = c_tab_appl-tab2.
    when c_tab_appl-tab3.
      g_tab_appl-pressed_tab = c_tab_appl-tab3.
    when c_tab_appl-tab4.
      g_tab_appl-pressed_tab = c_tab_appl-tab4.
    when c_tab_appl-tab5.
      g_tab_appl-pressed_tab = c_tab_appl-tab5.
    when c_tab_appl-tab6.
      g_tab_appl-pressed_tab = c_tab_appl-tab6.
    when others.
*&SPWIZARD:      DO NOTHING
  endcase.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.

  set pf-status 'D0100'.
  set titlebar 'TABSTRIP'.

endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.

  perform d0100_user_command changing g_okcode.

endmodule.                 " USER_COMMAND_0100  INPUT
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

endform.                    " D0100_USER_COMMAND
*&---------------------------------------------------------------------*
*&      Module  DISPLAY_GRID  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module display_grid1 output.


  perform grid1_display.


endmodule.                 " DISPLAY_GRID  OUTPUT

module display_grid2 output.


  perform grid2_display.

endmodule.                 " DISPLAY_GRID  OUTPUT

module display_grid3 output.

  perform grid3_display.

endmodule.                 " DISPLAY_GRID  OUTPUT

module display_grid4 output.

  perform grid4_display.

endmodule.                 " DISPLAY_GRID  OUTPUT

module display_grid5 output.

  perform grid5_display.

endmodule.                 " DISPLAY_GRID  OUTPUT

module display_grid6 output.

  perform grid6_display.

endmodule.                 " DISPLAY_GRID  OUTPUT


*&---------------------------------------------------------------------*
*&      Form  GRID1_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form grid1_display .

*set pf-status 'D0101'.

* create container of tabstrip 1
  if container1 is not bound.
    if cl_salv_table=>is_offline( ) eq if_salv_c_bool_sap=>false.
      create object container1
        exporting
          container_name = 'CONTAINER1'.
    endif.

* create grid instance of tabstrip 1
    create object gr_table1
      exporting
        i_parent = container1.
    if sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

* set handle for layout variants
    data: ls_variant type disvariant.
    ls_variant-report = sy-repid.
    ls_variant-handle = '001'.

    perform create_fieldcatalog using i_variable changing gt_fieldcatalog1.

* call grid instance of tab strip 1
    call method gr_table1->set_table_for_first_display
*      exportXing
*        i_structure_name = 'ALV_T_T2'
*       is_variant       = ls_variant
*       i_save           = 'A'
      changing
        it_fieldcatalog = gt_fieldcatalog1
        it_outtab       = i_variable.
  else.
    call method gr_table1->refresh_table_display.
  endif.

endform.                    " GRID1_DISPLAY
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

  set screen 0.
  leave screen.


endform.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_exit input.

  g_okcode = g_okcode.
  clear g_okcode.

  case g_okcode.
    when 'BACK'.
      perform exit_program.
    when 'EXIT'.
      perform exit_program.
    when 'CANC'.
      perform exit_program.
  endcase.
  leave program.
endmodule.

*&---------------------------------------------------------------------*
*& Form create_fieldcatalog
*&---------------------------------------------------------------------*
* Create a field catalogue from any internal table
*----------------------------------------------------------------------*
* -->PT_TABLE Internal table
* -->PT_FIELDCAT Field Catalogue
*----------------------------------------------------------------------*
form create_fieldcatalog
using pt_table type any table
changing pt_fieldcat type lvc_t_fcat.

  data:
  lr_tabdescr type ref to cl_abap_structdescr
  , lr_data type ref to data
  , lt_dfies type ddfields
  , ls_dfies type dfies
  , ls_fieldcat type lvc_s_fcat
  .

  clear pt_fieldcat.

  create data lr_data like line of pt_table.
  lr_tabdescr ?= cl_abap_structdescr=>describe_by_data_ref( lr_data ).
  lt_dfies = cl_salv_data_descr=>read_structdescr( lr_tabdescr ).

  loop at lt_dfies
  into ls_dfies.
    clear ls_fieldcat.
    move-corresponding ls_dfies to ls_fieldcat.
    ls_fieldcat-seltext = ls_dfies-fieldname.
    ls_fieldcat-coltext = ls_dfies-fieldname.
*    ls_fieldcat-outputlen = strlen( ls_dfies-outputlen ).
    ls_fieldcat-col_opt = 'X'.
    ls_fieldcat-lowercase = 'X'.
    append ls_fieldcat to pt_fieldcat.
  endloop.
endform. "create_fieldcatalog
*&---------------------------------------------------------------------*
*&      Form  GRID2_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form grid2_display .

*set pf-status 'D0101'.

* create container of tabstrip 2
  if container2 is not bound.
    if cl_salv_table=>is_offline( ) eq if_salv_c_bool_sap=>false.
      create object container2
        exporting
          container_name = 'CONTAINER2'.
    endif.

* create grid instance of tabstrip 1
    create object gr_table2
      exporting
        i_parent = container2.
    if sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

* set handle for layout variants
    data: ls_variant type disvariant.
    ls_variant-report = sy-repid.
    ls_variant-handle = '001'.

    perform create_fieldcatalog using i_dimension changing gt_fieldcatalog2.

* call grid instance of tab strip 1
    call method gr_table2->set_table_for_first_display
*      exportXing
*        i_structure_name = 'ALV_T_T2'
*       is_variant       = ls_variant
*       i_save           = 'A'
      changing
        it_fieldcatalog = gt_fieldcatalog2
        it_outtab       = i_dimension.
  else.
    call method gr_table2->refresh_table_display.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  GRID3_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form grid3_display .

*set pf-status 'D0101'.

* create container of tabstrip 2
  if container3 is not bound.
    if cl_salv_table=>is_offline( ) eq if_salv_c_bool_sap=>false.
      create object container3
        exporting
          container_name = 'CONTAINER3'.
    endif.

* create grid instance of tabstrip 1
    create object gr_table3
      exporting
        i_parent = container3.
    if sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

* set handle for layout variants
    data: ls_variant type disvariant.
    ls_variant-report = sy-repid.
    ls_variant-handle = '001'.

    perform create_fieldcatalog using i_restrected changing gt_fieldcatalog3.

* call grid instance of tab strip 1
    call method gr_table3->set_table_for_first_display
*      exportXing
*        i_structure_name = 'ALV_T_T2'
*       is_variant       = ls_variant
*       i_save           = 'A'
      changing
        it_fieldcatalog = gt_fieldcatalog3
        it_outtab       = i_restrected.
  else.
    call method gr_table3->refresh_table_display.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  GRID6_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form grid6_display .

*set pf-status 'D0101'.

* create container of tabstrip 2
  if container6 is not bound.
    if cl_salv_table=>is_offline( ) eq if_salv_c_bool_sap=>false.
      create object container6
        exporting
          container_name = 'CONTAINER6'.
    endif.

* create grid instance of tabstrip 1
    create object gr_table6
      exporting
        i_parent = container6.
    if sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

* set handle for layout variants
    data: ls_variant type disvariant.
    ls_variant-report = sy-repid.
    ls_variant-handle = '001'.

    perform create_fieldcatalog using i_all_dependecies changing gt_fieldcatalog6.

* call grid instance of tab strip 1
    call method gr_table6->set_table_for_first_display
*      exportXing
*        i_structure_name = 'ALV_T_T2'
*       is_variant       = ls_variant
*       i_save           = 'A'
      changing
        it_fieldcatalog = gt_fieldcatalog6
        it_outtab       = i_all_dependecies.
  else.
    call method gr_table6->refresh_table_display.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  GRID7_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  GRID4_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form grid4_display .

*set pf-status 'D0101'.

* create container of tabstrip 2
  if container4 is not bound.
    if cl_salv_table=>is_offline( ) eq if_salv_c_bool_sap=>false.
      create object container4
        exporting
          container_name = 'CONTAINER4'.
    endif.

* create grid instance of tabstrip 1
    create object gr_table4
      exporting
        i_parent = container4.
    if sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

* set handle for layout variants
    data: ls_variant type disvariant.
    ls_variant-report = sy-repid.
    ls_variant-handle = '001'.

    perform create_fieldcatalog using i_formula changing gt_fieldcatalog4.

* call grid instance of tab strip 1
    call method gr_table4->set_table_for_first_display
*      exportXing
*        i_structure_name = 'ALV_T_T2'
*       is_variant       = ls_variant
*       i_save           = 'A'
      changing
        it_fieldcatalog = gt_fieldcatalog4
        it_outtab       = i_formula.
  else.
    call method gr_table4->refresh_table_display.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  GRID5_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form grid5_display .

*set pf-status 'D0101'.

* create container of tabstrip 2
  if container5 is not bound.
    if cl_salv_table=>is_offline( ) eq if_salv_c_bool_sap=>false.
      create object container5
        exporting
          container_name = 'CONTAINER5'.
    endif.

* create grid instance of tabstrip 1
    create object gr_table5
      exporting
        i_parent = container5.
    if sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

* set handle for layout variants
    data: ls_variant type disvariant.
    ls_variant-report = sy-repid.
    ls_variant-handle = '001'.

    perform create_fieldcatalog using i_formula_dependencies changing gt_fieldcatalog5.

* call grid instance of tab strip 1
    call method gr_table5->set_table_for_first_display
*      exportXing
*        i_structure_name = 'ALV_T_T2'
*       is_variant       = ls_variant
*       i_save           = 'A'
      changing
        it_fieldcatalog = gt_fieldcatalog5
        it_outtab       = i_formula_dependencies.
  else.
    call method gr_table5->refresh_table_display.
  endif.

endform.
