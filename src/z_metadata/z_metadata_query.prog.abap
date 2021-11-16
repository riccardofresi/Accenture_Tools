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
DATA: lo_excel     TYPE REF TO zcl_excel,
      lo_worksheet TYPE REF TO zcl_excel_worksheet,
      lo_column    TYPE REF TO zcl_excel_column.

DATA: ls_table_settings       TYPE zexcel_s_table_settings.


DATA: lv_title TYPE zexcel_sheet_title,
      lt_carr  TYPE TABLE OF scarr,
      row      TYPE zexcel_cell_row VALUE 2,
      lo_range TYPE REF TO zcl_excel_range.
DATA: lo_data_validation  TYPE REF TO zcl_excel_data_validation.
CLASS lcl_output DEFINITION CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      output         IMPORTING cl_excel            TYPE REF TO zcl_excel
                               iv_writerclass_name TYPE clike OPTIONAL,
      f4_path        RETURNING VALUE(selected_folder) TYPE string,
      parametertexts.

  PRIVATE SECTION.
    METHODS:
      download_frontend,
      download_backend,
      display_online,
      send_email.

    DATA: xdata     TYPE xstring,             " Will be used for sending as email
          t_rawdata TYPE solix_tab,           " Will be used for downloading or open directly
          bytecount TYPE i.                   " Will be used for downloading or open directly
ENDCLASS.                    "lcl_output DEFINITION
data gc_save_file_name TYPE string VALUE '03_iTab.xlsx'.

selection-screen begin of screen 1001.

parameters: p_query TYPE RZD1_S_COMPDIR-COMPID obligatory.

*SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE block.
*
*SELECTION-SCREEN BEGIN OF LINE.
*
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK block.
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE txt_bl1.

PARAMETERS: rb_down as checkbox  USER-COMMAND space.
PARAMETERS: p_path  TYPE string LOWER CASE MODIF ID pat.
PARAMETERS: p_backfn TYPE text40 NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK bl1.
selection-screen end of screen 1001.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.

    IF rb_down IS INITIAL AND screen-group1 = 'PAT'.
      screen-input = 0.
      screen-invisible = 1.
    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.

INITIALIZATION.
  IF sy-batch IS INITIAL.
    cl_gui_frontend_services=>get_sapgui_workdir( CHANGING sapworkdir = p_path ).
    cl_gui_cfw=>flush( ).
  ENDIF.
  lcl_output=>parametertexts( ).  " If started in language w/o textelements translated set defaults

  concatenate p_query '.xlsx' into gc_save_file_name.
  sy-title = gc_save_file_name.
  txt_bl1 = 'Output options'(bl1).
  p_backfn = gc_save_file_name.  " Use as default if nothing else is supplied by submit

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.
  p_path = lcl_output=>f4_path( ).

start-of-selection.

  call screen 1001.
  if p_query is not initial.
    concatenate p_query '.xlsx' into gc_save_file_name.
  sy-title = gc_save_file_name.
  txt_bl1 = 'Output options'(bl1).
  p_backfn = gc_save_file_name.  " Use as default if nothing else is supplied by submit

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
  if rb_down is initial.
    perform display_grids.
    else.
    perform export.
    endif.
  else.
    set screen 0.
    leave screen.
  endif.

*&---------------------------------------------------------------------*
*&      Form  export
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form export.
  CREATE OBJECT lo_excel.

  " Get active sheet
  lo_worksheet = lo_excel->get_active_worksheet( ).
  lo_worksheet->set_title( ip_title = 'Variables' ).

  ls_table_settings-table_style       = zcl_excel_table=>builtinstyle_medium2.
  ls_table_settings-show_row_stripes  = abap_true.
  ls_table_settings-nofilters         = abap_true.

  lo_worksheet->bind_table( ip_table          = i_variable
                            is_table_settings = ls_table_settings ).


  lo_worksheet = lo_excel->add_new_worksheet( ).
  lo_worksheet->set_title( ip_title = 'Dimension' ).

  lo_worksheet->bind_table( ip_table          = i_dimension
                            is_table_settings = ls_table_settings ).

  lo_worksheet = lo_excel->add_new_worksheet( ).
  lo_worksheet->set_title( ip_title = 'Restricted' ).

  lo_worksheet->bind_table( ip_table          = i_restrected
                            is_table_settings = ls_table_settings ).

  lo_worksheet = lo_excel->add_new_worksheet( ).
  lo_worksheet->set_title( ip_title = 'Formulas' ).

  lo_worksheet->bind_table( ip_table          = i_formula
                            is_table_settings = ls_table_settings ).

  lo_worksheet = lo_excel->add_new_worksheet( ).
  lo_worksheet->set_title( ip_title = 'Formulas Dependencies' ).

  lo_worksheet->bind_table( ip_table          = i_formula_dependencies
                            is_table_settings = ls_table_settings ).

  lo_worksheet = lo_excel->add_new_worksheet( ).
  lo_worksheet->set_title( ip_title = 'All Dependencies' ).

  lo_worksheet->bind_table( ip_table          = i_all_dependecies
                            is_table_settings = ls_table_settings ).
***Create output
  lcl_output=>output( lo_excel ).
endform.
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

*----------------------------------------------------------------------*
*       CLASS lcl_output IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_output IMPLEMENTATION.
  METHOD output.

    DATA: cl_output TYPE REF TO lcl_output,
          cl_writer TYPE REF TO zif_excel_writer.

    IF iv_writerclass_name IS INITIAL.
      CREATE OBJECT cl_output.
      CREATE OBJECT cl_writer TYPE zcl_excel_writer_2007.
    ELSE.
      CREATE OBJECT cl_output.
      CREATE OBJECT cl_writer TYPE (iv_writerclass_name).
    ENDIF.
    cl_output->xdata = cl_writer->write_file( cl_excel ).

* After 6.40 via cl_bcs_convert
    cl_output->t_rawdata = cl_bcs_convert=>xstring_to_solix( iv_xstring  = cl_output->xdata ).
    cl_output->bytecount = xstrlen( cl_output->xdata ).

* before 6.40
*  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
*    EXPORTING
*      buffer        = cl_output->xdata
*    IMPORTING
*      output_length = cl_output->bytecount
*    TABLES
*      binary_tab    = cl_output->t_rawdata.

    CASE 'X'.
      WHEN rb_down.
        IF sy-batch IS INITIAL.
          cl_output->download_frontend( ).
        ELSE.
          MESSAGE e802(zabap2xlsx).
        ENDIF.

*      WHEN rb_back.
*        cl_output->download_backend( ).
*
*      WHEN rb_show.
*        IF sy-batch IS INITIAL.
*          cl_output->display_online( ).
*        ELSE.
*          MESSAGE e803(zabap2xlsx).
*        ENDIF.
*
*      WHEN rb_send.
*        cl_output->send_email( ).

    ENDCASE.
  ENDMETHOD.                    "output
    METHOD f4_path.
    DATA: new_path      TYPE string,
          repid         TYPE syrepid,
          dynnr         TYPE sydynnr,
          lt_dynpfields TYPE TABLE OF dynpread,
          ls_dynpfields LIKE LINE OF lt_dynpfields.

* Get current value
    dynnr = sy-dynnr.
    repid = sy-repid.
    ls_dynpfields-fieldname = 'P_PATH'.
    APPEND ls_dynpfields TO lt_dynpfields.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = repid
        dynumb               = dynnr
      TABLES
        dynpfields           = lt_dynpfields
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        invalid_parameter    = 7
        undefind_error       = 8
        double_conversion    = 9
        stepl_not_found      = 10
        OTHERS               = 11.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.

    READ TABLE lt_dynpfields INTO ls_dynpfields INDEX 1.

    new_path = ls_dynpfields-fieldvalue.
    selected_folder = new_path.

    cl_gui_frontend_services=>directory_browse(
      EXPORTING
        window_title         = 'Select path to download EXCEL-file'
        initial_folder       = new_path
      CHANGING
        selected_folder      = new_path
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4
           ).
    cl_gui_cfw=>flush( ).
    CHECK new_path IS NOT INITIAL.
    selected_folder = new_path.

  ENDMETHOD.                                                "f4_path

  METHOD parametertexts.
* If started in language w/o textelements translated set defaults
* Furthermore I don't have to change the selectiontexts of all demoreports.
    DEFINE default_parametertext.
      IF %_&1_%_app_%-text = '&1' OR
         %_&1_%_app_%-text IS INITIAL.
        %_&1_%_app_%-text = &2.
      ENDIF.
    END-OF-DEFINITION.

    default_parametertext:  rb_down  'Save to frontend',
*                            rb_back  'Save to backend',
*                            rb_show  'Direct display',
*                            rb_send  'Send via email',

                            p_path   'Frontend-path to download to'.
*                            p_email  'Email to send xlsx to'.

  ENDMETHOD.                    "parametertexts

  METHOD: download_frontend.
    DATA: filename TYPE string.
* I don't like p_path here - but for this include it's ok
    filename = p_path.
* Add trailing "\" or "/"
    IF filename CA '/'.
      REPLACE REGEX '([^/])\s*$' IN filename WITH '$1/' .
    ELSE.
      REPLACE REGEX '([^\\])\s*$' IN filename WITH '$1\\'.
    ENDIF.

    CONCATENATE filename gc_save_file_name INTO filename.
* Get trailing blank
    cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = bytecount
                                                      filename     = filename
                                                      filetype     = 'BIN'
                                             CHANGING data_tab     = t_rawdata ).
  ENDMETHOD.                    "download_frontend

  METHOD download_backend.
    DATA: bytes_remain TYPE i.
    FIELD-SYMBOLS: <rawdata> LIKE LINE OF t_rawdata.

    OPEN DATASET p_backfn FOR OUTPUT IN BINARY MODE.
    CHECK sy-subrc = 0.

    bytes_remain = bytecount.

    LOOP AT t_rawdata ASSIGNING <rawdata>.

      AT LAST.
        CHECK bytes_remain >= 0.
        TRANSFER <rawdata> TO p_backfn LENGTH bytes_remain.
        EXIT.
      ENDAT.

      TRANSFER <rawdata> TO p_backfn.
      SUBTRACT 255 FROM bytes_remain.  " Solix has length 255

    ENDLOOP.

    CLOSE DATASET p_backfn.

    IF sy-repid <> sy-cprog AND sy-cprog IS NOT INITIAL.  " no need to display anything if download was selected and report was called for demo purposes
      LEAVE PROGRAM.
    ELSE.
      MESSAGE 'Data transferred to default backend directory' TYPE 'S'.
    ENDIF.
  ENDMETHOD.                    "download_backend

  METHOD display_online.
    DATA:error       TYPE REF TO i_oi_error,
         t_errors    TYPE STANDARD TABLE OF REF TO i_oi_error WITH NON-UNIQUE DEFAULT KEY,
         cl_control  TYPE REF TO i_oi_container_control, "OIContainerCtrl
         cl_document TYPE REF TO i_oi_document_proxy.   "Office Dokument

    c_oi_container_control_creator=>get_container_control( IMPORTING control = cl_control
                                                                     error   = error ).
    APPEND error TO t_errors.

    cl_control->init_control( EXPORTING  inplace_enabled     = 'X'
                                         no_flush            = 'X'
                                         r3_application_name = 'Demo Document Container'
                                         parent              = cl_gui_container=>screen0
                              IMPORTING  error               = error
                              EXCEPTIONS OTHERS              = 2 ).
    APPEND error TO t_errors.

    cl_control->get_document_proxy( EXPORTING document_type  = 'Excel.Sheet'                " EXCEL
                                              no_flush       = ' '
                                    IMPORTING document_proxy = cl_document
                                              error          = error ).
    APPEND error TO t_errors.
* Errorhandling should be inserted here

    cl_document->open_document_from_table( EXPORTING document_size    = bytecount
                                                     document_table   = t_rawdata
                                                     open_inplace     = 'X' ).

    WRITE: '.'.  " To create an output.  That way screen0 will exist
  ENDMETHOD.                    "display_online

  METHOD send_email.
** Needed to send emails
*    DATA: bcs_exception        TYPE REF TO cx_bcs,
*          errortext            TYPE string,
*          cl_send_request      TYPE REF TO cl_bcs,
*          cl_document          TYPE REF TO cl_document_bcs,
*          cl_recipient         TYPE REF TO if_recipient_bcs,
*          cl_sender            TYPE REF TO cl_cam_address_bcs,
*          t_attachment_header  TYPE soli_tab,
*          wa_attachment_header LIKE LINE OF t_attachment_header,
*          attachment_subject   TYPE sood-objdes,
*
*          sood_bytecount       TYPE sood-objlen,
*          mail_title           TYPE so_obj_des,
*          t_mailtext           TYPE soli_tab,
*          wa_mailtext          LIKE LINE OF t_mailtext,
*          send_to              TYPE adr6-smtp_addr,
*          sent                 TYPE abap_bool.
*
*
*    mail_title     = 'Mail title'.
*    wa_mailtext    = 'Mailtext'.
*    APPEND wa_mailtext TO t_mailtext.
*
*    TRY.
** Create send request
*        cl_send_request = cl_bcs=>create_persistent( ).
** Create new document with mailtitle and mailtextg
*        cl_document = cl_document_bcs=>create_document( i_type    = 'RAW' "#EC NOTEXT
*                                                        i_text    = t_mailtext
*                                                        i_subject = mail_title ).
** Add attachment to document
** since the new excelfiles have an 4-character extension .xlsx but the attachment-type only holds 3 charactes .xls,
** we have to specify the real filename via attachment header
** Use attachment_type xls to have SAP display attachment with the excel-icon
*        attachment_subject  = gc_save_file_name.
*        CONCATENATE '&SO_FILENAME=' attachment_subject INTO wa_attachment_header.
*        APPEND wa_attachment_header TO t_attachment_header.
** Attachment
*        sood_bytecount = bytecount.  " next method expects sood_bytecount instead of any positive integer *sigh*
*        cl_document->add_attachment(  i_attachment_type    = 'XLS' "#EC NOTEXT
*                                      i_attachment_subject = attachment_subject
*                                      i_attachment_size    = sood_bytecount
*                                      i_att_content_hex    = t_rawdata
*                                      i_attachment_header  = t_attachment_header ).
*
** add document to send request
*        cl_send_request->set_document( cl_document ).
*
** set sender in case if no own email is availabe
**        cl_sender  = cl_cam_address_bcs=>create_internet_address( 'sender@sender.sender' ).
**        cl_send_request->set_sender( cl_sender ).
*
** add recipient(s) - here only 1 will be needed
*        send_to = p_email.
*        IF send_to IS INITIAL.
*          send_to = 'no_email@no_email.no_email'.  " Place into SOST in any case for demonstration purposes
*        ENDIF.
*        cl_recipient = cl_cam_address_bcs=>create_internet_address( send_to ).
*        cl_send_request->add_recipient( cl_recipient ).
*
** Und abschicken
*        sent = cl_send_request->send( i_with_error_screen = 'X' ).
*
*        COMMIT WORK.
*
*        IF sent = abap_true.
*          MESSAGE s805(zabap2xlsx).
*          MESSAGE 'Document ready to be sent - Check SOST or SCOT' TYPE 'I'.
*        ELSE.
*          MESSAGE i804(zabap2xlsx) WITH p_email.
*        ENDIF.
*
*      CATCH cx_bcs INTO bcs_exception.
*        errortext = bcs_exception->if_message~get_text( ).
*        MESSAGE errortext TYPE 'I'.
*
*    ENDTRY.
  ENDMETHOD.                    "send_email


ENDCLASS.                    "lcl_output IMPLEMENTATION
