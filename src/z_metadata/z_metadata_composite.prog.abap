*&---------------------------------------------------------------------*
*& Report Z_METADATA_HANAVIEW
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report z_metadata_composite.

data: gr_table1  type ref to cl_gui_alv_grid.   "cl_salv_table.

data: container1  type ref to cl_gui_custom_container.


data: gt_fieldcatalog1  type lvc_t_fcat.

data: field_catalog type slis_t_fieldcat_alv.

data: g_okcode type syucomm.



selection-screen begin of screen 1001.

parameters: p_comp type RSOHCPRNM obligatory.
*parameters: p_ctx type esh_e_om_sysrel as listbox visible length 32 user-command p_ctx default 'HEAD' obligatory.
*parameters: p_var as checkbox.
*parameters: p_ds as checkbox.
*parameters: p_proj as checkbox.
*parameters: p_join as checkbox.
*parameters: p_union as checkbox.
*parameters: p_aggr as checkbox.
*parameters: p_rank as checkbox.
parameters: p_model as checkbox default abap_true.
*SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE block.
*
*SELECTION-SCREEN BEGIN OF LINE.
*
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK block.
selection-screen end of screen 1001.

*at selection-screen  output.
*  perform f4_ctx.

at selection-screen on value-request for p_comp.

  perform p_comp_f4 changing p_comp.

start-of-selection.
*INITIALIZATION.
*
*  perform f4_ctx.
  call screen 1001.
  if p_comp is not initial.
    call method zcl_metadata_COMPOSITE=>get_xml_from_composite
      exporting
        p_calcview = p_comp
      importing
        p_xml      = data(p_xml).

    call method zcl_metadata_composite=>p_parse_xml
      exporting
        p_xml              = p_xml
      importing
        p_dom              = data(p_dom)
        p_error            = data(p_error)
        p_strip_empty_text = data(p_strip_empty_text).


    if p_model = abap_true.
      call method zcl_metadata_composite=>get_details
        exporting
          tab_dom   = p_dom
        importing
          p_details = data(p_details).
    endif.
  endif.

end-of-selection.

  if p_comp is not initial.
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

IF p_model IS NOT INITIAL.
  perform grid1_display.
ENDIF.

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

    perform create_fieldcatalog using p_details changing gt_fieldcatalog1.

* call grid instance of tab strip 1
    call method gr_table1->set_table_for_first_display
      changing
        it_fieldcatalog = gt_fieldcatalog1
        it_outtab       = p_details.

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


  free gr_table1.

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
*&---------------------------------------------------------------------*
*&      Form  P_CALCVIEW_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_CALCVIEW  text
*----------------------------------------------------------------------*
form p_comp_f4  changing p_p_calcview.

  data:
    lv_value     type help_info-fldvalue,
    lt_return    type standard table of ddshretval,
    ls_odp_f4    type rodps_s_odp_f4,
    lt_odp_f4    like standard table of ls_odp_f4,
    lt_field_tab like dfies occurs 0 with header line,
    l_suffix     type c.


  data(et_odpdescr) = cl_rodps_default_context=>get_instance( i_context    = 'HANA'
                                                            i_use_buffer = 'X' )->get_odp_list( i_pattern     = ''
                                                                                                  i_langu       = sy-langu
                                                                                                  i_view        = abap_true ).
  sort et_odpdescr by odpname sysrel.
  delete adjacent duplicates from et_odpdescr comparing odpname sysrel.

  loop at et_odpdescr into data(ls_odpdescr).
*      CHECK p_sysrel IS INITIAL OR ls_odpdescr-sysrel = p_sysrel.
    move-corresponding ls_odpdescr to ls_odp_f4.
    split ls_odpdescr-odpname at '$' into ls_odp_f4-odpname l_suffix.
    append ls_odp_f4 to lt_odp_f4.
  endloop.
  sort lt_odp_f4 by odpname.

  select * from RSOHCPRT into table @data(I_comp) where LANGU = 'E' AND OBJVERS = 'A' AND COLNAME = ''.

  lt_field_tab-tabname = 'RSOHCPRT'.
*  lt_field_tab-fieldname = 'LANGU'.
*  append lt_field_tab.
  lt_field_tab-fieldname = 'HCPRNM'.
  append lt_field_tab.
*  lt_field_tab-fieldname = 'OBJVERS'.
*  append lt_field_tab.
*  lt_field_tab-fieldname = 'COLNAME'.
*  append lt_field_tab.
  lt_field_tab-fieldname = 'DESCRIPTION'.
  append lt_field_tab.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield   = 'HCPRNM'
      value_org  = 'S'
    tables
      value_tab  = I_comp
      return_tab = lt_return
      field_tab  = lt_field_tab
    exceptions
      others     = 1.
  if sy-subrc <> 0.
    message id sy-msgid type 'I' number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    exit.
  elseif not lt_return[] is initial.
    read table lt_return into data(ls_return) index 1.
  else.
    exit.
  endif.

  check not ls_return-fieldval is initial.
  p_comp = ls_return-fieldval.

endform.
*&---------------------------------------------------------------------*
*&      Form  F4_CTX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f4_ctx .

*  data: lt_value type vrm_values,
*        ls_value type vrm_value.
*
*
*
*  ls_value-key    = 'HEAD'.
*  ls_value-text  = 'Header'.
*
*  append ls_value to lt_value.
*
*  ls_value-key    = 'DET'.
*  ls_value-text = 'Details'.
*
*  append ls_value to lt_value.
*
*  ls_value-key    = 'HEDDET'.
*  ls_value-text = 'Both Header and Details'.
*
*  append ls_value to lt_value.
*
*  call function 'VRM_SET_VALUES'
*    exporting
*      id     = 'P_CTX'
*      values = lt_value
*    exceptions
*      others = 0.
*
*  read table lt_value transporting no fields
*       with key key = p_ctx.
*  if sy-subrc <> 0.
*
*  endif.
endform.

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
    ls_fieldcat-outputlen = strlen( ls_dfies-outputlen ).
    ls_fieldcat-col_opt = 'X'.
    ls_fieldcat-lowercase = 'X'.
    append ls_fieldcat to pt_fieldcat.
  endloop.
endform. "create_fieldcatalog
