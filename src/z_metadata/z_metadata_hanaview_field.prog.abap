*&---------------------------------------------------------------------*
*& Report Z_METADATA_HANAVIEW
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report z_metadata_hanaview_field.

data: gr_table1  type ref to cl_gui_alv_grid,   "cl_salv_table,
      gr_table2  type ref to cl_gui_alv_grid,   "cl_salv_table,
      gr_table3  type ref to cl_gui_alv_grid,   "cl_salv_table,
      gr_table4  type ref to cl_gui_alv_grid,   "cl_salv_table,
      gr_table5  type ref to cl_gui_alv_grid,   "cl_salv_table,
      gr_table6  type ref to cl_gui_alv_grid,   "cl_salv_table,
      gr_table7  type ref to cl_gui_alv_grid,   "cl_salv_table,
      gr_table8  type ref to cl_gui_alv_grid,   "cl_salv_table,
      gr_table9  type ref to cl_gui_alv_grid,   "cl_salv_table,
      gr_table10 type ref to cl_gui_alv_grid,   "cl_salv_table,
      gr_table11 type ref to cl_gui_alv_grid,   "cl_salv_table,
      gr_table12 type ref to cl_gui_alv_grid,   "cl_salv_table,
      gr_table13 type ref to cl_gui_alv_grid.   "cl_salv_table.

data: container1  type ref to cl_gui_custom_container,
      container2  type ref to cl_gui_custom_container,
      container3  type ref to cl_gui_custom_container,
      container4  type ref to cl_gui_custom_container,
      container5  type ref to cl_gui_custom_container,
      container6  type ref to cl_gui_custom_container,
      container7  type ref to cl_gui_custom_container,
      container8  type ref to cl_gui_custom_container,
      container9  type ref to cl_gui_custom_container,
      container10 type ref to cl_gui_custom_container,
      container11 type ref to cl_gui_custom_container,
      container12 type ref to cl_gui_custom_container,
      container13 type ref to cl_gui_custom_container.


data: gt_fieldcatalog1  type lvc_t_fcat,
      gt_fieldcatalog2  type lvc_t_fcat,
      gt_fieldcatalog3  type lvc_t_fcat,
      gt_fieldcatalog4  type lvc_t_fcat,
      gt_fieldcatalog5  type lvc_t_fcat,
      gt_fieldcatalog6  type lvc_t_fcat,
      gt_fieldcatalog7  type lvc_t_fcat,
      gt_fieldcatalog8  type lvc_t_fcat,
      gt_fieldcatalog9  type lvc_t_fcat,
      gt_fieldcatalog10 type lvc_t_fcat,
      gt_fieldcatalog11 type lvc_t_fcat,
      gt_fieldcatalog12 type lvc_t_fcat,
      gt_fieldcatalog13 type lvc_t_fcat.

data: field_catalog type slis_t_fieldcat_alv.

data: g_okcode type syucomm.

* structure for Form READ_DYNP
DATA: BEGIN OF tb_read OCCURS 0,
          fieldname  TYPE dynpread-fieldname,
          stepl      TYPE dynpread-stepl,
          fieldvalue TYPE dynpread-fieldvalue,
          fieldinp   TYPE dynpread-fieldinp,
      END OF tb_read.
DATA: v_prog        LIKE d020s-prog,
      v_dyn         LIKE d020s-dnum.

selection-screen begin of screen 1001.

parameters: p_calc type c length 64 obligatory.
parameters: p_field type c length 64 obligatory.
parameters: p_ctx type esh_e_om_sysrel as listbox visible length 32 user-command p_ctx default 'HEAD' obligatory.
parameters: p_var as checkbox.
parameters: p_ds as checkbox.
parameters: p_proj as checkbox.
parameters: p_join as checkbox.
parameters: p_union as checkbox.
parameters: p_aggr as checkbox.
parameters: p_rank as checkbox.
parameters: p_model as checkbox default abap_true.
*SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME TITLE block.
*
*SELECTION-SCREEN BEGIN OF LINE.
*
*SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK block.
selection-screen end of screen 1001.

at selection-screen  output.
  perform f4_ctx.

at selection-screen on value-request for p_calc.

  perform p_calcview_f4 changing p_calc.

at selection-screen on value-request for p_field.

  perform p_field_f4 changing p_field.

start-of-selection.
*INITIALIZATION.
*
*  perform f4_ctx.
  call screen 1001.
  if p_calc is not initial.
    call method zcl_metadata_hanaview=>get_xml_from_hana_view
      exporting
        p_calcview = p_calc
      importing
        p_xml      = data(p_xml).

    call method zcl_metadata_hanaview=>p_parse_xml
      exporting
        p_xml              = p_xml
      importing
        p_dom              = data(p_dom)
        p_error            = data(p_error)
        p_strip_empty_text = data(p_strip_empty_text).

    if p_var = abap_true.
      call method zcl_metadata_hanaview=>get_variables
        exporting
          tab_dom     = p_dom
        importing
          p_variables = data(p_variables).
    endif.

    if p_ds = abap_true.
      call method zcl_metadata_hanaview=>get_datasources
        exporting
          tab_dom       = p_dom
        importing
          p_datasources = data(p_datasources).
    endif.

    if p_proj = abap_true and ( p_ctx = 'HEAD').
      call method zcl_metadata_hanaview=>get_proj_header
        exporting
          tab_dom       = p_dom
        importing
          p_proj_header = data(p_proj_header).
    elseif p_proj = abap_true and ( p_ctx = 'DET').
      call method zcl_metadata_hanaview=>get_proj_details
        exporting
          tab_dom        = p_dom
        importing
          p_proj_details = data(p_proj_details).
    elseif p_proj = abap_true and ( p_ctx = 'HEDDET').
      call method zcl_metadata_hanaview=>get_proj_header
        exporting
          tab_dom       = p_dom
        importing
          p_proj_header = p_proj_header.
      call method zcl_metadata_hanaview=>get_proj_details
        exporting
          tab_dom        = p_dom
        importing
          p_proj_details = p_proj_details.
    endif.

    if p_join = abap_true and ( p_ctx = 'HEAD').
      call method zcl_metadata_hanaview=>get_join_header
        exporting
          tab_dom       = p_dom
        importing
          p_join_header = data(p_join_header).
    elseif p_join = abap_true and ( p_ctx = 'DET').
      call method zcl_metadata_hanaview=>get_join_details
        exporting
          tab_dom        = p_dom
        importing
          p_join_details = data(p_join_details).
    ELSEIF p_join = abap_true and ( p_ctx = 'HEDDET').
      call method zcl_metadata_hanaview=>get_join_header
        exporting
          tab_dom       = p_dom
        importing
          p_join_header = p_join_header.
      call method zcl_metadata_hanaview=>get_join_details
        exporting
          tab_dom        = p_dom
        importing
          p_join_details = p_join_details.
    endif.

    if p_union = abap_true and ( p_ctx = 'HEAD').
      call method zcl_metadata_hanaview=>get_union_header
        exporting
          tab_dom        = p_dom
        importing
          p_union_header = data(p_union_header).
    elseif p_union = abap_true and ( p_ctx = 'DET').
      call method zcl_metadata_hanaview=>get_union_details
        exporting
          tab_dom         = p_dom
        importing
          p_union_details = data(p_union_details).
      elseif p_union = abap_true and ( p_ctx = 'HEDDET').
        call method zcl_metadata_hanaview=>get_union_header
        exporting
          tab_dom        = p_dom
        importing
          p_union_header = p_union_header.
        call method zcl_metadata_hanaview=>get_union_details
        exporting
          tab_dom         = p_dom
        importing
          p_union_details = p_union_details.
    endif.

    if p_aggr = abap_true and ( p_ctx = 'HEAD').
      call method zcl_metadata_hanaview=>get_aggr_header
        exporting
          tab_dom       = p_dom
        importing
          p_aggr_header = data(p_aggr_header).
    elseif p_aggr = abap_true and ( p_ctx = 'DET').
      call method zcl_metadata_hanaview=>get_aggr_details
        exporting
          tab_dom        = p_dom
        importing
          p_aggr_details = data(p_aggr_details).
      elseif p_aggr = abap_true and ( p_ctx = 'HEDDET').
        call method zcl_metadata_hanaview=>get_aggr_header
        exporting
          tab_dom       = p_dom
        importing
          p_aggr_header = p_aggr_header.
        call method zcl_metadata_hanaview=>get_aggr_details
        exporting
          tab_dom        = p_dom
        importing
          p_aggr_details = p_aggr_details.
    endif.

    if p_rank = abap_true and ( p_ctx = 'HEAD' ).
      call method zcl_metadata_hanaview=>get_rank_header
        exporting
          tab_dom       = p_dom
        importing
          p_rank_header = data(p_rank_header).
    elseif p_aggr = abap_true and ( p_ctx = 'DET' ).
      call method zcl_metadata_hanaview=>get_rank_details
        exporting
          tab_dom        = p_dom
        importing
          p_rank_details = data(p_rank_details).
      elseif p_aggr = abap_true and ( p_ctx = 'HEDDET' ).
              call method zcl_metadata_hanaview=>get_rank_header
        exporting
          tab_dom       = p_dom
        importing
          p_rank_header = p_rank_header.
              call method zcl_metadata_hanaview=>get_rank_details
        exporting
          tab_dom        = p_dom
        importing
          p_rank_details = p_rank_details.
    endif.

    if p_model = abap_true.
      call method zcl_metadata_hanaview=>get_details
        exporting
          tab_dom   = p_dom
        importing
          p_details = data(p_details).
    endif.
  endif.

end-of-selection.

  if p_calc is not initial.
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
             tab7  like sy-ucomm value 'TAB_APPL_FC7',
             tab8  like sy-ucomm value 'TAB_APPL_FC8',
             tab9  like sy-ucomm value 'TAB_APPL_FC9',
             tab10 like sy-ucomm value 'TAB_APPL_FC10',
             tab11 like sy-ucomm value 'TAB_APPL_FC11',
             tab12 like sy-ucomm value 'TAB_APPL_FC12',
             tab13 like sy-ucomm value 'TAB_APPL_FC13',
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
    when c_tab_appl-tab7.
      g_tab_appl-subscreen = '0107'.
    when c_tab_appl-tab8.
      g_tab_appl-subscreen = '0108'.
    when c_tab_appl-tab9.
      g_tab_appl-subscreen = '0109'.
    when c_tab_appl-tab10.
      g_tab_appl-subscreen = '0110'.
    when c_tab_appl-tab11.
      g_tab_appl-subscreen = '0111'.
    when c_tab_appl-tab12.
      g_tab_appl-subscreen = '0112'.
    when c_tab_appl-tab13.
      g_tab_appl-subscreen = '0113'.
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
    when c_tab_appl-tab7.
      g_tab_appl-pressed_tab = c_tab_appl-tab7.
    when c_tab_appl-tab8.
      g_tab_appl-pressed_tab = c_tab_appl-tab8.
    when c_tab_appl-tab9.
      g_tab_appl-pressed_tab = c_tab_appl-tab9.
    when c_tab_appl-tab10.
      g_tab_appl-pressed_tab = c_tab_appl-tab10.
    when c_tab_appl-tab11.
      g_tab_appl-pressed_tab = c_tab_appl-tab11.
    when c_tab_appl-tab12.
      g_tab_appl-pressed_tab = c_tab_appl-tab12.
    when c_tab_appl-tab13.
      g_tab_appl-pressed_tab = c_tab_appl-tab13.
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

module display_grid2 output.

IF p_variables IS NOT INITIAL.
  perform grid2_display.
ENDIF.
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

module display_grid7 output.

  perform grid7_display.

endmodule.                 " DISPLAY_GRID  OUTPUT

module display_grid8 output.

  perform grid8_display.

endmodule.                 " DISPLAY_GRID  OUTPUT

module display_grid9 output.

  perform grid9_display.

endmodule.                 " DISPLAY_GRID  OUTPUT

module display_grid10 output.

  perform grid10_display.

endmodule.                 " DISPLAY_GRID  OUTPUT

module display_grid11 output.

  perform grid11_display.

endmodule.                 " DISPLAY_GRID  OUTPUT

module display_grid12 output.

  perform grid12_display.

endmodule.                 " DISPLAY_GRID  OUTPUT

module display_grid13 output.

  perform grid13_display.

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
*      exportXing
*        i_structure_name = 'ALV_T_T2'
*       is_variant       = ls_variant
*       i_save           = 'A'
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
form p_calcview_f4  changing p_p_calcview.

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

  lt_field_tab-tabname = 'RODPS_S_ODP_F4'.
  lt_field_tab-fieldname = 'ODPNAME'.
  append lt_field_tab.
  lt_field_tab-fieldname = 'SYSREL'.
  append lt_field_tab.
  lt_field_tab-fieldname = 'OBJECT_TYPE_NAME'.
  append lt_field_tab.
  lt_field_tab-fieldname = 'OLTPSOURCE'.
  append lt_field_tab.
  lt_field_tab-fieldname = 'ODPDESCR'.
  append lt_field_tab.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield   = 'OBJECT_TYPE_NAME'
      value_org  = 'S'
    tables
      value_tab  = lt_odp_f4
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
  p_calc = ls_return-fieldval.

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

  data: lt_value type vrm_values,
        ls_value type vrm_value.



  ls_value-key    = 'HEAD'.
  ls_value-text  = 'Header'.

  append ls_value to lt_value.

  ls_value-key    = 'DET'.
  ls_value-text = 'Details'.

  append ls_value to lt_value.

  ls_value-key    = 'HEDDET'.
  ls_value-text = 'Both Header and Details'.

  append ls_value to lt_value.

  call function 'VRM_SET_VALUES'
    exporting
      id     = 'P_CTX'
      values = lt_value
    exceptions
      others = 0.

  read table lt_value transporting no fields
       with key key = p_ctx.
  if sy-subrc <> 0.

  endif.
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

    perform create_fieldcatalog using p_variables changing gt_fieldcatalog2.

* call grid instance of tab strip 1
    call method gr_table2->set_table_for_first_display
*      exportXing
*        i_structure_name = 'ALV_T_T2'
*       is_variant       = ls_variant
*       i_save           = 'A'
      changing
        it_fieldcatalog = gt_fieldcatalog2
        it_outtab       = p_variables.
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

    perform create_fieldcatalog using p_datasources changing gt_fieldcatalog3.

* call grid instance of tab strip 1
    call method gr_table3->set_table_for_first_display
*      exportXing
*        i_structure_name = 'ALV_T_T2'
*       is_variant       = ls_variant
*       i_save           = 'A'
      changing
        it_fieldcatalog = gt_fieldcatalog3
        it_outtab       = p_datasources.
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

    perform create_fieldcatalog using p_join_header changing gt_fieldcatalog6.

* call grid instance of tab strip 1
    call method gr_table6->set_table_for_first_display
*      exportXing
*        i_structure_name = 'ALV_T_T2'
*       is_variant       = ls_variant
*       i_save           = 'A'
      changing
        it_fieldcatalog = gt_fieldcatalog6
        it_outtab       = p_join_header.
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
form grid7_display .

*set pf-status 'D0101'.

* create container of tabstrip 2
  if container7 is not bound.
    if cl_salv_table=>is_offline( ) eq if_salv_c_bool_sap=>false.
      create object container7
        exporting
          container_name = 'CONTAINER7'.
    endif.

* create grid instance of tabstrip 1
    create object gr_table7
      exporting
        i_parent = container7.
    if sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

* set handle for layout variants
    data: ls_variant type disvariant.
    ls_variant-report = sy-repid.
    ls_variant-handle = '001'.

    perform create_fieldcatalog using p_join_details changing gt_fieldcatalog7.

* call grid instance of tab strip 1
    call method gr_table7->set_table_for_first_display
*      exportXing
*        i_structure_name = 'ALV_T_T2'
*       is_variant       = ls_variant
*       i_save           = 'A'
      changing
        it_fieldcatalog = gt_fieldcatalog7
        it_outtab       = p_join_details.
  else.
    call method gr_table7->refresh_table_display.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  GRID8_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form grid8_display .

*set pf-status 'D0101'.

* create container of tabstrip 2
  if container8 is not bound.
    if cl_salv_table=>is_offline( ) eq if_salv_c_bool_sap=>false.
      create object container8
        exporting
          container_name = 'CONTAINER8'.
    endif.

* create grid instance of tabstrip 1
    create object gr_table8
      exporting
        i_parent = container8.
    if sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

* set handle for layout variants
    data: ls_variant type disvariant.
    ls_variant-report = sy-repid.
    ls_variant-handle = '001'.

    perform create_fieldcatalog using p_union_header changing gt_fieldcatalog8.

* call grid instance of tab strip 1
    call method gr_table8->set_table_for_first_display
*      exportXing
*        i_structure_name = 'ALV_T_T2'
*       is_variant       = ls_variant
*       i_save           = 'A'
      changing
        it_fieldcatalog = gt_fieldcatalog8
        it_outtab       = p_union_header.
  else.
    call method gr_table8->refresh_table_display.
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
form grid9_display .

*set pf-status 'D0101'.

* create container of tabstrip 2
  if container9 is not bound.
    if cl_salv_table=>is_offline( ) eq if_salv_c_bool_sap=>false.
      create object container9
        exporting
          container_name = 'CONTAINER9'.
    endif.

* create grid instance of tabstrip 1
    create object gr_table9
      exporting
        i_parent = container9.
    if sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

* set handle for layout variants
    data: ls_variant type disvariant.
    ls_variant-report = sy-repid.
    ls_variant-handle = '001'.

    perform create_fieldcatalog using p_union_details changing gt_fieldcatalog9.

* call grid instance of tab strip 1
    call method gr_table9->set_table_for_first_display
*      exportXing
*        i_structure_name = 'ALV_T_T2'
*       is_variant       = ls_variant
*       i_save           = 'A'
      changing
        it_fieldcatalog = gt_fieldcatalog9
        it_outtab       = p_union_details.
  else.
    call method gr_table9->refresh_table_display.
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
form grid10_display .

*set pf-status 'D0101'.

* create container of tabstrip 2
  if container10 is not bound.
    if cl_salv_table=>is_offline( ) eq if_salv_c_bool_sap=>false.
      create object container10
        exporting
          container_name = 'CONTAINER10'.
    endif.

* create grid instance of tabstrip 1
    create object gr_table10
      exporting
        i_parent = container10.
    if sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

* set handle for layout variants
    data: ls_variant type disvariant.
    ls_variant-report = sy-repid.
    ls_variant-handle = '001'.

    perform create_fieldcatalog using p_aggr_header changing gt_fieldcatalog10.

* call grid instance of tab strip 1
    call method gr_table10->set_table_for_first_display
*      exportXing
*        i_structure_name = 'ALV_T_T2'
*       is_variant       = ls_variant
*       i_save           = 'A'
      changing
        it_fieldcatalog = gt_fieldcatalog10
        it_outtab       = p_aggr_header.
  else.
    call method gr_table10->refresh_table_display.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  GRID11_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form grid11_display .

*set pf-status 'D0101'.

* create container of tabstrip 2
  if container11 is not bound.
    if cl_salv_table=>is_offline( ) eq if_salv_c_bool_sap=>false.
      create object container11
        exporting
          container_name = 'CONTAINER11'.
    endif.

* create grid instance of tabstrip 1
    create object gr_table11
      exporting
        i_parent = container11.
    if sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

* set handle for layout variants
    data: ls_variant type disvariant.
    ls_variant-report = sy-repid.
    ls_variant-handle = '001'.

    perform create_fieldcatalog using p_aggr_details changing gt_fieldcatalog11.

* call grid instance of tab strip 1
    call method gr_table11->set_table_for_first_display
*      exportXing
*        i_structure_name = 'ALV_T_T2'
*       is_variant       = ls_variant
*       i_save           = 'A'
      changing
        it_fieldcatalog = gt_fieldcatalog11
        it_outtab       = p_aggr_details.
  else.
    call method gr_table11->refresh_table_display.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  GRID12_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form grid12_display .

*set pf-status 'D0101'.

* create container of tabstrip 2
  if container12 is not bound.
    if cl_salv_table=>is_offline( ) eq if_salv_c_bool_sap=>false.
      create object container12
        exporting
          container_name = 'CONTAINER12'.
    endif.

* create grid instance of tabstrip 1
    create object gr_table12
      exporting
        i_parent = container12.
    if sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

* set handle for layout variants
    data: ls_variant type disvariant.
    ls_variant-report = sy-repid.
    ls_variant-handle = '001'.

    perform create_fieldcatalog using p_rank_header changing gt_fieldcatalog12.

* call grid instance of tab strip 1
    call method gr_table12->set_table_for_first_display
*      exportXing
*        i_structure_name = 'ALV_T_T2'
*       is_variant       = ls_variant
*       i_save           = 'A'
      changing
        it_fieldcatalog = gt_fieldcatalog12
        it_outtab       = p_rank_header.
  else.
    call method gr_table12->refresh_table_display.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  GRID13_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form grid13_display .

*set pf-status 'D0101'.

* create container of tabstrip 2
  if container13 is not bound.
    if cl_salv_table=>is_offline( ) eq if_salv_c_bool_sap=>false.
      create object container13
        exporting
          container_name = 'CONTAINER13'.
    endif.

* create grid instance of tabstrip 1
    create object gr_table13
      exporting
        i_parent = container13.
    if sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

* set handle for layout variants
    data: ls_variant type disvariant.
    ls_variant-report = sy-repid.
    ls_variant-handle = '001'.

    perform create_fieldcatalog using p_rank_details changing gt_fieldcatalog13.

* call grid instance of tab strip 1
    call method gr_table13->set_table_for_first_display
*      exportXing
*        i_structure_name = 'ALV_T_T2'
*       is_variant       = ls_variant
*       i_save           = 'A'
      changing
        it_fieldcatalog = gt_fieldcatalog13
        it_outtab       = p_rank_details.
  else.
    call method gr_table13->refresh_table_display.
  endif.

endform.
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

    perform create_fieldcatalog using p_proj_header changing gt_fieldcatalog4.

* call grid instance of tab strip 1
    call method gr_table4->set_table_for_first_display
*      exportXing
*        i_structure_name = 'ALV_T_T2'
*       is_variant       = ls_variant
*       i_save           = 'A'
      changing
        it_fieldcatalog = gt_fieldcatalog4
        it_outtab       = p_proj_header.
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

    perform create_fieldcatalog using p_proj_details changing gt_fieldcatalog5.

* call grid instance of tab strip 1
    call method gr_table5->set_table_for_first_display
*      exportXing
*        i_structure_name = 'ALV_T_T2'
*       is_variant       = ls_variant
*       i_save           = 'A'
      changing
        it_fieldcatalog = gt_fieldcatalog5
        it_outtab       = p_proj_details.
  else.
    call method gr_table5->refresh_table_display.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  P_FIELD_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_FIELD  text
*----------------------------------------------------------------------*
form p_field_f4 changing p_field.

  data:
    lt_field_tab like dfies occurs 0 with header line,
    lt_return    type standard table of ddshretval.

DATA: BEGIN OF wa_tab,
          fieldname type c LENGTH 30,
      END OF wa_tab,
      t_tab LIKE STANDARD TABLE OF wa_tab.

    PERFORM read_dynp USING 'p_calc' CHANGING P_CALC.

    TRANSLATE P_CALC TO UPPER CASE.

    if P_CALC is not initial.
    call method zcl_metadata_hanaview=>get_xml_from_hana_view
      exporting
        p_calcview = P_CALC
      importing
        p_xml      = data(p_xml).

    call method zcl_metadata_hanaview=>p_parse_xml
      exporting
        p_xml              = p_xml
      importing
        p_dom              = data(p_dom)
        p_error            = data(p_error)
        p_strip_empty_text = data(p_strip_empty_text).


     call method zcl_metadata_hanaview=>get_details
        exporting
          tab_dom   = p_dom
        importing
          p_details = data(p_details).

  LOOP AT p_details into data(l_data).
    wa_tab-fieldname = l_data-id_field.
    append wa_tab to t_tab.
  ENDLOOP.

  lt_field_tab-fieldname = 'FIELDNAME'.
  lt_field_tab-intlen = 50.
  lt_field_tab-outputlen = 50.
  append lt_field_tab.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield   = 'FIELDNAME'
      value_org  = 'S'
    tables
      value_tab  = t_tab
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

  check not ls_return-fieldVAL is initial.
  p_FIELD = ls_return-fieldVAL.

ENDIF.

endform.
*&---------------------------------------------------------------------*
*&      Form  READ_DYNP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2643   text
*      <--P_P_QUERY  text
*----------------------------------------------------------------------*
form read_dynp  USING pi_field CHANGING p_value.

  DATA: p_field TYPE string.
  p_field = pi_field.
  TRANSLATE p_field TO UPPER CASE.
  CLEAR tb_read.
  REFRESH tb_read.
  tb_read-fieldname = p_field.
  APPEND tb_read.
  v_prog = sy-repid.
  v_dyn = sy-dynnr.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = v_prog
      dynumb     = v_dyn
    TABLES
      dynpfields = tb_read.
  READ TABLE tb_read INDEX 1.
  IF tb_read-fieldvalue IS NOT INITIAL.
    p_value = tb_read-fieldvalue.
  ENDIF.

endform.
