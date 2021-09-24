*&---------------------------------------------------------------------*
*&  Include           Z_METADATA_GRID_DISPLAY
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GRID1_DISPLAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
DEFINE CONTAINER.
"&1 container1
"&2 'CONTAINER1'
"&3 gr_table1
"&4 gt_fieldcatalog1
  if &1  is not bound.
    if cl_salv_table=>is_offline( ) eq if_salv_c_bool_sap=>false.
      create object &1
        exporting
          container_name = &2.
    endif.

* create grid instance of tabstrip 1
    create object &3
      exporting
        i_parent = &1.
    if sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

* set handle for layout variants
    data: ls_variant type disvariant.
    ls_variant-report = sy-repid.
    ls_variant-handle = '001'.

    perform create_fieldcatalog using &5 changing &4.

* call grid instance of tab strip 1
    call method &3->set_table_for_first_display
*      exportXing
*        i_structure_name = 'ALV_T_T2'
*       is_variant       = ls_variant
*       i_save           = 'A'
      changing
        it_fieldcatalog = &4
        it_outtab       = &5.
  else.
    call method &3->refresh_table_display.
  endif.

END-OF-DEFINITION.


form grid1_display .

*set pf-status 'D0101'.
CONTAINER container1 'CONTAINER1' gr_table1 gt_fieldcatalog1 p_details.

endform.                    " GRID1_DISPLAY
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
CONTAINER container2 'CONTAINER2' gr_table2 gt_fieldcatalog2 p_variables.

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
CONTAINER container3 'CONTAINER3' gr_table3 gt_fieldcatalog3 p_datasources.

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
CONTAINER container4 'CONTAINER4' gr_table4 gt_fieldcatalog4 p_proj_header.

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
CONTAINER container5 'CONTAINER5' gr_table5 gt_fieldcatalog5 p_proj_details.

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
CONTAINER container6 'CONTAINER6' gr_table6 gt_fieldcatalog6 p_join_header.

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
CONTAINER container7 'CONTAINER7' gr_table7 gt_fieldcatalog7 p_join_details.

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
CONTAINER container8 'CONTAINER8' gr_table8 gt_fieldcatalog8 p_union_header.

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
CONTAINER container9 'CONTAINER9' gr_table9 gt_fieldcatalog9 p_union_details.

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
CONTAINER container10 'CONTAINER10' gr_table10 gt_fieldcatalog10 p_aggr_header.

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
CONTAINER container11 'CONTAINER11' gr_table11 gt_fieldcatalog11 p_aggr_details.

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
CONTAINER container12 'CONTAINER12' gr_table12 gt_fieldcatalog12 p_rank_header.

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
CONTAINER container13 'CONTAINER13' gr_table13 gt_fieldcatalog13 p_rank_details.

endform.
