*&---------------------------------------------------------------------*
*& Report Z_METADATA_HANAVIEW
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report z_metadata_hanaview.

include z_metadata_declare.


selection-screen begin of screen 1001.

parameters: p_calc type c length 64 obligatory.
PARAMETERS: p_vers type i DEFAULT 0.
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

include z_metadata_form_f4.

at selection-screen  output.
  perform f4_ctx.

at selection-screen on value-request for p_calc.

  perform p_calcview_f4 changing p_calc.

at selection-screen on value-request for p_vers.

  perform p_calcview_vers_f4 changing p_vers.

start-of-selection.
*INITIALIZATION.
*
*  perform f4_ctx.
  call screen 1001.
  include z_metadata_elaborate.

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

include z_metadata_module_grid.
include z_metadata_grid_display.
include z_metadata_fieldcatalog.
include z_metadata_command.
