*&---------------------------------------------------------------------*
*&  Include           Z_METADATA_DECLARE
*&---------------------------------------------------------------------*

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
