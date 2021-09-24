*&---------------------------------------------------------------------*
*&  Include           Z_METADATA_FIELDCATALOG
*&---------------------------------------------------------------------*
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
    ls_fieldcat-col_opt = 'X'.
    append ls_fieldcat to pt_fieldcat.
  endloop.
endform. "create_fieldcatalog
