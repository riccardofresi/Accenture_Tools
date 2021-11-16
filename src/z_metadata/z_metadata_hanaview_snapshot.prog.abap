*&---------------------------------------------------------------------*
*& Report Z_METADATA_HANAVIEW_SNAPSHOT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_METADATA_HANAVIEW_SNAPSHOT.


 DATA: p_calc type c length 64, x type c LENGTH 250.

 data(et_odpdescr) = cl_rodps_default_context=>get_instance( i_context    = 'HANA'
                                                            i_use_buffer = 'X' )->get_odp_list( i_pattern     = ''
                                                                                                  i_langu       = sy-langu
                                                                                             i_view        = abap_true ).
  select  * into table @data(i_metadata) from  ZMETADATADOM where OBJECTTYPE = 'X'.
    READ TABLE i_metadata into data(l_metadata) INDEX 1.

    clear i_metadata. free i_metadata.
    clear l_metadata.

   loop at et_odpdescr into data(ls_odpdescr) where object_type_name is NOT INITIAL.

 p_calc = ls_odpdescr-object_type_name.

    call method zcl_metadata_hanaview=>get_xml_from_hana_view
      exporting
        p_calcview = p_calc
*        p_version = p_vers
      importing
        p_xml      = data(p_xml).

    call method zcl_metadata_hanaview=>p_parse_xml
      exporting
        p_xml              = p_xml
      importing
        p_dom              = data(p_dom)
        p_error            = data(p_error)
        p_strip_empty_text = data(p_strip_empty_text).

LOOP AT p_dom into data(l_dom).
  l_metadata-objecttype = 'CALCVIEW'.
  l_metadata-objectname = p_calc.
  MOVE-CORRESPONDING l_dom to l_metadata.
*  l_metadata-node_value = l_metadata-node_value(1000).
  x = l_metadata-node_value.
  l_metadata-node_value = x.

  APPEND l_metadata to i_metadata.

ENDLOOP.
  delete from ZMETADATADOM where objectname = p_calc.
  MODIFY ZMETADATADOM from TABLE i_metadata.
  COMMIT WORK.
  CLEAR: p_calc, l_dom, p_xml, p_error, p_strip_empty_text, p_dom, l_metadata, i_metadata.
  endloop.
