*&---------------------------------------------------------------------*
*&  Include           Z_METADATA_ELABORATE
*&---------------------------------------------------------------------*
 if p_calc is not initial.

   if p_vers ne 0.

         call method zcl_metadata_hanaview=>get_xml_from_hana_view
      exporting
        p_calcview = p_calc
        p_version = p_vers
      importing
        p_xml      = data(p_xml).

    call method zcl_metadata_hanaview=>p_parse_xml
      exporting
        p_xml              = p_xml
      importing
        p_dom              = data(p_dom)
        p_error            = data(p_error)
        p_strip_empty_text = data(p_strip_empty_text).
     else.
    call method zcl_metadata_hanaview=>p_get_parsed_snap
     exporting
       p_calcview = p_calc
       importing
         p_dom = p_dom.
endif.
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

      call method zcl_metadata_hanaview=>get_dependencies
        exporting
          tab_dom   = p_dom
        importing
          p_dependencies = data(p_DEP).

       call method zcl_metadata_hanaview=>get_dependencies_details
        exporting
          tab_dom   = p_dom
        importing
          p_dependencies_details = data(p_DEP_det).
    endif.
  endif.
