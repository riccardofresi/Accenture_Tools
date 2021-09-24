*&---------------------------------------------------------------------*
*&  Include           Z_METADATA_FORM_F4
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F4_CTX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f4_ctx .
*&---------------------------------------------------------------------*
*&      Form  F4_CTX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*


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
*&      Form  P_CALCVIEW_VERS_F4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_VERS  text
*----------------------------------------------------------------------*
form p_calcview_vers_f4  changing p_vers.

  data:
    lt_field_tab like dfies occurs 0 with header line,
    lt_return    type standard table of ddshretval,
    string type string.

DATA: BEGIN OF wa_tab,
          CalcView like /OSP/T_CORR_EXT-OBJECT_ID, " type c LENGTH 64,
          version like SFLCONN-period, " type i
          activation_date like sy-datum,
      END OF wa_tab,
      t_tab LIKE STANDARD TABLE OF wa_tab.

    PERFORM read_dynp USING 'p_calc' CHANGING P_CALC.

    TRANSLATE P_CALC TO UPPER CASE.

    if P_CALC is not initial.
    call method zcl_metadata_hanaview=>get_hana_view_version
      exporting
        p_calcview = P_CALC
      importing
        p_calc_vers = DATA(p_calc_version).


  LOOP AT p_calc_version into data(l_calc_version).
    wa_tab-calcview = l_calc_version-calcview.
    wa_tab-version = l_calc_version-version.
    wa_tab-activation_date = l_calc_version-activation_date.
    append wa_tab to t_tab.
  ENDLOOP.

*  t_tab[] = p_calc_version[].

  lt_field_tab-fieldname = 'CALCVIEW'.
  lt_field_tab-position = 1.
  lt_field_tab-offset = 0.
  lt_field_tab-intlen = 128.
  lt_field_tab-outputlen = 64.
  lt_field_tab-datatype = 'CHAR'.
  lt_field_tab-inttype = 'C'.
  lt_field_tab-reptext = 'Calculation View'.
  append lt_field_tab.

  lt_field_tab-fieldname = 'VERSION'.
  lt_field_tab-position = 2.
  lt_field_tab-offset = 128.
  lt_field_tab-intlen = 1.
  lt_field_tab-outputlen = 3.
  lt_field_tab-datatype = 'INT1'.
  lt_field_tab-inttype = 'b'.
  lt_field_tab-reptext = 'Version'.
  append lt_field_tab.

  lt_field_tab-fieldname = 'ACTIVATION_DATE'.
  lt_field_tab-position = 3.
  lt_field_tab-offset = 130.
  lt_field_tab-intlen = 16.
  lt_field_tab-leng = 8.
  lt_field_tab-outputlen = 10.
  lt_field_tab-datatype = 'DATS'.
  lt_field_tab-inttype = 'D'.
  lt_field_tab-reptext = 'Activation Date'.
  append lt_field_tab.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield   = 'VERSION'
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
  CONDENSE ls_return-fieldVAL NO-GAPS.
  p_vers =  ls_return-fieldVAL .

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
