*&---------------------------------------------------------------------*
*&  Include           Z_METADATA_MODULE_GRID
*&---------------------------------------------------------------------*
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
                " D0100_USER_COMMAND

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
