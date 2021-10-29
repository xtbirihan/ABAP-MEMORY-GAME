*&---------------------------------------------------------------------*
*& Report ymemory_game
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ymemory_game.

CLASS memory_game DEFINITION.

  PUBLIC SECTION.
    METHODS:
      constructor,
      display.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF t_cell,
        id                TYPE string,
        value             TYPE i,
        html_class        TYPE string,
        html_event        TYPE string,
        open_tag          TYPE string,
        close_tag         TYPE string,
        number            TYPE i,
        color             TYPE char20,
        previous_selected TYPE xfeld,
        last_selected     TYPE xfeld,
        is_matched        TYPE xfeld,
      END OF t_cell,
      t_cells TYPE STANDARD TABLE OF t_cell  WITH EMPTY KEY.

    DATA: random_table TYPE t_cells.
    DATA: html                TYPE string,
          html_beginning_part TYPE string,
          html_closing_part   TYPE string,
          html_win            TYPE string.
    METHODS:
      at_click FOR EVENT sapevent OF cl_abap_browser IMPORTING action,
      call_random_number IMPORTING iv_size     TYPE i
                         RETURNING VALUE(rv_i) TYPE i.
ENDCLASS.

CLASS memory_game IMPLEMENTATION.
  METHOD constructor.

    TYPES: lty_numbers   TYPE TABLE OF i WITH EMPTY KEY.
    DATA: lt_numbers      TYPE lty_numbers,
          lt_numbers_copy TYPE lty_numbers,
          lv_i            TYPE i.


    html_beginning_part =
   `<!DOCTYPE html>` &&
   `<html lang="en">`  &&
   `<head>` &&
   `<meta charset="UTF-8" />`  &&
   `<meta`  &&
   `name="viewport"`  &&
   `content="width=device-width, initial-scale=1.0"`  &&
   `/>`  &&
   `<meta http-equiv="X-UA-Compatible" content="ie=edge" />`  &&
   `<title>Memory Games</title>`  &&
   `<style>`  &&
   `.row {  text-align: center; }`  &&
   `.card { display: inline-block;  width: 100px; height: 100px;   background-color: #eee; cursor: pointer;  }`  &&
   `.card.done { cursor: default;  }` &&
   `.card.done:hover { cursor: default; box-shadow: none; }` &&
   `.card:hover { box-shadow: inset 0px 0px 0px 1px red; box-sizing: border-box; }`  &&
   `.red { background-color: red; }`  &&
   `.green { background-color: green; }` &&
   `.blue { background-color: blue; }`  &&
   `.orange { background-color: orange; }`  &&
   `.cyan { background-color: cyan; }`  &&
   `.yellow{ background-color: yellow; }` &&
   ` .pink {  background-color: pink; }`  &&
   `.teal{ background-color: teal; }`  &&
   `.color-hidden { background-color: #eee; }`  &&
   ` </style>`  &&
   ` <script>`  &&
   `function onCardClicked(event) { window.location = event; }`  &&
   `</script>`  &&
   `</head>`  &&
   `<body>` .

    html_closing_part = `</body>` && `</html>`.

    lt_numbers =  VALUE #( ( 1 ) ( 1 ) ( 2 ) ( 2 ) ( 3 ) ( 3 ) ( 4 ) ( 4 ) ( 5 ) ( 5 ) ( 6 )  ( 6 ) ( 7 ) ( 7 ) ( 8 ) ( 8 ) ).
    DATA(lv_size) = lines( lt_numbers ).

    WHILE lv_size > 0.
      DO.
        lv_i = me->call_random_number( iv_size = lv_size ).
        IF lv_i NE 0.
          EXIT.
        ENDIF.
      ENDDO.
      DATA(ls_numbers) = lt_numbers[ lv_i  ].
      DELETE lt_numbers INDEX  lv_i.
      APPEND ls_numbers TO lt_numbers_copy.

      lv_size = lines( lt_numbers ).
    ENDWHILE.

    lt_numbers = lt_numbers_copy.

    random_table = VALUE #( FOR i = 1 UNTIL i > 4
                            FOR j = 1 UNTIL j > 4
                           (  id = |{ i }{ j }|
                              html_class = |<div class="card color-hidden" |
                              html_event = |onclick="onCardClicked('sapevent:{ i }{ j }');"></div>|
                              open_tag   = |{ COND #( WHEN j = 1  THEN |<div class="row">| ELSE `` ) }|
                              close_tag  = |{ COND #( WHEN j = 4  THEN |</div>| ELSE `` ) }|
                              number     = lt_numbers[ COND #( WHEN i = 1 THEN j
                                                               WHEN i = 2 THEN j + 4
                                                               WHEN i = 3 THEN j + 8
                                                               WHEN i = 4 THEN j + 12  )  ]
                              color      = COND #( WHEN lt_numbers[ COND #( WHEN i = 1 THEN j
                                                                            WHEN i = 2 THEN j + 4
                                                                            WHEN i = 3 THEN j + 8
                                                                            WHEN i = 4 THEN j + 12  )  ]   = 1 THEN 'red'
                                                   WHEN lt_numbers[ COND #( WHEN i = 1 THEN j
                                                                            WHEN i = 2 THEN j + 4
                                                                            WHEN i = 3 THEN j + 8
                                                                            WHEN i = 4 THEN j + 12  )  ]   = 2 THEN 'green'
                                                   WHEN lt_numbers[ COND #( WHEN i = 1 THEN j
                                                                            WHEN i = 2 THEN j + 4
                                                                            WHEN i = 3 THEN j + 8
                                                                            WHEN i = 4 THEN j + 12  )  ]   = 3 THEN 'blue'
                                                   WHEN lt_numbers[ COND #( WHEN i = 1 THEN j
                                                                            WHEN i = 2 THEN j + 4
                                                                            WHEN i = 3 THEN j + 8
                                                                            WHEN i = 4 THEN j + 12  )  ]   = 4 THEN 'orange'
                                                   WHEN lt_numbers[ COND #( WHEN i = 1 THEN j
                                                                            WHEN i = 2 THEN j + 4
                                                                            WHEN i = 3 THEN j + 8
                                                                            WHEN i = 4 THEN j + 12  )  ]   = 5 THEN 'cyan'
                                                   WHEN lt_numbers[ COND #( WHEN i = 1 THEN j
                                                                            WHEN i = 2 THEN j + 4
                                                                            WHEN i = 3 THEN j + 8
                                                                            WHEN i = 4 THEN j + 12  )  ]   = 6 THEN 'yellow'
                                                   WHEN lt_numbers[ COND #( WHEN i = 1 THEN j
                                                                            WHEN i = 2 THEN j + 4
                                                                            WHEN i = 3 THEN j + 8
                                                                            WHEN i = 4 THEN j + 12  )  ]   = 7 THEN 'pink'
                                                   WHEN lt_numbers[ COND #( WHEN i = 1 THEN j
                                                                            WHEN i = 2 THEN j + 4
                                                                            WHEN i = 3 THEN j + 8
                                                                            WHEN i = 4 THEN j + 12  )  ]   = 8 THEN 'teal'  )

                                            ) ).

    html = html_beginning_part.
    LOOP AT random_table INTO DATA(ls_line).
      html = html && | | && ls_line-open_tag && | | && | | && ls_line-html_class && | | && ls_line-html_event && | | && ls_line-close_tag.
    ENDLOOP.
    html = html &&  | | && html_closing_part.

  html_win =
`<!DOCTYPE html> ` &&
`<html lang="en"> ` &&
`   <head>` &&
`      <meta http-equiv="content-type" content="text/html; charset=utf-8">` &&
`      <meta charset="UTF-8" />` &&
`      <metaname="viewport"content="width=device-width, initial-scale=1.0"/>` &&
`      <meta http-equiv="X-UA-Compatible" content="ie=edge" />` &&
`      <title>Memory Games</title>` &&
`      <style> ` &&
` body{   text-align: center;   font-size: 42px;  }  ` &&
` h2{ color: blue; }    ` &&
`      </style>` &&
`   </head> ` &&
`   <body> ` &&
`     <div > ` &&
` <h2> YOU WIN   </h2> ` &&
`     </div> ` &&
`   </body> ` &&
`</html> `.


    SET HANDLER at_click.
  ENDMETHOD.
  METHOD display.
    cl_abap_browser=>show_html( size         = cl_abap_browser=>medium
                                format       = cl_abap_browser=>landscape
                                context_menu = 'X'
                                html_string  = html ) .
  ENDMETHOD.
  METHOD at_click.

    DATA: ls_modify TYPE t_cell.
    READ TABLE random_table ASSIGNING FIELD-SYMBOL(<fs_clicked>) WITH KEY id = action.
    IF <fs_clicked>-is_matched IS NOT INITIAL OR <fs_clicked>-previous_selected IS NOT INITIAL OR <fs_clicked>-last_selected IS NOT INITIAL.
      RETURN.
    ENDIF.
    LOOP AT random_table ASSIGNING FIELD-SYMBOL(<fs_random_table>) WHERE is_matched IS INITIAL.
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
    LOOP AT random_table ASSIGNING <fs_random_table> WHERE previous_selected IS NOT INITIAL.
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.
      <fs_clicked>-previous_selected = abap_true.
    ELSE.
      <fs_clicked>-last_selected  = abap_true.
    ENDIF.


    READ TABLE random_table ASSIGNING FIELD-SYMBOL(<fs_previous_selected>) WITH KEY previous_selected = abap_true.
    READ TABLE random_table ASSIGNING FIELD-SYMBOL(<fs_last_selected>)     WITH KEY last_selected = abap_true.
    IF <fs_previous_selected> IS ASSIGNED AND <fs_last_selected> IS ASSIGNED.
      IF <fs_previous_selected>-color = <fs_last_selected>-color.
        <fs_previous_selected>-previous_selected = <fs_last_selected>-last_selected = abap_false.
        <fs_previous_selected>-is_matched = <fs_last_selected>-is_matched = abap_true.
      ENDIF.
    ENDIF.

    html = html_beginning_part.
    LOOP AT random_table ASSIGNING <fs_clicked>.
      IF <fs_clicked>-previous_selected IS NOT INITIAL OR <fs_clicked>-last_selected IS NOT INITIAL OR <fs_clicked>-is_matched IS NOT INITIAL.
        REPLACE ALL OCCURRENCES OF 'color-hidden' IN <fs_clicked>-html_class WITH <fs_clicked>-color.
      ENDIF.
      html = html && | | && <fs_clicked>-open_tag && | | && | | && <fs_clicked>-html_class && | | && <fs_clicked>-html_event && | | && <fs_clicked>-close_tag.
    ENDLOOP.
    html = html &&  | | && html_closing_part.

    me->display( ).

    IF <fs_previous_selected> IS ASSIGNED AND <fs_last_selected> IS ASSIGNED.
      IF <fs_previous_selected>-color NE <fs_last_selected>-color.
        REPLACE ALL OCCURRENCES OF <fs_previous_selected>-color  IN <fs_previous_selected>-html_class WITH 'color-hidden'.
        REPLACE ALL OCCURRENCES OF <fs_last_selected>-color  IN <fs_last_selected>-html_class         WITH 'color-hidden'.
        DATA(lv_check) = abap_true.
      ENDIF.
    ENDIF.

    IF <fs_previous_selected> IS ASSIGNED AND <fs_last_selected> IS ASSIGNED.
      IF <fs_previous_selected>-previous_selected = <fs_last_selected>-last_selected.
        <fs_previous_selected>-previous_selected = <fs_last_selected>-last_selected = abap_false.
      ENDIF.
    ENDIF.

    IF lv_check EQ abap_true.
      WAIT UP TO '0.5' SECONDS.
      html = html_beginning_part.
      LOOP AT random_table INTO DATA(ls_line).
        html = html && | | && ls_line-open_tag && | | && | | && ls_line-html_class && | | && ls_line-html_event && | | && ls_line-close_tag.
      ENDLOOP.
      html = html &&  | | && html_closing_part.
      me->display( ).
    ENDIF.

    LOOP AT random_table ASSIGNING <fs_random_table> WHERE is_matched IS INITIAL.
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.
      html = html_win.
      me->display( ).
      RETURN.
    ENDIF.

  ENDMETHOD.
  METHOD call_random_number.
    rv_i = floor(  cl_abap_random_float=>create( seed = cl_abap_random=>seed( ) )->get_next( ) * iv_size ).
    IF iv_size EQ 1.
      rv_i = 1.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.

START-OF-SELECTION.
  NEW memory_game( )->display( ).
