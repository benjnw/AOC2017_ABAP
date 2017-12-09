REPORT zrep_aoc04 NO STANDARD PAGE HEADING.

*>> class-definition

CLASS lcl_aoc DEFINITION.

  PUBLIC SECTION.

    TYPES:
     tt_row TYPE STANDARD TABLE OF string.

    METHODS:
      challenge IMPORTING it_input TYPE tt_row
                RETURNING VALUE(r_avai) TYPE i.

ENDCLASS.

*>> class-implementation

CLASS lcl_aoc IMPLEMENTATION.

  METHOD challenge.

    DATA l_size_b TYPE i.
    DATA l_size_a TYPE i.

    r_avai = lines( it_input ).

    LOOP AT it_input INTO DATA(ls_input).
      SPLIT ls_input AT space INTO TABLE DATA(lt_full_row).

      l_size_b = lines( lt_full_row ).
      SORT lt_full_row BY table_line.
      DELETE ADJACENT DUPLICATES FROM lt_full_row COMPARING table_line.
      l_size_a = lines( lt_full_row ).

      IF l_size_b NE l_size_a.
        SUBTRACT 1 FROM r_avai.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

*>> testclass-definition

CLASS lcl_test_aoc DEFINITION FOR TESTING
                   RISK LEVEL HARMLESS
                   DURATION SHORT.

  PRIVATE SECTION.
    DATA solver TYPE REF TO lcl_aoc.
    METHODS:
      setup,
      test_3_valids FOR TESTING,
      test_0_valids FOR TESTING.

ENDCLASS.

*>> testclass-implementation

CLASS lcl_test_aoc IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT me->solver.
  ENDMETHOD.

  METHOD test_3_valids.

    DATA lt_input TYPE STANDARD TABLE OF string.
    DATA ls_input LIKE LINE OF lt_input.

    ls_input = 'aa bb cc dd ee'.
    APPEND ls_input TO lt_input.
    ls_input = 'ff bb fr der erd'.
    APPEND ls_input TO lt_input.
    ls_input = 'aa bb cc dd aaa'.
    APPEND ls_input TO lt_input.

    DATA(available) = me->solver->challenge( lt_input ).
    cl_aunit_assert=>assert_equals( exp = 3 act = available ).

  ENDMETHOD.

  METHOD test_0_valids.

    DATA lt_input TYPE STANDARD TABLE OF string.
    DATA ls_input LIKE LINE OF lt_input.

    ls_input = 'aa bb cc dd aa'.
    APPEND ls_input TO lt_input.
    ls_input = 'e3e e3e dd gg rr'.
    APPEND ls_input TO lt_input.
    ls_input = 'dd aa w22 dasdsaw riwqer aa'.
    APPEND ls_input TO lt_input.

    DATA(available) = me->solver->challenge( lt_input ).
    cl_aunit_assert=>assert_equals( exp = 0 act = available ).

  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.

  DATA lt_input TYPE STANDARD TABLE OF string.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename = 'YOUR_PATH_HERE\FILE.txt'
      filetype = 'ASC'
    TABLES
      data_tab = lt_input.

  DATA lo_aoc TYPE REF TO lcl_aoc.
  CREATE OBJECT lo_aoc.

  DATA(available) = lo_aoc->challenge( lt_input ).

  WRITE available.
