REPORT zrep_aoc05 NO STANDARD PAGE HEADING.

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

    DATA l_tabix TYPE sy-tabix VALUE 1.
    DATA l_valid TYPE abap_bool VALUE abap_true.
    DATA lt_input TYPE tt_row.

    lt_input[] = it_input[].

    WHILE l_valid EQ abap_true.

      READ TABLE lt_input ASSIGNING FIELD-SYMBOL(<ls_input>) INDEX l_tabix.
      IF sy-subrc NE 0.
        l_valid = abap_false.
        CONTINUE.
      ENDIF.

      IF <ls_input> NE 0.
        ADD <ls_input> TO l_tabix.
      ENDIF.

      ADD 1 TO <ls_input>.
      ADD 1 TO r_avai.

    ENDWHILE.

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
      test_5_steps FOR TESTING.

ENDCLASS.

*>> testclass-implementation

CLASS lcl_test_aoc IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT me->solver.
  ENDMETHOD.

  METHOD test_5_steps.

    DATA lt_input TYPE STANDARD TABLE OF string.
    DATA ls_input LIKE LINE OF lt_input.

    ls_input = '0'.
    APPEND ls_input TO lt_input.
    ls_input = '3'.
    APPEND ls_input TO lt_input.
    ls_input = '0'.
    APPEND ls_input TO lt_input.
    ls_input = '1'.
    APPEND ls_input TO lt_input.
    ls_input = '-3'.
    APPEND ls_input TO lt_input.

    DATA(steps) = me->solver->challenge( lt_input ).
    cl_aunit_assert=>assert_equals( exp = 5 act = steps ).

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

  DATA(steps) = lo_aoc->challenge( lt_input ).

  WRITE steps.
