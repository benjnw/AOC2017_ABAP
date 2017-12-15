REPORT zrep_aoc07 NO STANDARD PAGE HEADING.

*>> class-definition

CLASS lcl_aoc DEFINITION.

  PUBLIC SECTION.

    TYPES:
     tt_row TYPE STANDARD TABLE OF string WITH NON-UNIQUE EMPTY KEY.

    TYPES:
     tt_circus TYPE STANDARD TABLE OF char20 WITH NON-UNIQUE EMPTY KEY.

    DATA gt_input TYPE tt_row.
    DATA gt_circus TYPE tt_circus.

    METHODS:
      challenge  IMPORTING it_input      TYPE tt_row
                 RETURNING VALUE(r_avai) TYPE char20.

ENDCLASS.

*>> class-implementation

CLASS lcl_aoc IMPLEMENTATION.

  METHOD challenge.

    "for manipulating
    me->gt_input[] = it_input[].

    LOOP AT me->gt_input INTO DATA(ls_input).

      FIND REGEX '^(.*)\(' IN ls_input
      IGNORING CASE
      SUBMATCHES DATA(name).

      APPEND name TO me->gt_circus.

      FIND REGEX '^.*\->\s(.*)$' IN ls_input
      IGNORING CASE
      SUBMATCHES DATA(discs).
      IF sy-subrc EQ 0.
        SPLIT discs AT ', ' INTO TABLE DATA(lt_discs).
        APPEND LINES OF lt_discs TO me->gt_circus.
      ENDIF.

    ENDLOOP.

    SORT me->gt_circus BY table_line.
    "any ways to do this using DELETE ADJASCENT DUPLICATES?

    DATA my TYPE char20.
    DATA prev TYPE sy-tabix.
    DATA next TYPE sy-tabix.
    LOOP AT me->gt_circus INTO DATA(curr_disc).

      IF sy-tabix EQ 1.
        prev = next = 1.
      ELSE.
        prev = sy-tabix - 1.
        next = sy-tabix + 1.
      ENDIF.

      IF ( me->gt_circus[ prev ] EQ curr_disc ) OR
         ( me->gt_circus[ next ] EQ curr_disc ).
        CONTINUE.
      ELSE.
        r_avai = curr_disc.
        EXIT.
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
      test_tknk FOR TESTING.

ENDCLASS.

*>> testclass-implementation

CLASS lcl_test_aoc IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT me->solver.
  ENDMETHOD.

  METHOD test_tknk.

    DATA lt_input TYPE STANDARD TABLE OF string WITH NON-UNIQUE EMPTY KEY.
    DATA ls_input LIKE LINE OF lt_input.

    ls_input = 'pbga (66)'.
    APPEND ls_input TO lt_input.
    ls_input = 'xhth (57)'.
    APPEND ls_input TO lt_input.
    ls_input = 'ebii (61)'.
    APPEND ls_input TO lt_input.
    ls_input = 'havc (66)'.
    APPEND ls_input TO lt_input.
    ls_input = 'ktlj (57)'.
    APPEND ls_input TO lt_input.
    ls_input = 'fwft (72) -> ktlj, cntj, xhth'.
    APPEND ls_input TO lt_input.
    ls_input = 'qoyq (66)'.
    APPEND ls_input TO lt_input.
    ls_input = 'padx (45) -> pbga, havc, qoyq'.
    APPEND ls_input TO lt_input.
    ls_input = 'tknk (41) -> ugml, padx, fwft'.
    APPEND ls_input TO lt_input.
    ls_input = 'jptl (61)'.
    APPEND ls_input TO lt_input.
    ls_input = 'ugml (68) -> gyxo, ebii, jptl'.
    APPEND ls_input TO lt_input.
    ls_input = 'gyxo (61)'.
    APPEND ls_input TO lt_input.
    ls_input = 'cntj (57)'.
    APPEND ls_input TO lt_input.

    DATA(bottom) = me->solver->challenge( lt_input ).
    cl_aunit_assert=>assert_equals( exp = 'tknk' act = bottom ).

  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.

  DATA lt_input TYPE STANDARD TABLE OF string WITH NON-UNIQUE EMPTY KEY.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename = 'YOUR\PATH\HERE\file.txt'
      filetype = 'ASC'
    TABLES
      data_tab = lt_input.

  DATA lo_aoc TYPE REF TO lcl_aoc.
  CREATE OBJECT lo_aoc.

  DATA(bottom) = lo_aoc->challenge( lt_input ).

  WRITE bottom.
