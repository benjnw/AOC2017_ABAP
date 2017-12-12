REPORT zrep_aoc06 NO STANDARD PAGE HEADING.

*>> class-definition

CLASS lcl_aoc DEFINITION.

  PUBLIC SECTION.

    TYPES:
     tt_row TYPE STANDARD TABLE OF string WITH NON-UNIQUE EMPTY KEY.

    DATA gt_input TYPE tt_row.

    METHODS:
      set_guid   RETURNING VALUE(r_guid) TYPE string,

      distribute IMPORTING
                   i_tbx TYPE sy-tabix
                   i_max TYPE i
                 RETURNING value(ref) TYPE REF TO lcl_aoc,

      challenge  IMPORTING it_input      TYPE tt_row
                 RETURNING VALUE(r_avai) TYPE i.

ENDCLASS.

*>> class-implementation

CLASS lcl_aoc IMPLEMENTATION.

  METHOD set_guid.

    LOOP AT me->gt_input INTO DATA(ls_input).
      r_guid = r_guid && ls_input && '-'.
      CONDENSE r_guid NO-GAPS.
    ENDLOOP.

  ENDMETHOD.

  METHOD distribute.

    FIELD-SYMBOLS <ls_input> TYPE any.
    DATA l_tbx TYPE sy-tabix.

    l_tbx = i_tbx.

    DO i_max TIMES.
      READ TABLE me->gt_input ASSIGNING <ls_input> INDEX ( l_tbx + 1 ).
      IF sy-subrc NE 0.
        l_tbx = 0.
        READ TABLE me->gt_input ASSIGNING <ls_input> INDEX ( l_tbx + 1 ).
      ENDIF.
      ADD 1 TO <ls_input>.
      ADD 1 TO l_tbx.
    ENDDO.

    "for chaining
    ref = me.

  ENDMETHOD.

  METHOD challenge.

    DATA l_valid TYPE abap_bool VALUE abap_true.
    DATA lt_input TYPE tt_row.
    DATA lt_guid_seen TYPE tt_row.
    DATA l_max TYPE i VALUE 0.
    DATA l_tbx TYPE sy-tabix.
    DATA current_guid TYPE string.

    "for manipulating
    me->gt_input[] = it_input[].

    WHILE l_valid EQ abap_true.

      l_max = 0.
      LOOP AT me->gt_input INTO DATA(ls_input).
        IF ls_input GT l_max.
          l_max = ls_input.
          l_tbx = sy-tabix.
        ENDIF.
      ENDLOOP.

      MODIFY me->gt_input INDEX l_tbx FROM 0.

      CLEAR current_guid.
      current_guid = me->distribute( i_max = l_max i_tbx = l_tbx )->set_guid( ).

      READ TABLE lt_guid_seen WITH KEY table_line = current_guid TRANSPORTING NO FIELDS.
      IF sy-subrc NE 0.
        ADD 1 TO r_avai.
        APPEND current_guid TO lt_guid_seen.
        CONTINUE.
      ENDIF.

      ADD 1 TO r_avai.
      l_valid = abap_false.

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

    DATA lt_input TYPE STANDARD TABLE OF string WITH NON-UNIQUE EMPTY KEY.
    DATA ls_input LIKE LINE OF lt_input.

    ls_input = '0'.
    APPEND ls_input TO lt_input.
    ls_input = '2'.
    APPEND ls_input TO lt_input.
    ls_input = '7'.
    APPEND ls_input TO lt_input.
    ls_input = '0'.
    APPEND ls_input TO lt_input.

    DATA(steps) = me->solver->challenge( lt_input ).
    cl_aunit_assert=>assert_equals( exp = 5 act = steps ).

  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.

  DATA lt_input TYPE STANDARD TABLE OF string WITH NON-UNIQUE EMPTY KEY.
  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename = 'C:\Users\TCBJ00991773\Documents\ABAP_BEN\aoc_day6.txt'
      filetype = 'ASC'
    TABLES
      data_tab = lt_input.

  DATA lo_aoc TYPE REF TO lcl_aoc.
  CREATE OBJECT lo_aoc.

  READ TABLE lt_input INTO DATA(ls_input) INDEX 1.
  SPLIT ls_input AT cl_abap_char_utilities=>horizontal_tab INTO TABLE DATA(lt_input_filtered).
  CLEAR lt_input[].
  lt_input[] = lt_input_filtered[].

  DATA(steps) = lo_aoc->challenge( lt_input ).

  WRITE steps.
