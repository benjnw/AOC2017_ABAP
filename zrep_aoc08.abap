REPORT zrep_aoc08 NO STANDARD PAGE HEADING.

*>> class-definition

CLASS lcl_aoc DEFINITION.

  PUBLIC SECTION.

    TYPES: BEGIN OF instructions,
             variable TYPE char20,
             value    TYPE i,
           END OF instructions.

    TYPES:
     tt_row TYPE STANDARD TABLE OF string WITH NON-UNIQUE EMPTY KEY.

    TYPES:
     tt_instructions TYPE STANDARD TABLE OF instructions WITH NON-UNIQUE EMPTY KEY.

    DATA gt_input TYPE tt_row.
    DATA gt_instructions TYPE tt_instructions.

    METHODS:
      do_action  IMPORTING i_action  TYPE string
                           i_to_add  TYPE i
                 CHANGING  c_current TYPE i,
      evaluate   IMPORTING i_symbol      TYPE string
                           i_value       TYPE i
                           i_compare     TYPE i
                 RETURNING VALUE(r_pass) TYPE abap_bool,
      challenge  IMPORTING it_input      TYPE tt_row
                 RETURNING VALUE(r_avai) TYPE i.

ENDCLASS.

*>> class-implementation

CLASS lcl_aoc IMPLEMENTATION.

  METHOD do_action.

    CASE i_action.
      WHEN 'inc'.
        ADD i_to_add TO c_current.
      WHEN 'dec'.
        SUBTRACT i_to_add FROM c_current.
    ENDCASE.

  ENDMETHOD.

  METHOD evaluate.

    r_pass = abap_false.

    CASE i_symbol.
      WHEN '<='.
        IF ( i_value LE i_compare ). r_pass = abap_true. ENDIF.
      WHEN '<'.
        IF ( i_value LT i_compare ). r_pass = abap_true. ENDIF.
      WHEN '=='.
        IF ( i_value EQ i_compare ). r_pass = abap_true. ENDIF.
      WHEN '!='.
        IF ( i_value NE i_compare ). r_pass = abap_true. ENDIF.
      WHEN '>'.
        IF ( i_value GT i_compare ). r_pass = abap_true. ENDIF.
      WHEN '>='.
        IF ( i_value GE i_compare ). r_pass = abap_true. ENDIF.
    ENDCASE.

  ENDMETHOD.

  METHOD challenge.

    DATA ls_instructions LIKE LINE OF me->gt_instructions.

    "for manipulating
    me->gt_input[] = it_input[].

    LOOP AT me->gt_input INTO DATA(ls_input).

      FIND REGEX 'if\s(\w*)\s(<=|<|==|!=|>|>=)\s(.*)' IN ls_input
      IGNORING CASE
      SUBMATCHES DATA(var1) DATA(symbol) DATA(compare).

      DO.
        READ TABLE me->gt_instructions INTO DATA(ls_inst) WITH KEY variable = var1.
        IF sy-subrc EQ 0.

          IF NOT me->evaluate( i_value   = ls_inst-value
                               i_compare = CONV #( compare )
                               i_symbol  = symbol ).
            EXIT.
          ENDIF.

          FIND REGEX '(.*)\s(dec|inc)\s(.*)\sif' IN ls_input
          IGNORING CASE
          SUBMATCHES DATA(var2) DATA(action) DATA(number).

          READ TABLE me->gt_instructions ASSIGNING FIELD-SYMBOL(<fs_instruc>) WITH KEY variable = var2.
          IF sy-subrc EQ 0.

            CALL METHOD do_action
              EXPORTING
                i_to_add  = CONV #( number )
                i_action  = action
              CHANGING
                c_current = <fs_instruc>-value.
          ELSE.

            ls_instructions-variable = var2.
            ls_instructions-value = 0.

            CALL METHOD do_action
              EXPORTING
                i_to_add  = CONV #( number )
                i_action  = action
              CHANGING
                c_current = ls_instructions-value.

            APPEND ls_instructions TO me->gt_instructions.

          ENDIF.

          EXIT.

        ELSE.

          ls_instructions-variable = var1.
          ls_instructions-value = 0.
          APPEND  ls_instructions TO me->gt_instructions.

        ENDIF.

      ENDDO.

    ENDLOOP.

    SORT me->gt_instructions BY value DESCENDING.
    READ TABLE me->gt_instructions INTO DATA(my_instruc) INDEX 1 TRANSPORTING value.

    r_avai = my_instruc-value.

*    BREAK-POINT.

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
      test_should_be_1 FOR TESTING.

ENDCLASS.

*>> testclass-implementation

CLASS lcl_test_aoc IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT me->solver.
  ENDMETHOD.

  METHOD test_should_be_1.

    DATA lt_input TYPE STANDARD TABLE OF string WITH NON-UNIQUE EMPTY KEY.
    DATA ls_input LIKE LINE OF lt_input.

    ls_input = 'b inc 5 if a > 1'.
    APPEND ls_input TO lt_input.
    ls_input = 'a inc 1 if b < 5'.
    APPEND ls_input TO lt_input.
    ls_input = 'c dec -10 if a >= 1'.
    APPEND ls_input TO lt_input.
    ls_input = 'c inc -20 if c == 10'.
    APPEND ls_input TO lt_input.

    DATA(larg_value) = me->solver->challenge( lt_input ).
    cl_aunit_assert=>assert_equals( exp = 1 act = larg_value ).

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

  DATA(larg_value) = lo_aoc->challenge( lt_input ).

  WRITE larg_value.
