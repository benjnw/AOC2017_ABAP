REPORT zrep_aoc02 NO STANDARD PAGE HEADING.

*>> class-definition

CLASS lcl_aoc DEFINITION.

  PUBLIC SECTION.

    TYPES:
    BEGIN OF ty_tab,
      line TYPE i,
      row TYPE string,
    END OF ty_tab.
    TYPES:
     tt_row TYPE STANDARD TABLE OF ty_tab WITH NON-UNIQUE EMPTY KEY.

    DATA: my_tab TYPE tt_row.
    DATA: my_inc TYPE i VALUE 1.

    METHODS:
      add_row IMPORTING i_row TYPE string,
      challenge RETURNING VALUE(r_checksum) TYPE i.

ENDCLASS.

*>> class-implementation

CLASS lcl_aoc IMPLEMENTATION.

  METHOD add_row.

    DATA ls_tab LIKE LINE OF me->my_tab.
    DATA(len) = lines( me->my_tab ) + 1.

    ls_tab-line = me->my_inc.

    SPLIT i_row AT space INTO TABLE DATA(lt_full_row).
    DELETE lt_full_row WHERE table_line IS INITIAL.
    LOOP AT lt_full_row INTO DATA(ls_row).
      ls_tab-row = ls_row.
      APPEND ls_tab TO me->my_tab.
    ENDLOOP.

    ADD 1 TO me->my_inc.

  ENDMETHOD.

  METHOD challenge.

    DATA diff  TYPE i.
    DATA l_idx TYPE sy-index.
    DATA(l_times) = me->my_inc - 1.

    DO l_times TIMES.

      l_idx = sy-index.

      CLEAR diff.
      DATA max TYPE i VALUE 0.
      DATA min TYPE i VALUE 0.

      LOOP AT me->my_tab INTO DATA(ls_row) WHERE line EQ l_idx.

        IF sy-tabix EQ 1.
          min = max = ls_row-row.
          CONTINUE.
        ENDIF.

        IF ls_row-row LT min.
          min = ls_row-row.
        ENDIF.
        IF ls_row-row GT max.
          max = ls_row-row.
        ENDIF.

      ENDLOOP.

      diff = ( max - min ).
      ADD diff TO r_checksum.

      DELETE me->my_tab WHERE line EQ l_idx.

    ENDDO.

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
      test_split FOR TESTING,
      test_checksum_5195 FOR TESTING.

ENDCLASS.

*>> testclass-implementation

CLASS lcl_test_aoc IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT me->solver.
  ENDMETHOD.

  METHOD test_split.

    me->solver->add_row( '1111 2222  3333  4444 6' ).
    me->solver->add_row( '77 888 97' ).
    cl_aunit_assert=>assert_equals( exp = 8       act = lines( me->solver->my_tab ) ).
    cl_aunit_assert=>assert_equals( exp = '1111'  act = me->solver->my_tab[ 1 ]-row ).
    cl_aunit_assert=>assert_equals( exp = '3333'  act = me->solver->my_tab[ 3 ]-row ).
    cl_aunit_assert=>assert_equals( exp = 1       act = me->solver->my_tab[ 3 ]-line ).

  ENDMETHOD.

  METHOD test_checksum_5195.

    me->solver->add_row( '1111 2222  3333  4444 60' ).
    me->solver->add_row( '77 888 97' ).

    DATA(checksum) = me->solver->challenge( ).
    cl_aunit_assert=>assert_equals( exp = 5195  act = checksum ).

  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.

  DATA lo_aoc TYPE REF TO lcl_aoc.
  CREATE OBJECT lo_aoc.

  lo_aoc->add_row( '6046 6349  208 276 4643  1085  1539  4986  7006  5374  252 4751  226 6757  7495  2923' ).
  lo_aoc->add_row( '1432 1538  1761  1658  104 826 806 109 939 886 1497  280 1412  127 1651  156' ).
  lo_aoc->add_row( '244  1048  133 232 226 1072  883 1045  1130  252 1038  1022  471 70  1222  957' ).
  lo_aoc->add_row( '87 172 93  73  67  192 249 239 155 23  189 106 55  174 181 116' ).
  lo_aoc->add_row( '5871 204 6466  6437  5716  232 1513  7079  6140  268 350 6264  6420  3904  272 5565' ).
  lo_aoc->add_row( '1093 838 90  1447  1224  744 1551  59  328 1575  1544  1360  71  1583  75  370' ).
  lo_aoc->add_row( '213  166 7601  6261  247 210 4809  6201  6690  6816  7776  2522  5618  580 2236  3598' ).
  lo_aoc->add_row( '92 168 96  132 196 157 116 94  253 128 60  167 192 156 76  148' ).
  lo_aoc->add_row( '187  111 141 143 45  132 140 402 134 227 342 276 449 148 170 348' ).
  lo_aoc->add_row( '1894 1298  1531  1354  1801  974 85  93  1712  130 1705  110 314 107 449 350' ).
  lo_aoc->add_row( '1662 1529  784 1704  1187  83  422 146 147 1869  1941  110 525 1293  158 1752' ).
  lo_aoc->add_row( '162  1135  3278  1149  3546  3686  182 149 119 1755  3656  2126  244 3347  157 865' ).
  lo_aoc->add_row( '2049 6396  4111  6702  251 669 1491  245 210 4314  6265  694 5131  228 6195  6090' ).
  lo_aoc->add_row( '458  448 324 235 69  79  94  78  515 68  380 64  440 508 503 452' ).
  lo_aoc->add_row( '198  216 5700  4212  2370  143 5140  190 4934  539 5054  3707  6121  5211  549 2790' ).
  lo_aoc->add_row( '3021 3407  218 1043  449 214 1594  3244  3097  286 114 223 1214  3102  257 3345' ).

  DATA(checksum) = lo_aoc->challenge( ).

  WRITE checksum.
