REPORT zrep_aoc01 NO STANDARD PAGE HEADING.

*>> class-definition

CLASS lcl_aoc DEFINITION.

  PUBLIC SECTION.
    METHODS:
      challenge IMPORTING i_captcha    TYPE string
                RETURNING VALUE(r_sum) TYPE i.

ENDCLASS.

*>> class-implementation

CLASS lcl_aoc IMPLEMENTATION.

  METHOD challenge.

    DATA curr_idx TYPE sy-index VALUE 0.
    DATA next_idx TYPE sy-index VALUE 1.
    DATA curr TYPE i.
    DATA next TYPE i.
    DATA(len) = strlen( i_captcha ) - 1.

    DO len TIMES.

      IF sy-index GT 1.
        ADD 1 TO curr_idx.
        ADD 1 TO next_idx.
      ENDIF.

      curr = i_captcha+curr_idx(1).
      next = i_captcha+next_idx(1).

      IF curr EQ next.
        ADD curr TO r_sum.
      ENDIF.

    ENDDO.

    IF i_captcha(1) EQ i_captcha+len(1).
      ADD i_captcha(1) TO r_sum.
    ENDIF.

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
      test_1122 FOR TESTING,
      test_1111 FOR TESTING,
      test_1234 FOR TESTING,
      test_91212129 FOR TESTING.

ENDCLASS.

*>> testclass-implementation

CLASS lcl_test_aoc IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT me->solver.
  ENDMETHOD.

  METHOD test_1122.

    DATA(sum) = me->solver->challenge( '1122' ).
    cl_aunit_assert=>assert_equals( exp = 3  act = sum ).

  ENDMETHOD.

  METHOD test_1111.

    DATA(sum) = me->solver->challenge( '1111' ).
    cl_aunit_assert=>assert_equals( exp = 4  act = sum ).

  ENDMETHOD.

  METHOD test_1234.

    DATA(sum) = me->solver->challenge( '1234' ).
    cl_aunit_assert=>assert_equals( exp = 0  act = sum ).

  ENDMETHOD.

  METHOD test_91212129.

    DATA(sum) = me->solver->challenge( '91212129' ).
    cl_aunit_assert=>assert_equals( exp = 9  act = sum ).

  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.

  DATA lo_aoc TYPE REF TO lcl_aoc.
  DATA(lv_captcha) =
  '683763347952248558274598352939674972954641755898815882568823446994735954139126882786472358625661232339839216625'  && "111
  '7879291745391279535274642651264996561591958851212556718683741137117987528762148875976142962917488697229834919772' && "112 x 17
  '2423458299323141529413191327622485249495864168181327197661454464926326248274999448373741839963155646828842752761' &&
  '2931423564229643553495219874832114963612896663757797283459522316494537116845391648931518118496533318459989985979' &&
  '9114688136171723451791175989379234881581875526245637862711677949543559613961724657167853118333595624416387144567' &&
  '4244765586446362529159854137535962117184875192273872222899887357292312978286182636232921252574738118347521187637' &&
  '8296238318724373819792239556756342578891378236849241273384332485195152117967325993149216113997365712772225463323' &&
  '6946113627741741979486552412398972249235653683231393759743771787378759384946883673364252937854715114639753299723' &&
  '7439387663769334722979172954835154486382983716698212694357398153392926255272961384626131829678171219569288685597' &&
  '1411323553227882541639238883781555739487531854231589978777186876424464574466434225365412389797617254964262923593' &&
  '8216853564121612421174189655256212894182417224191387353782897617273827698391523224145158942191112156722889985393' &&
  '4667954786256223614621554618294467191255153395256524786159758429643756586457639177183891162214163549688595416893' &&
  '3831949958245342478414142475262682127619549137194521148767647457999827925947537596263343196311919178943681167388' &&
  '9354879766111189966413839835481893113548698494471999239314868172411661674142893768798515265829667984547476647774' &&
  '1553632712968679175356452987459761126437216758171182395219393289199148996813762849991484678429793578629331215796' &&
  '9967514843757848955616821566585798875187468623717513726924727652173747913246567452915747844952994773629646763511' &&
  '4818367689712236683865634274594494527526361772935983146656569498321725259423782818761285752334426541822788321938' &&
  '3138893873384775659548637662867572687198263688597865118173921615178165442133987362382721444844952715592955744739' &&
  '873677838847693982379696776'. "27
  "111 + (112x17) + 27 = 2042 --> ok

  CREATE OBJECT lo_aoc.

  DATA(sum) = lo_aoc->challenge( lv_captcha ).

  WRITE sum.