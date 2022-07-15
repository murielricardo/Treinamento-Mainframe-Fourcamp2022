      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR25CB21.                                            00040000
                                                                        00050000
      *=====================================================            00060000
      *  AUTOR: MURIEL                                                  00070000
      *  EMPRESA: FOURSYS                                               00080000
      *=====================================================            00090000
                                                                        00100000
      *====================================================             00110000
       ENVIRONMENT                               DIVISION.              00120000
      *====================================================             00130000
       CONFIGURATION                             SECTION.               00131000
       SPECIAL-NAMES.                                                   00132000
           DECIMAL-POINT  IS COMMA.                                     00133000
                                                                        00134000
      *====================================================             00135000
       DATA                                      DIVISION.              00136000
      *====================================================             00137000
       WORKING-STORAGE                         SECTION.                 00138000
       01 WRK-DADOS.                                                    00139000
          05 WRK-NOTA1       PIC 9(02)V99 VALUE ZEROS.                  00140004
          05 WRK-NOTA2       PIC 9(02)V99 VALUE ZEROS.                  00150004
       77 WRK-MEDIA          PIC 9(02)V99 VALUE ZEROS.                  00153004
                                                                        00154200
      *--------------------MASCARAS-----------------------*             00154306
                                                                        00154401
       77 WRK-NOTA1-ED       PIC 99,99.                                 00154504
       77 WRK-NOTA2-ED       PIC 99,99.                                 00154604
       77 WRK-MEDIA-ED       PIC 99,99.                                 00154704
                                                                        00154804
      *====================================================             00154900
       PROCEDURE                                 DIVISION.              00155000
      *====================================================             00155100
                                                                        00155201
           ACCEPT WRK-DADOS FROM SYSIN.                                 00155300
                                                                        00155403
      *-------------CASO VALORES SEJAM NUMERICOS----------*             00155505
                                                                        00155603
            IF WRK-NOTA1 AND WRK-NOTA2 IS NOT NUMERIC                   00158105
             DISPLAY 'SOMENTE NUMEROS SAO PERMITIDOS'                   00158204
                                                                        00158304
      *-----------------CALCULO DE MEDIA------------------*             00158405
                                                                        00158504
            ELSE                                                        00158604
             COMPUTE WRK-MEDIA = WRK-NOTA1 + WRK-NOTA2 / 2              00158703
                                                                        00158803
      *------------- VERIFICACAO DE MEDIA-----------------*             00158905
                                                                        00159003
             IF WRK-MEDIA GREATER 10                                    00159104
              DISPLAY 'MEDIA ERRADA'                                    00159604
                                                                        00159703
      *------------- SE MEDIA MENOR QUE 10 ---------------*             00159805
             ELSE                                                       00159904
               MOVE WRK-NOTA1 TO WRK-NOTA1-ED                           00160104
               MOVE WRK-NOTA2 TO WRK-NOTA2-ED                           00160204
               MOVE WRK-MEDIA TO WRK-MEDIA-ED                           00160304
               DISPLAY 'NOTA 1....   ' WRK-NOTA1-ED                     00160404
               DISPLAY 'NOTA 2....   ' WRK-NOTA2-ED                     00160504
               DISPLAY 'MEDIA.....   ' WRK-MEDIA-ED                     00160604
                                                                        00160704
                EVALUATE WRK-MEDIA                                      00160804
                    WHEN LESS THAN 2                                    00161004
                      DISPLAY 'REPROVADO'                               00162004
                    WHEN GREATER THAN OR EQUAL 2 AND EQUAL OR LESS 5,99 00170004
                      DISPLAY 'RECUPERACAO'                             00180004
                    WHEN GREATER THAN OR EQUAL 6 AND EQUAL OR LESS 8,99 00180104
                      DISPLAY 'APROVADO'                                00180204
                    WHEN GREATER THAN OR EQUAL 9 AND EQUAL OR LESS 10   00180304
                      DISPLAY 'APROVADO COM LOUVOR'                     00180404
                END-EVALUATE                                            00180504
             END-IF                                                     00182704
            END-IF.                                                     00183003
            STOP RUN.                                                   00184003
