      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR25CB15.                                            00040005
                                                                        00050000
      *=====================================================            00060000
      *  AUTOR: IVAN                                                    00070000
      *  EMPRESA: FOURSYS                                               00080000
      *=====================================================            00090000
                                                                        00100000
      *====================================================             00110000
       ENVIRONMENT                               DIVISION.              00120000
      *====================================================             00130000
       CONFIGURATION                             SECTION.               00140016
       SPECIAL-NAMES.                                                   00141016
           DECIMAL-POINT IS COMMA.                                      00142016
      *====================================================             00150000
       DATA                                      DIVISION.              00160000
      *================================================                 00170044
       WORKING-STORAGE                         SECTION.                 00180000
       01 WRK-DADOS.                                                    00190000
          05 WRK-FUNC          PIC X(15) VALUE SPACES.                  00200003
          05 WRK-TOTALVEND     PIC 9(05)V99 VALUE ZEROS.                00210006
          05 WRK-COMISSAO      PIC 9(02)V99 VALUE ZEROS.                00220006
       77 WRK-COMISSAO-AJUS    PIC 9(08)V99 VALUE ZEROS.                00230006
                                                                        00230127
      *--------------------MASCARAS------------------------             00230227
       77 WRK-TOTALVEND-ED1    PIC -$99.999,99.                         00230328
       77 WRK-TOTALVEND-ED2    PIC -$Z.Z99.999,99.                      00231041
       77 WRK-COMISSAO-AJUS-ED PIC -$Z.Z99.999,99.                      00232042
      *====================================================             00240000
       PROCEDURE                                 DIVISION.              00250000
      *====================================================             00260000
           ACCEPT WRK-DADOS FROM SYSIN.                                 00270000
           DISPLAY 'WRK-FUNC          ..  ' WRK-FUNC.                   00280032
            MOVE WRK-TOTALVEND TO WRK-TOTALVEND-ED1.                    00281027
                                                                        00282031
           DISPLAY 'WRK-TOTALVEND     ..  ' WRK-TOTALVEND-ED1.          00290032
           DISPLAY 'COMISSAO          ..    ' WRK-COMISSAO.             00300046
                                                                        00301031
            MULTIPLY WRK-TOTALVEND BY WRK-COMISSAO GIVING,              00310006
                     WRK-COMISSAO-AJUS.                                 00311006
            MOVE WRK-COMISSAO-AJUS TO WRK-COMISSAO-AJUS-ED.             00321031
           DISPLAY 'VALOR DA COMISSAO ..  ' WRK-COMISSAO-AJUS-ED.       00322031
                                                                        00323031
           ADD WRK-TOTALVEND TO WRK-COMISSAO-AJUS GIVING WRK-TOTALVEND. 00330013
            MOVE WRK-TOTALVEND TO WRK-TOTALVEND-ED2.                    00350027
           DISPLAY 'VALOR FINAL       ..  ' WRK-TOTALVEND-ED2.          00360032
           STOP RUN.                                                    00370000
