      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR25CB13.                                            00040002
                                                                        00050000
      *=====================================================            00060000
      *  AUTOR: MURIEL                                                  00070003
      *  EMPRESA: FOURSYS                                               00080000
      *=====================================================            00090000
                                                                        00100000
      *====================================================             00101000
       ENVIRONMENT                               DIVISION.              00102000
      *====================================================             00103000
       CONFIGURATION                             SECTION.               00104038
       SPECIAL-NAMES.                                                   00104138
           DECIMAL-POINT IS COMMA.                                      00104238
      *====================================================             00105000
       DATA                                      DIVISION.              00106000
      *====================================================             00107000
       WORKING-STORAGE                         SECTION.                 00108000
       01 WRK-DADOS.                                                    00108100
          05 WRK-N1         PIC  X(30)    VALUE SPACES.                 00108234
          05 WRK-N2         PIC 9(05)V99 VALUE ZEROS.                   00108337
          05 WRK-DESC       PIC 9(03)V99 VALUE ZEROS.                   00108437
                                                                        00108604
       77 WRK-RES           PIC 9(05)V99 VALUE 0.                       00108737
      *-------------------MASCARAS-------------------------             00108839
                                                                        00108939
       77 WRK-RES-EDIT      PIC -99.999,99.                             00109043
       77 WRK-N2-EDIT       PIC -99.999,99.                             00109143
       77 WRK-DESC-EDIT     PIC -999,99.                                00109243
                                                                        00109339
      *====================================================             00109401
       PROCEDURE                                 DIVISION.              00109501
      *====================================================             00109601
           ACCEPT WRK-DADOS FROM SYSIN.                                 00109701
           DISPLAY 'WRK-N1        .. ' WRK-N1.                          00110043
                                                                        00110140
            MOVE WRK-N2 TO WRK-N2-EDIT.                                 00110240
           DISPLAY 'WRK-N2        .. R$' WRK-N2-EDIT.                   00110343
                                                                        00110440
            MOVE WRK-DESC TO WRK-DESC-EDIT.                             00110540
           DISPLAY 'WRK-DESC      .. R$   ' WRK-DESC-EDIT.              00110645
                                                                        00110740
            SUBTRACT  WRK-DESC FROM  WRK-N2 GIVING WRK-RES.             00110810
             MOVE WRK-RES TO WRK-RES-EDIT.                              00110901
           DISPLAY 'WRK-RES-EDIT  .. R$' WRK-RES-EDIT.                  00111043
           STOP RUN.                                                    00112001
