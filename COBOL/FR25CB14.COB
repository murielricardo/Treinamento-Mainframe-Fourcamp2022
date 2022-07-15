      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR25CB14.                                            00040001
                                                                        00050000
      *=====================================================            00060000
      *  AUTOR: MURIEL                                                  00070001
      *  EMPRESA: FOURSYS                                               00080000
      *=====================================================            00090000
                                                                        00100000
      *====================================================             00101000
       ENVIRONMENT                               DIVISION.              00102000
      *====================================================             00103000
       CONFIGURATION                             SECTION.               00104012
       SPECIAL-NAMES.                                                   00104112
           DECIMAL-POINT IS COMMA.                                      00104212
      *====================================================             00105000
       DATA                                      DIVISION.              00106000
      *====================================================             00107000
       WORKING-STORAGE                         SECTION.                 00108000
       01 WRK-DADOS.                                                    00108100
          05 WRK-N1       PIC 9(02) VALUE ZEROS.                        00108213
          05 WRK-N2       PIC 9(08) VALUE ZEROS.                        00108313
          05 WRK-N3       PIC 9(03) VALUE ZEROS.                        00108413
       77 WRK-MEDIA       PIC 9(10) VALUE ZEROS.                        00108513
       77 WRK-MEDIA-EDIT  PIC -ZZ.ZZZ.999,99.                           00108614
       77 WRK-N2-EDIT     PIC -ZZ9.999,99.                              00108714
      *====================================================             00108900
       PROCEDURE                                 DIVISION.              00109000
      *====================================================             00109100
           ACCEPT WRK-DADOS FROM SYSIN.                                 00109200
           DISPLAY 'MES                     ..    ' WRK-N1.             00109318
           DISPLAY 'TOTAL-FUNC              ..   ' WRK-N3.              00109417
            MOVE WRK-N2 TO WRK-N2-EDIT.                                 00109513
           DISPLAY 'TOTAL-VENDAS            .. R$   '   WRK-N2-EDIT.    00109620
            DIVIDE WRK-N2 BY WRK-N3 GIVING WRK-MEDIA                    00109802
               ON SIZE ERROR                                            00109900
                 DISPLAY 'ERRO NA DIVISAO'                              00110000
               NOT ON SIZE ERROR                                        00110100
                MOVE WRK-MEDIA TO WRK-MEDIA-EDIT                        00110209
                 DISPLAY 'MEDIA DE VENDAS P/ FUNC .. R$' WRK-MEDIA-EDIT 00110421
            END-DIVIDE.                                                 00111000
           STOP RUN.                                                    00120000
