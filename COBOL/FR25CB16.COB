      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR25CB16.                                            00040005
                                                                        00050000
      *=====================================================            00060000
      *  AUTOR: MURIEL                                                  00070005
      *  EMPRESA: FOURSYS                                               00080000
      *=====================================================            00090000
                                                                        00100000
      *====================================================             00110000
       ENVIRONMENT                               DIVISION.              00120000
      *====================================================             00130000
       CONFIGURATION                             SECTION.               00131004
       SPECIAL-NAMES.                                                   00132004
           DECIMAL-POINT  IS COMMA.                                     00133004
      *====================================================             00150000
       DATA                                      DIVISION.              00160000
      *====================================================             00170000
       WORKING-STORAGE                         SECTION.                 00180000
       01 WRK-DADOS.                                                    00190000
          05 WRK-CODIGOLOJA    PIC 9(03)     VALUE ZEROS.               00200012
          05 WRK-PRODUTO       PIC X(30)     VALUE SPACES.              00210012
          05 WRK-QTDVENDAS     PIC S9(03)    VALUE ZEROS.               00220024
          05 WRK-VALORUNIT     PIC S9(05)V99 VALUE ZEROS.               00221021
                                                                        00222024
       77 WRK-QTDVENDAS-CP     PIC S9(03) COMP VALUE ZEROS.             00223024
       77 WRK-TOTALVEND        PIC S9(08)V99 COMP VALUE ZEROS.          00223122
       77 WRK-MEDIA            PIC S9(08)V99 COMP VALUE ZEROS.          00223222
                                                                        00223313
      *---------------------MASCARAS-----------------------             00223413
       77 WRK-VALORUNIT-ED     PIC Z9.999,99.                           00224013
       77 WRK-TOTALVEND-ED     PIC ZZ.Z99.999,99.                       00231013
       77 WRK-MEDIA-ED         PIC ZZ.ZZ9.999,99.                       00233013
      *====================================================             00240000
       PROCEDURE                                 DIVISION.              00250000
      *====================================================             00260000
           ACCEPT WRK-DADOS FROM SYSIN.                                 00270000
            MOVE WRK-QTDVENDAS TO WRK-QTDVENDAS-CP                      00301125
                                                                        00301425
           COMPUTE WRK-TOTALVEND = WRK-QTDVENDAS-CP * WRK-VALORUNIT     00301526
           COMPUTE WRK-MEDIA = WRK-TOTALVEND / 5                        00301626
                                                                        00301727
            MOVE WRK-TOTALVEND TO WRK-TOTALVEND-ED                      00301827
            MOVE WRK-VALORUNIT TO WRK-VALORUNIT-ED                      00301927
            MOVE WRK-MEDIA TO WRK-MEDIA-ED                              00302027
                                                                        00302127
           DISPLAY 'CODIGO LOJA   ..  ' WRK-CODIGOLOJA.                 00302225
           DISPLAY 'PRODUTO       ..  ' WRK-PRODUTO.                    00302325
           DISPLAY 'QTD VENDAS    ..  ' WRK-QTDVENDAS.                  00302425
           DISPLAY 'VALOR UNIT    ..          R$    ' WRK-VALORUNIT-ED. 00302525
           DISPLAY 'TOTAL VENDIDO ..          R$' WRK-TOTALVEND-ED.     00302625
           DISPLAY 'MEDIA DE VENDAS SEMANAL.. R$' WRK-MEDIA-ED.         00302725
           STOP RUN.                                                    00370000
