      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
                                                                        00031015
       PROGRAM-ID. FR25CB08.                                            00040000
                                                                        00050000
      *====================================================             00060000
      * AUTOR: MURIEL                                                   00070000
      * EMPRESA: FOURSYS                                                00080000
      *====================================================             00090000
                                                                        00091000
      *====================================================             00092000
       ENVIRONMENT                               DIVISION.              00093000
      *====================================================             00094000
                                                                        00095000
      *====================================================             00096000
       DATA                                      DIVISION.              00097000
      *====================================================             00098000
       WORKING-STORAGE                           SECTION.               00099000
                                                                        00100000
       01 WRK-DADOS.                                                    00110000
          05 WRK-ID        PIC 9(05) VALUE ZEROS.                       00120009
          05 WRK-NOME      PIC X(30) VALUE SPACES.                      00121009
          05 WRK-ENDERECO  PIC X(30) VALUE SPACES.                      00122009
                                                                        00123000
       01 WRK-DADOS-RED1   REDEFINES WRK-DADOS.                         00124009
          05 WRK-ID-RED1   PIC 9(05).                                   00125009
          05 WRK-NOME-RED1 PIC X(30).                                   00126009
                                                                        00126100
       01 WRK-DADOS-RED2   REDEFINES WRK-DADOS.                         00126209
          05 WRK-ID-RED2   PIC 9(05).                                   00126409
                                                                        00126511
       01 WRK-DADOS-RED3   REDEFINES WRK-DADOS.                         00126611
          05 FILLER        PIC X(35).                                   00126814
          05 WRK-ENDERECO-RED3 PIC X(30).                               00126914
                                                                        00127011
      *====================================================             00127300
       PROCEDURE                                 DIVISION.              00127400
      *====================================================             00127500
           ACCEPT WRK-DADOS FROM SYSIN.                                 00127600
           DISPLAY 'WRK-DADOS     ..' WRK-DADOS.                        00127700
           DISPLAY 'WRK-DADOS-RED1..' WRK-DADOS-RED1.                   00127800
           DISPLAY 'WRK-DADOS-RED2..' WRK-DADOS-RED2.                   00127900
           DISPLAY 'WRK-DADOS-RED3..' WRK-ENDERECO-RED3.                00128012
           STOP RUN.                                                    00128100
      *====================================================             00129010
