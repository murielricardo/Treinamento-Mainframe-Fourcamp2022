      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR25CB18.                                            00040000
                                                                        00050000
      *=====================================================            00060000
      *  AUTOR: MURIEL                                                  00070000
      *  EMPRESA: FOURSYS                                               00080000
      *=====================================================            00090000
                                                                        00100000
      *====================================================             00110000
       ENVIRONMENT                               DIVISION.              00120000
      *====================================================             00130000
                                                                        00130101
       CONFIGURATION                             SECTION.               00131000
       SPECIAL-NAMES.                                                   00132000
           DECIMAL-POINT  IS COMMA.                                     00133000
                                                                        00134000
      *====================================================             00135000
       DATA                                      DIVISION.              00136000
      *====================================================             00137000
      *---------------------------------------------------*             00137101
      *                 LAYOUT DA SYSIN                   *             00137201
      *00015TELEVISAO0000150000SP                                       00137416
      *---------------------------------------------------*             00137501
                                                                        00137601
      *---------------------------------------------------*             00137701
       WORKING-STORAGE                         SECTION.                 00138000
                                                                        00138100
       01 WRK-DADOS.                                                    00138200
          05 WRK-CODIGO      PIC X(05)    VALUE SPACES.                 00138301
          05 WRK-PRODUTO     PIC X(09)    VALUE SPACES.                 00138406
          05 WRK-VALOR       PIC 9(08)V99 VALUE ZEROS.                  00138501
          05 WRK-ESTADO      PIC X(02)    VALUE SPACES.                 00138600
       77 WRK-FRETE          PIC 9(08)V99 COMP VALUE ZEROS.             00138701
       77 WRK-FLAG           PIC 9        VALUE 0.                      00138831
                                                                        00138931
      *---------------------MASCARAS-----------------------             00139000
                                                                        00139100
       77 WRK-VALOR-ED       PIC ZZ.ZZ9.999,99.                         00139225
       77 WRK-FRETE-ED       PIC ZZ.ZZZ.999,99.                         00139325
                                                                        00150000
      *====================================================             00160000
       PROCEDURE                                 DIVISION.              00170000
      *====================================================             00180000
                                                                        00181031
      *-------------------VALIDACOES----------------------*             00182031
                                                                        00183031
           ACCEPT WRK-DADOS FROM SYSIN.                                 00190000
                                                                        00194032
           IF WRK-VALOR IS LESS 1000                                    00195034
             DISPLAY 'RETIRAR NA LOJA'                                  00196031
                                                                        00196131
      *-----------CASO VALOR MAIOR OU IGUAL A 1000--------*             00197031
                                                                        00198031
           ELSE                                                         00200134
              IF WRK-ESTADO EQUAL 'SP'                                  00200231
                COMPUTE WRK-FRETE = WRK-VALOR * 0,10                    00200331
                 MOVE 1 TO WRK-FLAG                                     00200431
              END-IF                                                    00200531
              IF WRK-ESTADO EQUAL 'RJ'                                  00200631
                COMPUTE WRK-FRETE = WRK-VALOR * 0,15                    00200731
                 MOVE 1 TO WRK-FLAG                                     00200831
              END-IF                                                    00200931
              IF WRK-ESTADO EQUAL 'MG'                                  00201031
                COMPUTE WRK-FRETE = WRK-VALOR * 0,20                    00201131
                 MOVE 1 TO WRK-FLAG                                     00201231
              END-IF                                                    00201331
                                                                        00201431
                IF WRK-FLAG EQUAL 0                                     00201631
                  DISPLAY 'NAO ENTREGAMOS'                              00201731
                END-IF                                                  00201831
           END-IF.                                                      00202134
      *-----------MOVENDO VARIAVEIS PARA MASCARA----------*             00202231
                                                                        00202331
              MOVE WRK-VALOR TO WRK-VALOR-ED                            00202431
              MOVE WRK-FRETE TO WRK-FRETE-ED                            00202531
                                                                        00202631
      *-----------MOSTRANDO RESULTADO NO DISPLAY----------*             00202731
           IF WRK-FLAG EQUAL 1                                          00202835
              DISPLAY 'CODIGO      ..   ' WRK-CODIGO                    00202930
              DISPLAY 'PRODUTO     ..   ' WRK-PRODUTO                   00203030
              DISPLAY 'ESTADO      ..   ' WRK-ESTADO                    00203130
              DISPLAY 'VALOR       ..   R$' WRK-VALOR-ED                00203230
              DISPLAY 'FRETE       ..   R$' WRK-FRETE-ED                00204031
           END-IF.                                                      00209235
           STOP RUN.                                                    00248000
