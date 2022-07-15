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
                                                                        00130100
       CONFIGURATION                             SECTION.               00130200
       SPECIAL-NAMES.                                                   00130300
           DECIMAL-POINT  IS COMMA.                                     00130400
                                                                        00130500
      *====================================================             00130600
       DATA                                      DIVISION.              00130700
      *====================================================             00130800
      *---------------------------------------------------*             00130900
      *                 LAYOUT DA SYSIN                   *             00131000
      *00015TELEVISAO0000150000SP                                       00132000
      *---------------------------------------------------*             00133000
                                                                        00134000
      *---------------------------------------------------*             00135000
       WORKING-STORAGE                         SECTION.                 00136000
                                                                        00137000
       01 WRK-DADOS.                                                    00138000
          05 WRK-CODIGO      PIC X(05)    VALUE SPACES.                 00138100
          05 WRK-PRODUTO     PIC X(09)    VALUE SPACES.                 00138200
          05 WRK-VALOR       PIC 9(08)V99 VALUE ZEROS.                  00138300
          05 WRK-ESTADO      PIC X(02)    VALUE SPACES.                 00138400
       77 WRK-FRETE          PIC 9(08)V99 COMP VALUE ZEROS.             00138500
       77 WRK-FLAG           PIC 9        VALUE 0.                      00138604
                                                                        00138700
      *---------------------MASCARAS-----------------------             00138800
                                                                        00138900
       77 WRK-VALOR-ED       PIC ZZ.ZZ9.999,99.                         00139000
       77 WRK-FRETE-ED       PIC ZZ.ZZZ.999,99.                         00140000
                                                                        00150000
      *====================================================             00160000
       PROCEDURE                                 DIVISION.              00170000
      *====================================================             00180000
                                                                        00181000
      *-------------------VALIDACOES----------------------*             00182000
                                                                        00183000
           ACCEPT WRK-DADOS FROM SYSIN.                                 00184000
                                                                        00185000
           IF WRK-VALOR LESS 1000                                       00186001
             DISPLAY 'RETIRAR NA LOJA'                                  00187000
              MOVE 2 TO WRK-FLAG                                        00188007
      *-----------CASO VALOR MAIOR OU IGUAL A 1000--------*             00189000
                                                                        00190000
           ELSE                                                         00200000
             EVALUATE WRK-ESTADO                                        00200101
              WHEN 'SP'                                                 00200201
                COMPUTE WRK-FRETE = WRK-VALOR * 0,10                    00200300
              WHEN 'RJ'                                                 00200501
                COMPUTE WRK-FRETE = WRK-VALOR * 0,15                    00200601
              WHEN 'MG'                                                 00200701
                COMPUTE WRK-FRETE = WRK-VALOR * 0,20                    00200801
              WHEN OTHER                                                00200901
                DISPLAY 'NAO ENTREGAMOS'                                00201101
                 MOVE 1 TO WRK-FLAG                                     00201206
             END-EVALUATE                                               00202001
           END-IF.                                                      00202300
      *-----------MOVENDO VARIAVEIS PARA MASCARA----------*             00202400
                                                                        00202500
              MOVE WRK-VALOR TO WRK-VALOR-ED                            00202600
              MOVE WRK-FRETE TO WRK-FRETE-ED                            00202700
                                                                        00202800
      *-----------MOSTRANDO RESULTADO NO DISPLAY----------*             00202900
           IF WRK-FLAG EQUAL 0                                          00203004
              DISPLAY 'CODIGO      ..   ' WRK-CODIGO                    00203100
              DISPLAY 'PRODUTO     ..   ' WRK-PRODUTO                   00203200
              DISPLAY 'ESTADO      ..   ' WRK-ESTADO                    00203300
              DISPLAY 'VALOR       ..   R$' WRK-VALOR-ED                00203400
              DISPLAY 'FRETE       ..   R$' WRK-FRETE-ED                00203500
           END-IF.                                                      00203600
           STOP RUN.                                                    00204000
