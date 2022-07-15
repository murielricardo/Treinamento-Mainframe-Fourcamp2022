      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR25CB17.                                            00040001
                                                                        00050000
      *=====================================================            00060000
      *  AUTOR: MURIEL                                                  00070001
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
                                                                        00138127
       01 WRK-DADOS.                                                    00139000
          05 WRK-GAS         PIC 9(02)V99 VALUE ZEROS.                  00140027
          05 WRK-ALC         PIC 9(02)V99 VALUE ZEROS.                  00150027
       77 WRK-PORCENTAGEM    PIC 9(02)V99 COMP VALUE ZEROS.             00170027
                                                                        00171027
      *---------------------MASCARAS-----------------------             00180004
                                                                        00181004
       77 WRK-PORCENTAGEM-ED   PIC 99,99.                               00182004
       77 WRK-GAS-ED           PIC 99,99.                               00183004
       77 WRK-ALC-ED           PIC 99,99.                               00184004
                                                                        00186004
      *====================================================             00190000
       PROCEDURE                                 DIVISION.              00200000
      *====================================================             00210000
           ACCEPT WRK-DADOS FROM SYSIN.                                 00220000
           IF WRK-GAS IS NUMERIC AND                                    00221024
              WRK-ALC IS NUMERIC                                        00222024
                                                                        00223022
             COMPUTE WRK-PORCENTAGEM = (WRK-ALC / WRK-GAS) * 100        00240122
                                                                        00240204
              MOVE WRK-PORCENTAGEM TO WRK-PORCENTAGEM-ED                00240322
              MOVE WRK-GAS TO WRK-GAS-ED                                00240422
              MOVE WRK-ALC TO WRK-ALC-ED                                00240522
                                                                        00240604
             DISPLAY 'VALOR GASOLINA .. R$' WRK-GAS-ED                  00240722
             DISPLAY 'VALOR ALCOOL   .. R$' WRK-ALC-ED                  00240822
             DISPLAY 'PORCENTAGEM    .. ' WRK-PORCENTAGEM-ED '%'        00250022
                                                                        00260021
             IF WRK-PORCENTAGEM LESS THAN OR EQUAL 70                   00270026
                 DISPLAY 'COMPRAR ALCOOL'                               00280022
             ELSE                                                       00290022
                 DISPLAY 'COMPRAR GASOLINA'                             00300022
             END-IF                                                     00302022
           ELSE                                                         00302225
             DISPLAY 'SOMENTE NUMEROS SAO PERMITIDOS'                   00302325
           END-IF.                                                      00303022
           STOP RUN.                                                    00310000
