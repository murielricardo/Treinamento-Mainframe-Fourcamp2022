      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR25CB20.                                            00040002
                                                                        00050000
      *=====================================================            00060000
      *  AUTOR: MURIEL                                                  00070002
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
          05 WRK-CODIGO        PIC 9(05)    VALUE ZEROS.                00140000
          05 WRK-PRODUTO       PIC X(15)    VALUE SPACES.               00150000
          05 WRK-VALOR         PIC 9(08)V99 VALUE ZEROS.                00151000
          05 WRK-ESTADO        PIC X(02)    VALUE SPACES.               00152000
       77 WRK-FRETE            PIC 9(08)V99 VALUE ZEROS.                00153000
                                                                        00154115
      *---------------------MASCARAS----------------------*             00154215
                                                                        00154315
       77 WRK-VALOR-ED       PIC ZZ.ZZ9.999,99.                         00154415
       77 WRK-FRETE-ED       PIC ZZ.ZZZ.999,99.                         00154515
                                                                        00154615
      *====================================================             00155000
       PROCEDURE                                 DIVISION.              00156000
      *====================================================             00157000
           ACCEPT WRK-DADOS FROM SYSIN.                                 00158003
      *----------------- VERIFICA ESTADO -----------------*             00159646
                                                                        00159746
           IF WRK-ESTADO NOT EQUAL 'SP' AND 'RJ' AND 'MG'               00160039
              DISPLAY 'NAO ENTREGAMOS'                                  00171039
                                                                        00172046
      *------------ CASO ESTADO SEJA VALIDO --------------*             00180046
           ELSE                                                         00180141
               EVALUATE WRK-ESTADO                                      00180641
                   WHEN 'SP'                                            00180741
                    COMPUTE WRK-FRETE = WRK-VALOR * 0,10                00180841
                   WHEN 'RJ'                                            00180941
                    COMPUTE WRK-FRETE = WRK-VALOR * 0,15                00181041
                   WHEN 'MG'                                            00181141
                    COMPUTE WRK-FRETE = WRK-VALOR * 0,20                00181241
               END-EVALUATE                                             00181741
      *---------------- VERIFICA VALOR -------------------*             00181846
                                                                        00181946
               IF WRK-VALOR GREATER 1000                                00182341
                                                                        00182446
      *----------- CASO VALOR MAIOR QUE 1000 -------------*             00182546
                                                                        00182646
                    MOVE WRK-VALOR TO WRK-VALOR-ED                      00182741
                    MOVE WRK-FRETE TO WRK-FRETE-ED                      00182841
                   DISPLAY 'CODIGO....   ' WRK-CODIGO                   00182941
                   DISPLAY 'PRODUTO...   ' WRK-PRODUTO                  00183041
                   DISPLAY 'ESTADO ...   ' WRK-ESTADO                   00183141
                   DISPLAY 'VALOR .... R$' WRK-VALOR-ED                 00183241
                   DISPLAY 'FRETE .... R$' WRK-FRETE-ED                 00183341
                                                                        00183446
      *----------- CASO VALOR MENOR QUE 1000 -------------*             00183546
                                                                        00183646
               ELSE                                                     00184041
                 DISPLAY 'RETIRAR NA LOJA'                              00185041
               END-IF                                                   00186041
           END-IF.                                                      00303539
           STOP RUN.                                                    00306824
