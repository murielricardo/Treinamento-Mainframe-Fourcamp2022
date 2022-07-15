      *===============================================================* 00001000
       IDENTIFICATION                                        DIVISION.  00002000
      *===============================================================* 00003000
                                                                        00004000
       PROGRAM-ID.  FR25EX02.                                           00005041
                                                                        00006000
      *===============================================================* 00007000
      *     PROGRAMA....: FR25EX02.                                   * 00008041
      *     PROGRAMADOR.: MURIEL ANDRIGUETO                           * 00009000
      *     EMPRESA.....: FOURSYS                                     * 00010000
      *     ANALISTA....: IVAN SANCHES                                * 00010100
      *     DATA........: 31/05/2022                                  * 00010242
      *---------------------------------------------------------------* 00010300
      *     OBJETIVO....: O PROGRAMA TEM A FINALIDADE DE RECEBER      * 00010442
      *                   DADOS DOS ARQUIVOS "FUNC" E "PROJ"          * 00010542
      *                   RELACIONA-LOS POR MEIO DE UM BALANCE LINE E * 00010642
      *                   GRAVAR OS DADOS NO ARQUIVO "FUNPROJ" PARA   * 00010742
      *                   IDS QUE FOREM IGUAIS EM AMBOS OS REGISTRO   * 00010842
      *                   DOS  ARQUIVOS DE ENTRADA.                   * 00010942
      *---------------------------------------------------------------* 00013042
      *     ARQUIVOS....:                            INCLUDE/BOOK     * 00014042
      *      DDNAME               I/O                                 * 00014142
      *      FUNC                  I                                  * 00014242
      *      PROJ                  I                                  * 00014342
      *      FUNPROJ               0                                  * 00014442
      *---------------------------------------------------------------* 00014600
      *     MODULOS.....:                                             * 00014742
      *     GRAVALOG - TRATAMENTO DE ERROS              #GLOG         * 00014842
      *---------------------------------------------------------------* 00014942
                                                                        00015000
      *===============================================================* 00015100
       ENVIRONMENT                                           DIVISION.  00015200
      *===============================================================* 00015300
      *---------------------------------------------------------------* 00016000
       CONFIGURATION                                          SECTION.  00016100
      *---------------------------------------------------------------* 00016200
       SPECIAL-NAMES.                                                   00016300
           DECIMAL-POINT IS COMMA.                                      00016400
                                                                        00016500
                                                                        00016600
       INPUT-OUTPUT                                           SECTION.  00016700
       FILE-CONTROL.                                                    00016800
           SELECT FUNC      ASSIGN TO FUNC                              00016900
               FILE STATUS  IS WRK-FS-FUNC.                             00017000
                                                                        00017100
           SELECT PROJ      ASSIGN TO PROJ                              00017200
               FILE STATUS  IS WRK-FS-PROJ.                             00017300
                                                                        00017400
           SELECT FUNPROJ   ASSIGN TO FUNPROJ                           00017500
               FILE STATUS  IS WRK-FS-FUNPROJ.                          00017600
      *===============================================================* 00017700
       DATA                                                  DIVISION.  00017800
      *===============================================================* 00017900
       FILE                                                   SECTION.  00018000
      *------------------------LRECL 39-------------------------------- 00018142
       FD FUNC                                                          00018200
           RECORDING MODE IS F                                          00018300
           LABEL RECORD IS STANDARD                                     00018400
           BLOCK CONTAINS 0 RECORDS.                                    00018500
                                                                        00018600
       01 FD-FUNC.                                                      00018742
          05 FD-FUNC-IDFUNC     PIC X(05).                              00018842
          05 FD-FUNC-NOMEFUNC   PIC X(30).                              00018942
          05 FD-FUNC-SETORFUNC  PIC X(04).                              00019042
                                                                        00019142
      *------------------------LRECL 28-------------------------------- 00019342
       FD PROJ                                                          00019400
           RECORDING MODE IS F                                          00019500
           LABEL RECORD IS STANDARD                                     00019600
           BLOCK CONTAINS 0 RECORDS.                                    00019700
                                                                        00019800
       01 FD-PROJ.                                                      00019942
          05 FD-PROJ-IDFUNC     PIC X(05).                              00020042
          05 FD-PROJ-PROJETO    PIC X(20).                              00020142
          05 FD-PROJ-QTHORAS    PIC 9(03).                              00020242
                                                                        00020342
      *---------------------------------------------------------------- 00020442
       FD FUNPROJ                                                       00020700
           RECORDING MODE IS F                                          00020800
           BLOCK CONTAINS 0 RECORDS.                                    00020900
       01 FD-FUNPROJ PIC X(58).                                         00021042
                                                                        00021700
      *---------------------------------------------------------------* 00021800
       WORKING-STORAGE                                        SECTION.  00021900
      *---------------------------------------------------------------* 00022000
      *------------------------LRECL 58-------------------------------- 00022142
       01 WRK-FD-FUNPROJ.                                               00022242
          05 WRK-FD-IDFUNC     PIC X(05).                               00022342
          05 WRK-FD-NOMEFUNC   PIC X(30).                               00022442
          05 WRK-FD-PROJETO    PIC X(20).                               00022542
          05 WRK-FD-QTHORAS    PIC 9(03).                               00022642
                                                                        00022742
      *---------------------------------------------------------------* 00023000
       01 FILLER PIC X(50) VALUE                                        00023100
              '------------------#GLOG--------------------------'.      00023200
      *---------------------------------------------------------------* 00023300
                                                                        00023400
      *---------------------------------------------------------------* 00023500
      * PROGRAMA 08            LRECL 48                               * 00023600
      * SECAO    04                                                   * 00023700
      * MENSAGEM 30                                                   * 00023800
      * STATUS   02                                                   * 00023900
      *---------------------------------------------------------------* 00024000
                                                                        00024100
          COPY '#GLOG'.                                                 00024200
                                                                        00024300
      *---------------------------------------------------------------* 00024400
       01 FILLER PIC X(50) VALUE                                        00024500
              '----------VARIAVEIS DE FILE STATUS---------------'.      00024600
      *---------------------------------------------------------------* 00024700
       77 WRK-FS-FUNC      PIC 9(02).                                   00024900
       77 WRK-FS-PROJ      PIC 9(02).                                   00025000
       77 WRK-FS-FUNPROJ   PIC 9(02).                                   00025100
                                                                        00025200
      *---------------------------------------------------------------* 00025300
       01 FILLER PIC X(50) VALUE                                        00025400
              '-------VARIAVEIS PARA ACUMULACAO/EXIBICAO--------'.      00025500
      *---------------------------------------------------------------* 00025600
       77 WRK-GRAVALOG       PIC X(8) VALUE 'GRAVALOG'.                 00025800
       77 WRK-REGLIDOS       PIC 9(02) VALUE ZEROS.                     00025942
       77 WRK-REGGRAV        PIC 9(02) VALUE ZEROS.                     00026042
                                                                        00026142
      *---------------------------------------------------------------* 00026800
       01 FILLER PIC X(50) VALUE                                        00027000
              '------------VARIAVEIS PARA MENSAGEM--------------'.      00028000
      *---------------------------------------------------------------* 00029000
       77 WRK-MSG           PIC X(30) VALUE SPACES.                     00029242
       77 WRK-MSGFUNCVAZIO  PIC X(35) VALUE                             00029342
                               ' ERRO FUNC - ARQUIVO VAZIO! '.          00029442
       77 WRK-MSGPROJVAZIO  PIC X(35) VALUE                             00029542
                               ' ERRO PROJ - ARQUIVO VAZIO! '.          00029642
       77 WRK-MSGFINALARQ   PIC X(16) VALUE ' FIM DE ARQUIVO '.         00029742
                                                                        00029842
       77 WRK-MSGREGLIDOS   PIC X(20) VALUE ' REGISTRO LIDOS:    '.     00029942
       77 WRK-MSGREGGRAV    PIC X(20) VALUE ' REGISTRO GRAVADOS: '.     00030042
       77 WRK-MSGIDPROG     PIC X(08) VALUE 'FR25EX02'.                 00030142
       77 WRK-MSGPART       PIC X(04) VALUE '1000'.                     00030242
       77 WRK-ERROFUNC      PIC X(25) VALUE 'ERRO OPEN FUNC - '.        00030342
       77 WRK-ERROPROJ      PIC X(25) VALUE 'ERRO OPEN PROJ - '.        00030442
       77 WRK-ERROFUNVPROJ  PIC X(25) VALUE 'ERRO OPEN FUNPROJ - '.     00030542
                                                                        00030742
      *===============================================================* 00030839
       PROCEDURE                                           DIVISION.    00030939
      *===============================================================* 00031039
      *---------------------------------------------------------------* 00031139
       0000-PRINCIPAL                                         SECTION.  00031239
      *---------------------------------------------------------------* 00031339
                 PERFORM 1000-INICIAR.                                  00031439
                  PERFORM 1200-TESTA-VAZIO.                             00031539
                 PERFORM 2000-PROCESSAR UNTIL WRK-FS-FUNC EQUAL 10 AND  00031639
                                              WRK-FS-PROJ EQUAL 10.     00031739
                 PERFORM 3000-FINALIZAR.                                00031839
                 STOP RUN.                                              00031939
      *---------------------------------------------------------------* 00032039
       0000-99-FIM.                                      EXIT.          00032139
      *---------------------------------------------------------------* 00032239
      *---------------------------------------------------------------* 00032339
       1000-INICIAR                                           SECTION.  00032439
      *---------------------------------------------------------------* 00032539
                 OPEN INPUT FUNC PROJ                                   00032639
                     OUTPUT FUNPROJ.                                    00032739
                 PERFORM 1100-TESTA-STATUS.                             00032839
      *---------------------------------------------------------------* 00032939
       1000-99-FIM.                                      EXIT.          00033039
      *---------------------------------------------------------------* 00033139
      *---------------------------------------------------------------* 00033239
       1100-TESTA-STATUS                                      SECTION.  00033339
      *---------------------------------------------------------------* 00033439
                 IF WRK-FS-FUNC NOT EQUAL 0                             00033539
                   MOVE WRK-MSGIDPROG    TO WRK-PROGRAMA                00033642
                   MOVE WRK-MSGPART      TO WRK-SECAO                   00033742
                   MOVE WRK-ERROFUNC     TO WRK-MENSAGEM                00033842
                   MOVE WRK-FS-FUNC      TO WRK-STATUS                  00033942
                 PERFORM 9000-TRATA-ERROS                               00034039
                 END-IF.                                                00034139
                                                                        00034239
                 IF WRK-FS-PROJ NOT EQUAL 0                             00034339
                   MOVE WRK-MSGIDPROG    TO WRK-PROGRAMA                00034442
                   MOVE WRK-MSGPART      TO WRK-SECAO                   00034542
                   MOVE WRK-ERROPROJ     TO WRK-MENSAGEM                00034642
                   MOVE WRK-FS-PROJ      TO WRK-STATUS                  00034742
                 PERFORM 9000-TRATA-ERROS                               00034839
                 END-IF.                                                00034939
                                                                        00035039
                 IF WRK-FS-FUNPROJ NOT EQUAL 0                          00035139
                   MOVE WRK-MSGIDPROG    TO WRK-PROGRAMA                00035242
                   MOVE WRK-MSGPART      TO WRK-SECAO                   00035342
                   MOVE WRK-ERROFUNVPROJ TO WRK-MENSAGEM                00035442
                   MOVE WRK-FS-FUNPROJ   TO WRK-STATUS                  00035542
                 PERFORM 9000-TRATA-ERROS                               00035639
                 END-IF.                                                00035739
      *---------------------------------------------------------------* 00035839
       1100-99-FIM.                                      EXIT.          00035939
      *---------------------------------------------------------------* 00036039
      *---------------------------------------------------------------* 00036139
       1200-TESTA-VAZIO                                       SECTION.  00036239
      *---------------------------------------------------------------* 00036339
                 READ FUNC.                                             00036439
                 READ PROJ.                                             00036539
                  IF WRK-FS-FUNC EQUAL 10                               00036639
                    DISPLAY WRK-MSGFUNCVAZIO                            00036842
                  END-IF.                                               00036939
                                                                        00037039
                  IF WRK-FS-PROJ EQUAL 10                               00037139
                    DISPLAY WRK-MSGPROJVAZIO                            00037342
                  END-IF.                                               00037400
      *---------------------------------------------------------------* 00037500
       1200-99-FIM.                                      EXIT.          00037600
      *---------------------------------------------------------------* 00037700
      *---------------------------------------------------------------* 00037800
       2000-PROCESSAR                                         SECTION.  00037900
      *---------------------------------------------------------------* 00038000
                    EVALUATE TRUE                                       00038400
                     WHEN FD-FUNC-IDFUNC LESS FD-PROJ-IDFUNC            00038532
                      READ FUNC                                         00040042
                       ADD 1 TO WRK-REGLIDOS                            00040142
                                                                        00040242
                     WHEN FD-FUNC-IDFUNC EQUAL FD-PROJ-IDFUNC           00040300
                         MOVE FD-FUNC-IDFUNC   TO  WRK-FD-IDFUNC        00040542
                         MOVE FD-FUNC-NOMEFUNC TO  WRK-FD-NOMEFUNC      00040642
                         MOVE FD-PROJ-PROJETO  TO  WRK-FD-PROJETO       00040742
                         MOVE FD-PROJ-QTHORAS  TO  WRK-FD-QTHORAS       00040842
                          WRITE  FD-FUNPROJ FROM WRK-FD-FUNPROJ         00040942
                           ADD 1 TO WRK-REGGRAV                         00041042
                            READ PROJ                                   00041218
                             IF WRK-FS-PROJ EQUAL 10                    00041318
                              MOVE HIGH-VALUES TO FD-PROJ-IDFUNC        00041418
                             END-IF                                     00041518
                    END-EVALUATE.                                       00044042
      *---------------------------------------------------------------* 00044800
       2000-99-FIM.                                      EXIT.          00044900
      *---------------------------------------------------------------* 00045000
                                                                        00045100
      *---------------------------------------------------------------* 00045200
       3000-FINALIZAR                                         SECTION.  00045300
      *---------------------------------------------------------------* 00045400
                 DISPLAY WRK-MSGREGLIDOS WRK-REGLIDOS                   00045542
                 DISPLAY WRK-MSGREGGRAV  WRK-REGGRAV                    00045642
                 CLOSE FUNC PROJ FUNPROJ.                               00046000
                 MOVE WRK-MSGFINALARQ TO WRK-MSG                        00047000
                 DISPLAY WRK-MSG.                                       00048000
      *---------------------------------------------------------------* 00049000
       3000-99-FIM.                                      EXIT.          00049100
      *---------------------------------------------------------------* 00049200
      *---------------------------------------------------------------* 00049300
       9000-TRATA-ERROS                                       SECTION.  00049400
      *---------------------------------------------------------------* 00049500
                 CALL WRK-GRAVALOG USING WRK-DADOS.                     00049600
                 GOBACK.                                                00049700
      *---------------------------------------------------------------* 00049800
       9000-99-FIM.                                      EXIT.          00049900
      *---------------------------------------------------------------* 00050000
