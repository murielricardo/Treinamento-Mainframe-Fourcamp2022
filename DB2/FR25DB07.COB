      *=============================================================*   00001000
       IDENTIFICATION                            DIVISION.              00002000
      *=============================================================*   00003000
                                                                        00004000
       PROGRAM-ID. FR25DB07.                                            00005001
                                                                        00006000
      *=============================================================*   00007000
      *   AUTOR....:MURIEL ANDRIGUETO                               *   00007101
      *   ANALISTA.:IVAN PETRUCCI                                   *   00007201
      *   DATA ....:07/06/2022                                      *   00007300
      *-------------------------------------------------------------*   00007400
      *   OBJETIVO:LER FUNCIONARIOS COM SETOR ANALITICO DAS         *   00007504
      *            TABELAS RELACIONADAS IVAN.FUNC E IVAN.SETOR      *   00007604
      *            IMPREIMINDO OS RESULTADOS NO ARQUIVO             *   00007704
      *            DE SAIDA RELATSET                                *   00007804
      *                                                             *   00007900
      *-------------------------------------------------------------*   00008000
      *   BASE DE DADOS:                                            *   00008100
      *   TABELA.DB2..                                              *   00008200
      *    ------                                  INCLUDE/BOOK     *   00008300
      *   IVAN.FUNC                                #BKFUNC----      *   00008400
      *   IVAN.SETOR                               #BKSETOR---      *   00008500
      *-------------------------------------------------------------*   00008600
      *   MODULOS....:                             INCLUDE/BOOK     *   00008700
      *=============================================================*   00008800
                                                                        00008900
      *=============================================================*   00009000
       ENVIRONMENT                               DIVISION.              00009100
      *=============================================================*   00009200
                                                                        00009300
      *=============================================================*   00009400
       CONFIGURATION                               SECTION.             00009500
      *=============================================================*   00009600
       SPECIAL-NAMES.                                                   00009700
           DECIMAL-POINT IS COMMA.                                      00009800
                                                                        00009900
       INPUT-OUTPUT                                 SECTION.            00010000
       FILE-CONTROL.                                                    00010100
            SELECT RELATSET ASSIGN TO RELATSET                          00010201
                FILE STATUS IS WRK-FS-RELATSET.                         00010301
      *=============================================================*   00010400
       DATA                                      DIVISION.              00010500
      *=============================================================*   00010600
       FILE                                        SECTION.             00010700
       FD RELATSET                                                      00010801
           RECORDING MODE IS F                                          00010900
           LABEL RECORD IS STANDARD                                     00011000
           BLOCK CONTAINS 0 RECORDS.                                    00011100
                                                                        00011200
       01 FD-RELATSET       PIC X(135).                                 00011305
      *=============================================================*   00011400
       WORKING-STORAGE                             SECTION.             00011500
      *=============================================================*   00011600
                                                                        00011700
           EXEC SQL                                                     00011800
              INCLUDE #BKFUNC                                           00011900
           END-EXEC.                                                    00012000
                                                                        00012100
           EXEC SQL                                                     00012200
              INCLUDE #BKSETOR                                          00012300
           END-EXEC.                                                    00012400
                                                                        00012500
           EXEC SQL                                                     00013000
              INCLUDE SQLCA                                             00014000
           END-EXEC.                                                    00015000
                                                                        00016000
           EXEC SQL                                                     00017000
              DECLARE CFUNC CURSOR FOR                                  00018000
               SELECT ID,NOME,SALARIO,DATAADM,EMAIL,DESCSETOR           00019000
                FROM IVAN.FUNC F , IVAN.SETOR S                         00020000
                 WHERE F.SETOR = S.IDSETOR                              00021000
                ORDER BY ID                                             00021100
           END-EXEC.                                                    00021200
                                                                        00022000
      *--------LRECL 135--------------------------------------------*   00023001
       01 WRK-RELATSET.                                                 00023101
          05 WRK-ID           PIC 99999.                                00023200
          05 WRK-NOME         PIC X(30).                                00023300
          05 WRK-SALARIO      PIC 9999999999.                           00023504
          05 WRK-DATAADM      PIC X(10).                                00023600
          05 WRK-EMAIL        PIC X(40).                                00023700
          05 WRK-DESCSETOR    PIC X(40).                                00023801
                                                                        00023900
       77 WRK-REGLIDOS        PIC 9(03).                                00024000
       77 WRK-SALARIO-ACUM    PIC S9(8)V9(2) COMP.                      00024100
       77 WRK-MAIOR-SALARIO   PIC S9(8)V9(2) COMP.                      00024200
       77 WRJ-TEMP-SALARIO    PIC S9(8)V9(2) COMP.                      00024300
       77 WRK-MEDIA           PIC S9(8)V9(2) COMP.                      00024400
       77 WRK-SQLCODE         PIC -999.                                 00024500
       77 WRK-NULL-EMAIL      PIC S9(4) COMP.                           00024600
       77 WRK-FS-RELATSET     PIC 9(02).                                00024701
                                                                        00024800
      *=============================================================*   00024900
       PROCEDURE DIVISION.                                              00025000
      *=============================================================*   00025100
                                                                        00025200
      *-------------------------------------------------------------*   00025300
       0000-PRINCIPAL                           SECTION.                00025400
      *-------------------------------------------------------------*   00025500
            PERFORM  1000-INICIAR.                                      00025600
            PERFORM  2000-PROCESSAR UNTIL SQLCODE EQUAL 100.            00025700
            PERFORM  3000-FINALIZAR.                                    00025800
            STOP RUN.                                                   00025900
      *-------------------------------------------------------------*   00026001
       0000-99-FIM.              EXIT.                                  00026101
      *-------------------------------------------------------------*   00026201
      *-------------------------------------------------------------*   00026300
       1000-INICIAR                             SECTION.                00026400
      *-------------------------------------------------------------*   00026500
            EXEC SQL                                                    00026600
               OPEN CFUNC                                               00026700
            END-EXEC.                                                   00026800
             EVALUATE SQLCODE                                           00026900
              WHEN 0                                                    00027000
                PERFORM 2100-LER-FUNC                                   00027100
              WHEN 100                                                  00027200
                DISPLAY 'SEM FUNCIONARIO'                               00027300
              WHEN OTHER                                                00027400
                MOVE SQLCODE TO WRK-SQLCODE                             00027500
                DISPLAY 'ERRO ' WRK-SQLCODE ' NO OPEN DO CURSOR.'       00027600
                STOP RUN                                                00027700
             END-EVALUATE.                                              00027800
                                                                        00027900
            OPEN OUTPUT RELATSET.                                       00028001
                                                                        00028100
            IF WRK-FS-RELATSET NOT EQUAL 0                              00028201
               DISPLAY ' ERRO NO OPEN DO ARQUIVO'                       00028300
                  STOP RUN                                              00028400
            END-IF.                                                     00029000
      *-------------------------------------------------------------*   00030000
       1000-99-FIM.              EXIT.                                  00030100
      *-------------------------------------------------------------*   00030200
      *-------------------------------------------------------------*   00030400
       2000-PROCESSAR                           SECTION.                00030500
      *-------------------------------------------------------------*   00030600
             IF WRK-NULL-EMAIL = 0                                      00030700
              MOVE DB2-EMAIL   TO WRK-EMAIL                             00030800
             ELSE                                                       00030900
              MOVE '--SEM EMAIL ' TO WRK-EMAIL                          00031000
             END-IF                                                     00032000
              PERFORM 2100-LER-FUNC.                                    00033000
                                                                        00034000
             MOVE DB2-ID        TO WRK-ID.                              00035001
             MOVE DB2-NOME      TO WRK-NOME.                            00035101
             MOVE DB2-SALARIO   TO WRK-SALARIO.                         00035301
             MOVE DB2-DATAADM   TO WRK-DATAADM.                         00035401
             MOVE DB2-DESCSETOR TO WRK-DESCSETOR.                       00035501
             WRITE FD-RELATSET FROM WRK-RELATSET.                       00035601
                                                                        00036200
      *-------------------------------------------------------------*   00036300
       2000-99-FIM.              EXIT.                                  00036400
      *-------------------------------------------------------------*   00036500
      *-------------------------------------------------------------*   00036600
       2100-LER-FUNC                                SECTION.            00036700
      *-------------------------------------------------------------*   00036800
           EXEC SQL                                                     00036900
            FETCH CFUNC                                                 00037000
             INTO :DB2-ID,                                              00037100
                  :DB2-NOME,                                            00037200
                  :DB2-SALARIO,                                         00037400
                  :DB2-DATAADM,                                         00037500
                  :DB2-EMAIL     :WRK-NULL-EMAIL,                       00037602
                  :DB2-DESCSETOR                                        00037701
            END-EXEC.                                                   00038000
            EVALUATE SQLCODE                                            00039000
             WHEN 0                                                     00040000
               ADD 1 TO WRK-REGLIDOS                                    00041000
                ADD DB2-SALARIO TO WRK-SALARIO-ACUM                     00041100
               CONTINUE                                                 00041200
             WHEN 100                                                   00041300
              DISPLAY ' FINAL DE ARQUIVO'                               00041400
             WHEN OTHER                                                 00041500
               MOVE SQLCODE TO WRK-SQLCODE                              00041600
               DISPLAY 'ERRO NA LEITURA ' WRK-SQLCODE                   00041700
             END-EVALUATE.                                              00041800
                                                                        00041901
            PERFORM 2200-CALCULO-SALARIO.                               00042003
      *-------------------------------------------------------------*   00042900
       2100-99-FIM.              EXIT.                                  00043000
      *-------------------------------------------------------------*   00044000
      *-------------------------------------------------------------*   00044201
       2200-CALCULO-SALARIO                         SECTION.            00044301
      *-------------------------------------------------------------*   00044401
            MOVE DB2-SALARIO TO WRK-MAIOR-SALARIO.                      00046701
                                                                        00046801
            IF WRJ-TEMP-SALARIO EQUAL 0                                 00046901
              MOVE WRK-MAIOR-SALARIO TO WRJ-TEMP-SALARIO                00047001
            ELSE                                                        00047101
              IF WRK-MAIOR-SALARIO GREATER WRJ-TEMP-SALARIO             00047201
                MOVE WRK-MAIOR-SALARIO TO WRJ-TEMP-SALARIO              00047301
            END-IF.                                                     00047401
      *-------------------------------------------------------------*   00047501
       2200-99-FIM.              EXIT.                                  00047601
      *-------------------------------------------------------------*   00047701
      *-------------------------------------------------------------*   00048001
       3000-FINALIZAR                               SECTION.            00048100
      *-------------------------------------------------------------*   00048201
              MOVE WRJ-TEMP-SALARIO TO WRK-MAIOR-SALARIO                00048300
                                                                        00048400
              EXEC SQL                                                  00048500
                CLOSE CFUNC                                             00048600
              END-EXEC.                                                 00048700
              CLOSE RELATSET.                                           00048801
              DISPLAY '   '                                             00048900
              DISPLAY 'REGISTROS LIDOS.......' WRK-REGLIDOS.            00049000
              DISPLAY 'MAIOR SALARIO.........' WRK-MAIOR-SALARIO.       00049100
              DISPLAY 'SALARIO ACUMULADO.....' WRK-SALARIO-ACUM.        00049200
              DIVIDE WRK-SALARIO-ACUM BY WRK-REGLIDOS                   00049300
                    GIVING WRK-MEDIA.                                   00049400
              DISPLAY 'MEDIA DOS SALARIOS....' WRK-MEDIA.               00049500
              DISPLAY '-----FIM DO PROGRAMA----- '.                     00049600
      *-------------------------------------------------------------*   00049700
       3000-99-FIM.              EXIT.                                  00049800
      *-------------------------------------------------------------*   00049900
      *-------------------------------------------------------------*   00050100
       9000-TRATAERROS                              SECTION.            00050200
      *-------------------------------------------------------------*   00050300
                                                                        00050400
      *-------------------------------------------------------------*   00050500
       9000-99-FIM.              EXIT.                                  00051000
      *-------------------------------------------------------------*   00060000
