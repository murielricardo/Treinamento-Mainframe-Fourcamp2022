      *=============================================================*   00001000
       IDENTIFICATION                            DIVISION.              00002000
      *=============================================================*   00003000
                                                                        00004000
       PROGRAM-ID. FR25DB08.                                            00005000
                                                                        00006000
      *=============================================================*   00007000
      *   AUTOR....:MURIEL ANDRIGUETO                               *   00007100
      *   ANALISTA.:IVAN PETRUCCI                                   *   00007200
      *   DATA ....:13/06/2022                                      *   00007300
      *-------------------------------------------------------------*   00007400
      *   OBJETIVO:LER FUNCIONARIOS COM SETOR ANALITICO DAS         *   00007500
      *            TABELAS RELACIONADAS IVAN.FUNC E IVAN.SETOR      *   00007600
      *            IMPREIMINDO OS RESULTADOS NO ARQUIVO             *   00007700
      *            DE SAIDA RELATSET                                *   00007800
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
            SELECT RELATSET ASSIGN TO RELATSET                          00010200
                FILE STATUS IS WRK-FS-RELATSET.                         00010300
      *=============================================================*   00010400
       DATA                                      DIVISION.              00010500
      *=============================================================*   00010600
       FILE                                        SECTION.             00010700
       FD RELATSET                                                      00010800
           RECORDING MODE IS F                                          00010900
           LABEL RECORD IS STANDARD                                     00011000
           BLOCK CONTAINS 0 RECORDS.                                    00011100
                                                                        00011200
       01 FD-RELATSET       PIC X(135).                                 00011300
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
           EXEC SQL                                                     00012600
              INCLUDE SQLCA                                             00012700
           END-EXEC.                                                    00012800
                                                                        00012900
           EXEC SQL                                                     00013000
              DECLARE CFUNC CURSOR FOR                                  00014000
               SELECT ID,NOME,SALARIO,DATAADM,EMAIL                     00015021
                FROM IVAN.FUNC F                                        00016027
                WHERE NOT EXISTS(SELECT IDSETOR FROM IVAN.SETOR S       00018328
                                  WHERE F.SETOR = S.IDSETOR)            00018422
               ORDER BY ID                                              00018609
           END-EXEC.                                                    00019000
                                                                        00020000
      *--------LRECL 135--------------------------------------------*   00021000
                                                                        00021107
       01 WRK-RELATSET.                                                 00022000
          05 WRK-ID           PIC 99999.                                00023000
          05 WRK-NOME         PIC X(30).                                00023100
          05 WRK-SALARIO      PIC 9999999999.                           00023200
          05 WRK-DATAADM      PIC X(10).                                00023300
          05 WRK-EMAIL        PIC X(40).                                00023400
                                                                        00023600
       77 WRK-REGLIDOS        PIC 9(03).                                00023700
       77 WRK-SALARIO-ACUM    PIC S9(8)V9(2) COMP.                      00023800
       77 WRK-MAIOR-SALARIO   PIC S9(8)V9(2) COMP.                      00023900
       77 WRJ-TEMP-SALARIO    PIC S9(8)V9(2) COMP.                      00024000
       77 WRK-MEDIA           PIC S9(8)V9(2) COMP.                      00024100
       77 WRK-SQLCODE         PIC -999.                                 00024200
       77 WRK-NULL-EMAIL      PIC S9(4) COMP.                           00024300
       77 WRK-FS-RELATSET     PIC 9(02).                                00024400
                                                                        00024500
      *=============================================================*   00024600
       PROCEDURE DIVISION.                                              00024700
      *=============================================================*   00024800
                                                                        00024900
      *-------------------------------------------------------------*   00025000
       0000-PRINCIPAL                           SECTION.                00025100
      *-------------------------------------------------------------*   00025200
            PERFORM  1000-INICIAR.                                      00025300
            PERFORM  2000-PROCESSAR UNTIL SQLCODE EQUAL 100.            00025400
            PERFORM  3000-FINALIZAR.                                    00025500
            STOP RUN.                                                   00025600
      *-------------------------------------------------------------*   00025700
       0000-99-FIM.              EXIT.                                  00025800
      *-------------------------------------------------------------*   00025900
      *-------------------------------------------------------------*   00026000
       1000-INICIAR                             SECTION.                00026100
      *-------------------------------------------------------------*   00026200
            EXEC SQL                                                    00026300
               OPEN CFUNC                                               00026400
            END-EXEC.                                                   00026500
             EVALUATE SQLCODE                                           00026600
              WHEN 0                                                    00026700
                PERFORM 2100-LER-FUNC                                   00026800
              WHEN 100                                                  00026900
                DISPLAY 'SEM FUNCIONARIO'                               00027000
              WHEN OTHER                                                00027100
                MOVE SQLCODE TO WRK-SQLCODE                             00027200
                DISPLAY 'ERRO ' WRK-SQLCODE ' NO OPEN DO CURSOR.'       00027300
                STOP RUN                                                00027400
             END-EVALUATE.                                              00027500
                                                                        00027600
            OPEN OUTPUT RELATSET.                                       00027700
                                                                        00027800
            IF WRK-FS-RELATSET NOT EQUAL 0                              00027900
               DISPLAY ' ERRO NO OPEN DO ARQUIVO'                       00028000
                  STOP RUN                                              00028100
            END-IF.                                                     00028200
      *-------------------------------------------------------------*   00028300
       1000-99-FIM.              EXIT.                                  00028400
      *-------------------------------------------------------------*   00028500
      *-------------------------------------------------------------*   00028600
       2000-PROCESSAR                           SECTION.                00028700
      *-------------------------------------------------------------*   00028800
             IF WRK-NULL-EMAIL = 0                                      00028900
              MOVE DB2-EMAIL   TO WRK-EMAIL                             00029000
             ELSE                                                       00030000
              MOVE '--SEM EMAIL ' TO WRK-EMAIL                          00031000
             END-IF.                                                    00032030
                                                                        00032131
                                                                        00036029
            PERFORM 2200-CALCULO-SALARIO.                               00036129
             MOVE DB2-ID        TO WRK-ID.                              00036200
             MOVE DB2-NOME      TO WRK-NOME.                            00036300
             MOVE DB2-SALARIO   TO WRK-SALARIO.                         00036400
             MOVE DB2-DATAADM   TO WRK-DATAADM.                         00036500
             WRITE FD-RELATSET FROM WRK-RELATSET.                       00036600
                                                                        00036731
             PERFORM 2100-LER-FUNC.                                     00036831
                                                                        00036900
      *-------------------------------------------------------------*   00037000
       2000-99-FIM.              EXIT.                                  00037100
      *-------------------------------------------------------------*   00037200
      *-------------------------------------------------------------*   00037300
       2100-LER-FUNC                                SECTION.            00037431
      *-------------------------------------------------------------*   00037500
           EXEC SQL                                                     00037631
            FETCH CFUNC                                                 00037731
             INTO :DB2-ID,                                              00037831
                  :DB2-NOME,                                            00037931
                  :DB2-SALARIO,                                         00038031
                  :DB2-DATAADM,                                         00038131
                  :DB2-EMAIL     :WRK-NULL-EMAIL                        00038231
            END-EXEC.                                                   00038331
            EVALUATE SQLCODE                                            00039031
             WHEN 0                                                     00040031
               ADD 1 TO WRK-REGLIDOS                                    00041031
                ADD DB2-SALARIO TO WRK-SALARIO-ACUM                     00041131
               CONTINUE                                                 00041231
             WHEN 100                                                   00041331
              DISPLAY ' FINAL DE ARQUIVO'                               00041431
             WHEN OTHER                                                 00041531
               MOVE SQLCODE TO WRK-SQLCODE                              00041631
               DISPLAY 'ERRO NA LEITURA ' WRK-SQLCODE                   00041731
             END-EVALUATE.                                              00041831
                                                                        00041931
            PERFORM 2200-CALCULO-SALARIO.                               00042031
      *-------------------------------------------------------------*   00042100
       2100-99-FIM.              EXIT.                                  00042231
      *-------------------------------------------------------------*   00042300
      *-------------------------------------------------------------*   00042400
       2200-CALCULO-SALARIO                         SECTION.            00042500
      *-------------------------------------------------------------*   00042600
            MOVE DB2-SALARIO TO WRK-MAIOR-SALARIO.                      00042700
                                                                        00042800
            IF WRJ-TEMP-SALARIO EQUAL 0                                 00042900
              MOVE WRK-MAIOR-SALARIO TO WRJ-TEMP-SALARIO                00043000
            ELSE                                                        00044000
              IF WRK-MAIOR-SALARIO GREATER WRJ-TEMP-SALARIO             00045000
                MOVE WRK-MAIOR-SALARIO TO WRJ-TEMP-SALARIO              00046000
            END-IF.                                                     00047000
      *-------------------------------------------------------------*   00047100
       2200-99-FIM.              EXIT.                                  00047200
      *-------------------------------------------------------------*   00047300
      *-------------------------------------------------------------*   00047400
       3000-FINALIZAR                               SECTION.            00047500
      *-------------------------------------------------------------*   00047600
              MOVE WRJ-TEMP-SALARIO TO WRK-MAIOR-SALARIO                00047700
                                                                        00047800
              EXEC SQL                                                  00047900
                CLOSE CFUNC                                             00048000
              END-EXEC.                                                 00048100
              CLOSE RELATSET.                                           00048200
              DISPLAY '   '                                             00048300
              DISPLAY 'REGISTROS LIDOS.......' WRK-REGLIDOS.            00048400
              DISPLAY 'MAIOR SALARIO.........' WRK-MAIOR-SALARIO.       00048500
              DISPLAY 'SALARIO ACUMULADO.....' WRK-SALARIO-ACUM.        00048600
              DIVIDE WRK-SALARIO-ACUM BY WRK-REGLIDOS                   00048700
                    GIVING WRK-MEDIA.                                   00048800
              DISPLAY 'MEDIA DOS SALARIOS....' WRK-MEDIA.               00048900
              DISPLAY '-----FIM DO PROGRAMA----- '.                     00049000
      *-------------------------------------------------------------*   00049100
       3000-99-FIM.              EXIT.                                  00049200
      *-------------------------------------------------------------*   00049300
      *-------------------------------------------------------------*   00049400
       9000-TRATAERROS                              SECTION.            00049500
      *-------------------------------------------------------------*   00049600
                                                                        00049700
      *-------------------------------------------------------------*   00049800
       9000-99-FIM.              EXIT.                                  00049900
      *-------------------------------------------------------------*   00050000
