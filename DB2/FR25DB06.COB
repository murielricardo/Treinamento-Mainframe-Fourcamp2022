      *=============================================================*   00001000
       IDENTIFICATION                            DIVISION.              00002000
      *=============================================================*   00003000
                                                                        00004000
       PROGRAM-ID. FR25DB06.                                            00005000
                                                                        00006000
      *=============================================================*   00007000
      *   AUTOR....:MURIEL ANDRIGUETO                               *   00007100
      *   ANALISTA.:IVAN PETRUCCI                                   *   00007200
      *   DATA ....:10/06/2022                                      *   00007307
      *-------------------------------------------------------------*   00007400
      *   OBJETIVO:LER TABELA IVAN.FUNC E GERAR SAIDA               *   00007507
      *            CONSISTIDA NO ARQUIVO RELDB2.                    *   00007607
      *                                                             *   00007700
      *                                                             *   00007800
      *-------------------------------------------------------------*   00007900
      *  BASE  DE DADOS:                                            *   00008007
      *                     I/O           INCLUDE/BOOK              *   00008107
      *   IVAN.FUNC          I               #BKFUNC                *   00008207
      *-------------------------------------------------------------*   00008307
      *  ARQUIVOS:                                                  *   00008407
      *    DDNAME           I/O             COPY/BOOK               *   00008507
      *    RELDB2            O               --------               *   00008607
      *=============================================================*   00008707
                                                                        00008800
      *=============================================================*   00008900
       ENVIRONMENT                               DIVISION.              00009000
      *=============================================================*   00009100
                                                                        00009200
      *=============================================================*   00009300
       CONFIGURATION                               SECTION.             00009400
      *=============================================================*   00009500
       SPECIAL-NAMES.                                                   00009600
           DECIMAL-POINT IS COMMA.                                      00009700
                                                                        00009800
       INPUT-OUTPUT                                 SECTION.            00009900
       FILE-CONTROL.                                                    00010000
            SELECT RELDB2 ASSIGN TO RELDB2                              00010100
                FILE STATUS IS WRK-FS-RELDB2.                           00010200
      *=============================================================*   00010300
       DATA                                      DIVISION.              00010400
      *=============================================================*   00010500
       FILE                                        SECTION.             00010600
       FD RELDB2                                                        00010700
           RECORDING MODE IS F                                          00010800
           LABEL RECORD IS STANDARD                                     00010900
           BLOCK CONTAINS 0 RECORDS.                                    00011000
                                                                        00011103
       01 FD-RELDB2         PIC X(99).                                  00012003
      *=============================================================*   00012600
       WORKING-STORAGE                             SECTION.             00012700
      *=============================================================*   00012800
                                                                        00013000
           EXEC SQL                                                     00014000
              INCLUDE #BKFUNC                                           00015000
           END-EXEC.                                                    00016000
                                                                        00017000
           EXEC SQL                                                     00018000
              INCLUDE SQLCA                                             00019000
           END-EXEC.                                                    00020000
                                                                        00021000
           EXEC SQL                                                     00022000
              DECLARE CFUNC CURSOR FOR                                  00023000
               SELECT * FROM IVAN.FUNC                                  00023100
                ORDER BY ID                                             00023200
           END-EXEC.                                                    00023300
                                                                        00023403
      *--------LRECL 99---------------------------------------------*   00023503
       01 WRK-RELDB2.                                                   00023603
          05 WRK-ID           PIC 99999.                                00023704
          05 WRK-NOME         PIC X(30).                                00023804
          05 WRK-SETOR        PIC X(04).                                00023904
          05 WRK-SALARIO      PIC 999999999.                            00024004
          05 WRK-DATAADM      PIC X(10).                                00024104
          05 WRK-EMAIL        PIC X(40).                                00024204
                                                                        00024300
       77 WRK-REGLIDOS        PIC 9(03).                                00024400
       77 WRK-SALARIO-ACUM    PIC S9(8)V9(2) COMP.                      00024500
       77 WRK-MAIOR-SALARIO   PIC S9(8)V9(2) COMP.                      00024600
       77 WRJ-TEMP-SALARIO    PIC S9(8)V9(2) COMP.                      00024700
       77 WRK-MEDIA           PIC S9(8)V9(2) COMP.                      00024800
       77 WRK-SQLCODE         PIC -999.                                 00025000
       77 WRK-NULL-EMAIL      PIC S9(4) COMP.                           00025100
       77 WRK-FS-RELDB2       PIC 9(02).                                00025200
                                                                        00025300
      *=============================================================*   00025400
       PROCEDURE DIVISION.                                              00025500
      *=============================================================*   00025600
                                                                        00025700
      *-------------------------------------------------------------*   00025800
       0000-PRINCIPAL                           SECTION.                00025900
      *-------------------------------------------------------------*   00026000
            PERFORM  1000-INICIAR.                                      00026100
            PERFORM  2000-PROCESSAR UNTIL SQLCODE EQUAL 100.            00026200
            PERFORM  3000-FINALIZAR.                                    00026300
            STOP RUN.                                                   00026400
      *-------------------------------------------------------------*   00026700
       1000-INICIAR                             SECTION.                00026800
      *-------------------------------------------------------------*   00026900
            EXEC SQL                                                    00027000
               OPEN CFUNC                                               00027100
            END-EXEC.                                                   00027200
             EVALUATE SQLCODE                                           00027300
              WHEN 0                                                    00027400
                PERFORM 2100-LER-FUNC                                   00027500
              WHEN 100                                                  00027600
                DISPLAY 'SEM FUNCIONARIO'                               00027700
              WHEN OTHER                                                00027800
                MOVE SQLCODE TO WRK-SQLCODE                             00027900
                DISPLAY 'ERRO ' WRK-SQLCODE ' NO OPEN DO CURSOR.'       00028000
                STOP RUN                                                00029100
             END-EVALUATE.                                              00029200
                                                                        00029300
            OPEN OUTPUT RELDB2.                                         00029400
                                                                        00029500
            IF WRK-FS-RELDB2 NOT EQUAL 0                                00029600
               DISPLAY ' ERRO NO OPEN DO ARQUIVO'                       00029700
                  STOP RUN                                              00029800
            END-IF.                                                     00029900
      *-------------------------------------------------------------*   00030000
       1000-99-FIM.              EXIT.                                  00030100
      *-------------------------------------------------------------*   00030200
                                                                        00030300
      *-------------------------------------------------------------*   00030400
       2000-PROCESSAR                           SECTION.                00030500
      *-------------------------------------------------------------*   00030600
             IF WRK-NULL-EMAIL = 0                                      00034100
              MOVE DB2-EMAIL   TO WRK-EMAIL                             00034205
             ELSE                                                       00034400
              MOVE '--SEM EMAIL ' TO WRK-EMAIL                          00034505
             END-IF                                                     00034700
              PERFORM 2100-LER-FUNC.                                    00034800
                                                                        00034902
             MOVE DB2-ID      TO WRK-ID.                                00035003
             MOVE DB2-NOME    TO WRK-NOME.                              00035103
             MOVE DB2-SETOR   TO WRK-SETOR.                             00035203
             MOVE DB2-SALARIO TO WRK-SALARIO.                           00035303
             MOVE DB2-DATAADM TO WRK-DATAADM.                           00035403
             WRITE FD-RELDB2 FROM WRK-RELDB2.                           00035603
                                                                        00036100
      *-------------------------------------------------------------*   00036200
       2000-99-FIM.              EXIT.                                  00036300
      *-------------------------------------------------------------*   00036400
      *-------------------------------------------------------------*   00036500
       2100-LER-FUNC                                SECTION.            00036600
      *-------------------------------------------------------------*   00037000
           EXEC SQL                                                     00038000
            FETCH CFUNC                                                 00039000
             INTO :DB2-ID,                                              00040000
                  :DB2-NOME,                                            00041000
                  :DB2-SETOR,                                           00041100
                  :DB2-SALARIO,                                         00041200
                  :DB2-DATAADM,                                         00041300
                  :DB2-EMAIL     :WRK-NULL-EMAIL                        00041400
            END-EXEC.                                                   00041500
            EVALUATE SQLCODE                                            00041600
             WHEN 0                                                     00041700
               ADD 1 TO WRK-REGLIDOS                                    00041800
                ADD DB2-SALARIO TO WRK-SALARIO-ACUM                     00041900
               CONTINUE                                                 00042000
             WHEN 100                                                   00042100
              DISPLAY ' FINAL DE ARQUIVO'                               00042200
             WHEN OTHER                                                 00042300
               MOVE SQLCODE TO WRK-SQLCODE                              00042400
               DISPLAY 'ERRO NA LEITURA ' WRK-SQLCODE                   00042500
             END-EVALUATE.                                              00042600
                                                                        00042700
      *-----------------CALCULO MAIOR SALARIO------------------------   00042800
            MOVE DB2-SALARIO TO WRK-MAIOR-SALARIO.                      00042901
                                                                        00043000
            IF WRJ-TEMP-SALARIO EQUAL 0                                 00043100
              MOVE WRK-MAIOR-SALARIO TO WRJ-TEMP-SALARIO                00043200
            ELSE                                                        00043300
              IF WRK-MAIOR-SALARIO GREATER WRJ-TEMP-SALARIO             00043400
                MOVE WRK-MAIOR-SALARIO TO WRJ-TEMP-SALARIO              00043500
            END-IF.                                                     00043600
      *-------------------------------------------------------------*   00044300
       2100-99-FIM.              EXIT.                                  00044400
      *-------------------------------------------------------------*   00044500
      *-------------------------------------------------------------*   00044600
       3000-FINALIZAR                               SECTION.            00044700
      *-------------------------------------------------------------*   00044800
              MOVE WRJ-TEMP-SALARIO TO WRK-MAIOR-SALARIO                00044900
                                                                        00045000
              EXEC SQL                                                  00045100
                CLOSE CFUNC                                             00045200
              END-EXEC.                                                 00045300
              CLOSE RELDB2.                                             00045400
              DISPLAY '   '                                             00045500
              DISPLAY 'REGISTROS LIDOS.......' WRK-REGLIDOS.            00045600
              DISPLAY 'MAIOR SALARIO.........' WRK-MAIOR-SALARIO.       00045700
              DISPLAY 'SALARIO ACUMULADO.....' WRK-SALARIO-ACUM.        00046000
              DIVIDE WRK-SALARIO-ACUM BY WRK-REGLIDOS                   00046100
                    GIVING WRK-MEDIA.                                   00046200
              DISPLAY 'MEDIA DOS SALARIOS....' WRK-MEDIA.               00046300
              DISPLAY '-----FIM DO PROGRAMA----- '.                     00046400
      *-------------------------------------------------------------*   00046500
       3000-99-FIM.              EXIT.                                  00046600
      *-------------------------------------------------------------*   00046700
                                                                        00046800
      *-------------------------------------------------------------*   00046900
       9000-TRATAERROS                              SECTION.            00047000
      *-------------------------------------------------------------*   00047100
                                                                        00047200
      *-------------------------------------------------------------*   00047300
       9000-99-FIM.              EXIT.                                  00047400
      *-------------------------------------------------------------*   00047500
