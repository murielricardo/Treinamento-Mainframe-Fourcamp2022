      *===============================================================* 00001000
       IDENTIFICATION                                        DIVISION.  00002000
      *===============================================================* 00003000
                                                                        00004000
       PROGRAM-ID.  FR25EX03.                                           00005000
                                                                        00006000
      *===============================================================* 00007000
      *     PROGRAMA....: FR25EX03.                                   * 00008000
      *     PROGRAMADOR.: MURIEL ANDRIGUETO                           * 00009000
      *     EMPRESA.....: FOURSYS                                     * 00010000
      *     ANALISTA....: IVAN SANCHES                                * 00010100
      *     DATA........: 31/05/2022                                  * 00010200
      *---------------------------------------------------------------* 00010300
      *     OBJETIVO....: O PROGRAMA TEM A FINALIDADE DE RECEBER      * 00010400
      *                   DADOS DOS ARQUIVOS "CLIENTES" E "MOV3105"   * 00010500
      *                   RELACIONA-LOS POR MEIO DE UM BALANCE LINE E * 00010600
      *                   GRAVAR OS DADOS NO ARQUIVO "CLI3105".       * 00010700
      *---------------------------------------------------------------* 00011200
      *     ARQUIVOS....:                            INCLUDE/BOOK     * 00011300
      *      DDNAME               I/O                                 * 00011400
      *      CLIENTES              I                                  * 00011500
      *      MOV3105               I                                  * 00011600
      *      CLI3105               0                                  * 00011700
      *---------------------------------------------------------------* 00011900
      *     MODULOS.....:                                             * 00012000
      *     GRAVALOG - TRATAMENTO DE ERROS              #GLOG         * 00013000
      *---------------------------------------------------------------* 00014000
                                                                        00014100
      *===============================================================* 00014200
       ENVIRONMENT                                           DIVISION.  00014300
      *===============================================================* 00014400
      *---------------------------------------------------------------* 00014500
       CONFIGURATION                                          SECTION.  00014600
      *---------------------------------------------------------------* 00014700
       SPECIAL-NAMES.                                                   00014800
           DECIMAL-POINT IS COMMA.                                      00014900
                                                                        00015000
      *---------------------------------------------------------------* 00016000
       INPUT-OUTPUT                                           SECTION.  00016100
      *---------------------------------------------------------------* 00016200
       FILE-CONTROL.                                                    00016300
           SELECT CLIENTES  ASSIGN TO CLIENTES                          00016400
               FILE STATUS  IS WRK-FS-CLIENTES.                         00016500
                                                                        00016600
           SELECT MOV3105   ASSIGN TO MOV3105                           00016700
               FILE STATUS  IS WRK-FS-MOV3105.                          00016800
                                                                        00016900
           SELECT CLI3105   ASSIGN TO CLI3105                           00017000
               FILE STATUS  IS WRK-FS-CLI3105.                          00017100
                                                                        00017200
      *===============================================================* 00017500
       DATA                                                  DIVISION.  00017600
      *===============================================================* 00017700
       FILE                                                   SECTION.  00017800
      *-------------------------LRECL 46------------------------------* 00017900
       FD CLIENTES                                                      00018000
           RECORDING MODE IS F                                          00018100
           LABEL RECORD IS STANDARD                                     00018200
           BLOCK CONTAINS 0 RECORDS.                                    00018300
                                                                        00018400
        01 FD-CLIENTES.                                                 00018500
           05 FD-CLIENTES-CHAVE.                                        00018600
             10 FD-CLIENTES-AGENCIA PIC 9(04).                          00018700
             10 FD-CLIENTES-CONTA  PIC 9(04).                           00018800
           05 FD-CLIENTES-NOME     PIC X(30).                           00018900
           05 FD-CLIENTES-SALDO    PIC 9(08).                           00019000
                                                                        00019100
      *-------------------------LRECL 47------------------------------* 00019200
                                                                        00019300
       FD MOV3105                                                       00019400
           RECORDING MODE IS F                                          00019500
           LABEL RECORD IS STANDARD                                     00019600
           BLOCK CONTAINS 0 RECORDS.                                    00019700
                                                                        00019800
        01 FD-MOV3105.                                                  00019900
           05 FD-MOV3105-CHAVE.                                         00020000
             10 FD-MOV3105-AGENCIA PIC 9(04).                           00020100
             10 FD-MOV3105-CONTA   PIC 9(04).                           00020200
           05 FD-MOV3105-MOVIMENTO PIC X(30).                           00020300
           05 FD-MOV3105-VALORMOV  PIC 9(08).                           00020400
           05 FD-MOV3105-TIPOMOV   PIC X(01).                           00020500
                                                                        00020600
       FD CLI3105                                                       00020700
           RECORDING MODE IS F                                          00020800
           BLOCK CONTAINS 0 RECORDS.                                    00020900
       01 FD-CLI3105 PIC X(46).                                         00021000
                                                                        00022500
      *---------------------------------------------------------------* 00022600
       WORKING-STORAGE                                        SECTION.  00022700
      *---------------------------------------------------------------* 00022800
                                                                        00022900
      *---------------------------LRECL 46----------------------------* 00023000
      *---------------------------------------------------------------* 00023100
       01 FILLER PIC X(50) VALUE                                        00023200
              '----------------WRK-CLI3105---------------------'.       00023300
      *---------------------------------------------------------------* 00023400
       01 WRK-CLI3105.                                                  00023500
         05 WRK-CLI3105-CHAVE.                                          00023600
          10 WRK-CLI3105-AGENCIA PIC 9(04) VALUE ZEROS.                 00023700
          10 WRK-CLI3105-CONTA    PIC 9(04) VALUE ZEROS.                00023800
         05 WRK-CLI3105-NOME      PIC X(30) VALUE SPACES.               00023900
         05 WRK-CLI3105-SALDO     PIC 9(08) VALUE ZEROS.                00024000
                                                                        00024100
      *---------------------------LRECL 46----------------------------* 00024200
      *---------------------------------------------------------------* 00025400
       01 FILLER PIC X(50) VALUE                                        00025500
              '------------------#GLOG--------------------------'.      00025600
      *---------------------------------------------------------------* 00025700
      *---------------------------------------------------------------* 00025800
      * PROGRAMA 08            LRECL 48                               * 00025900
      * SECAO    04                                                   * 00026000
      * MENSAGEM 30                                                   * 00026100
      * STATUS   02                                                   * 00026200
      *---------------------------------------------------------------* 00026300
                                                                        00026400
          COPY '#GLOG'.                                                 00026500
                                                                        00026600
      *---------------------------------------------------------------* 00026700
       01 FILLER PIC X(50) VALUE                                        00026800
              '----------VARIAVEIS DE FILE STATUS---------------'.      00026900
      *---------------------------------------------------------------* 00027000
       77 WRK-FS-CLIENTES  PIC 9(02).                                   00027200
       77 WRK-FS-MOV3105   PIC 9(02).                                   00027300
       77 WRK-FS-CLI3105   PIC 9(02).                                   00027400
                                                                        00027500
      *---------------------------------------------------------------* 00027600
       01 FILLER PIC X(50) VALUE                                        00027700
              '-------VARIAVEIS PARA ACUMULACAO-----------------'.      00027800
      *---------------------------------------------------------------* 00027900
       77 WRK-REGLIDOS       PIC 9(02) VALUE ZEROS.                     00028000
       77 WRK-REGGRAV        PIC 9(02) VALUE ZEROS.                     00028100
       77 WRK-REG            PIC 9(02) VALUE ZEROS.                     00028200
                                                                        00028300
      *---------------------------------------------------------------* 00028400
       01 FILLER PIC X(50) VALUE                                        00028500
              '-------VARIAVEIS PARA EXIBICAO-------------------'.      00028600
      *---------------------------------------------------------------* 00028700
       77 WRK-GRAVALOG       PIC X(08) VALUE 'GRAVALOG'.                00028800
                                                                        00028900
      *---------------------------------------------------------------* 00029000
       01 FILLER PIC X(50) VALUE                                        00029100
              '------------VARIAVEIS PARA MENSAGEM--------------'.      00029200
      *---------------------------------------------------------------* 00029300
       77 WRK-MSG           PIC X(30) VALUE SPACES.                     00029400
       77 WRK-CLIENTESVAZIO PIC X(35) VALUE                             00029500
                               ' ERRO CLIENTES - ARQUIVO VAZIO! '.      00029600
       77 WRK-MOV3105VAZIO  PIC X(35) VALUE                             00029700
                               ' ERRO MOV3105  - ARQUIVO VAZIO! '.      00029800
       77 WRK-MSGFINALARQ   PIC X(16) VALUE ' FIM DE ARQUIVO '.         00029900
       77 WRK-MSGERRO       PIC X(05) VALUE ' ERRO'.                    00030000
       77 WRK-MSGREGLIDOS   PIC X(20) VALUE ' REGISTRO LIDOS:    '.     00031000
       77 WRK-MSGREGGRAV    PIC X(20) VALUE ' REGISTRO GRAVADOS: '.     00031100
       77 WRK-MSGIDPROG     PIC X(08) VALUE 'FR25EX03'.                 00031200
       77 WRK-MSGPART       PIC X(04) VALUE '1000'.                     00031300
       77 WRK-ERROCLIENTES  PIC X(25) VALUE 'ERRO OPEN CLIENTES - '.    00031400
       77 WRK-ERROMOV3105   PIC X(25) VALUE 'ERRO OPEN MOV3105  - '.    00031500
       77 WRK-ERROCLI3105   PIC X(25) VALUE 'ERRO OPEN CLI3105 - '.     00031600
                                                                        00031800
      *===============================================================* 00031900
       PROCEDURE                                           DIVISION.    00032000
      *===============================================================* 00032100
      *---------------------------------------------------------------* 00032200
       0000-PRINCIPAL                                         SECTION.  00032300
      *---------------------------------------------------------------* 00032400
             PERFORM 1000-INICIAR.                                      00032500
              PERFORM 1200-VERIFICAR-VAZIO.                             00032600
             PERFORM 2000-PROCESSAR UNTIL WRK-FS-CLIENTES EQUAL 10 AND  00032700
                                          WRK-FS-MOV3105 EQUAL 10.      00032800
             PERFORM 3000-FINALIZAR.                                    00032900
             STOP RUN.                                                  00033000
      *---------------------------------------------------------------* 00033100
       0000-99-FIM.                                      EXIT.          00033200
      *---------------------------------------------------------------* 00033300
      *---------------------------------------------------------------* 00033400
       1000-INICIAR                                           SECTION.  00033500
      *---------------------------------------------------------------* 00033600
                 OPEN INPUT MOV3105 CLIENTES                            00033700
                     OUTPUT CLI3105.                                    00033800
                 PERFORM 1100-TESTA-STATUS.                             00033900
      *---------------------------------------------------------------* 00034000
       1000-99-FIM.                                      EXIT.          00034100
      *---------------------------------------------------------------* 00034200
      *---------------------------------------------------------------* 00034300
       1100-TESTA-STATUS                                      SECTION.  00034400
      *---------------------------------------------------------------* 00034500
                 IF WRK-FS-CLIENTES NOT EQUAL 0                         00034600
                   MOVE WRK-MSGIDPROG    TO WRK-PROGRAMA                00034700
                   MOVE WRK-MSGPART      TO WRK-SECAO                   00034800
                   MOVE WRK-ERROCLIENTES TO WRK-MENSAGEM                00034900
                   MOVE WRK-FS-CLIENTES  TO WRK-STATUS                  00035000
                 PERFORM 9000-TRATA-ERROS                               00035100
                 END-IF.                                                00035200
                                                                        00035300
                 IF WRK-FS-MOV3105 NOT EQUAL 0                          00035400
                   MOVE WRK-MSGIDPROG    TO WRK-PROGRAMA                00035500
                   MOVE WRK-MSGPART      TO WRK-SECAO                   00035600
                   MOVE WRK-ERROMOV3105  TO WRK-MENSAGEM                00035700
                   MOVE WRK-FS-MOV3105   TO WRK-STATUS                  00035800
                 PERFORM 9000-TRATA-ERROS                               00035900
                 END-IF.                                                00036000
                                                                        00036100
                 IF WRK-FS-CLI3105 NOT EQUAL 0                          00036200
                   MOVE WRK-MSGIDPROG    TO WRK-PROGRAMA                00036300
                   MOVE WRK-MSGPART      TO WRK-SECAO                   00036400
                   MOVE WRK-ERROCLI3105 TO WRK-MENSAGEM                 00036500
                   MOVE WRK-FS-CLI3105   TO WRK-STATUS                  00036600
                 PERFORM 9000-TRATA-ERROS                               00036700
                 END-IF.                                                00036800
      *---------------------------------------------------------------* 00037700
       1100-99-FIM.                                      EXIT.          00037800
      *---------------------------------------------------------------* 00037900
      *---------------------------------------------------------------* 00038000
       1200-VERIFICAR-VAZIO                                   SECTION.  00038100
      *---------------------------------------------------------------* 00038200
                 READ CLIENTES.                                         00038300
                 READ MOV3105.                                          00038400
                  IF WRK-FS-CLIENTES EQUAL 10                           00038500
                    DISPLAY WRK-CLIENTESVAZIO                           00038600
                  END-IF.                                               00038700
                                                                        00038800
                  IF WRK-FS-MOV3105 EQUAL 10                            00038900
                    DISPLAY WRK-MOV3105VAZIO                            00039000
                  END-IF.                                               00039100
      *---------------------------------------------------------------* 00039200
       1200-99-FIM.                                      EXIT.          00039300
      *---------------------------------------------------------------* 00039400
      *---------------------------------------------------------------* 00039500
       2000-PROCESSAR                                         SECTION.  00039600
      *---------------------------------------------------------------* 00039700
           EVALUATE TRUE                                                00040000
              WHEN FD-CLIENTES-CHAVE LESS FD-MOV3105-CHAVE              00040100
                    MOVE FD-CLIENTES-CHAVE TO WRK-CLI3105-CHAVE         00040300
                    MOVE FD-CLIENTES-NOME TO WRK-CLI3105-NOME           00040400
                    MOVE FD-CLIENTES-SALDO TO WRK-CLI3105-SALDO         00040500
                    WRITE FD-CLI3105 FROM WRK-CLI3105                   00040600
                    ADD 1 TO WRK-REGGRAV                                00040700
              READ CLIENTES                                             00041500
                  ADD 1 TO WRK-REGLIDOS                                 00041600
                                                                        00042000
              WHEN FD-CLIENTES-CHAVE EQUAL FD-MOV3105-CHAVE             00042100
                  IF FD-MOV3105-TIPOMOV EQUAL 'D'                       00042200
                    SUBTRACT FD-MOV3105-VALORMOV FROM                   00042500
                             FD-CLIENTES-SALDO                          00042600
                  END-IF                                                00042800
                  IF FD-MOV3105-TIPOMOV EQUAL 'C'                       00043000
                   ADD FD-MOV3105-VALORMOV TO FD-CLIENTES-SALDO         00043100
                  END-IF                                                00043200
              READ MOV3105                                              00043400
                  IF WRK-FS-MOV3105 EQUAL 10                            00043500
                   MOVE HIGH-VALUE TO FD-MOV3105-CHAVE                  00043600
                  END-IF                                                00043700
           END-EVALUATE.                                                00043900
      *---------------------------------------------------------------* 00045000
       2000-99-FIM.                                      EXIT.          00045100
      *---------------------------------------------------------------* 00045200
      *---------------------------------------------------------------* 00045300
       3000-FINALIZAR                                         SECTION.  00045400
      *---------------------------------------------------------------* 00045500
                 DISPLAY WRK-MSGREGLIDOS WRK-REGLIDOS                   00045600
                 DISPLAY WRK-MSGREGGRAV  WRK-REGGRAV                    00045700
                 CLOSE MOV3105 CLIENTES CLI3105.                        00048000
                 MOVE WRK-MSGFINALARQ TO WRK-MSG                        00048100
                 DISPLAY WRK-MSG.                                       00048200
      *---------------------------------------------------------------* 00048300
       3000-99-FIM.                                      EXIT.          00048400
      *---------------------------------------------------------------* 00048500
      *---------------------------------------------------------------* 00048600
       9000-TRATA-ERROS                                       SECTION.  00048700
      *---------------------------------------------------------------* 00048800
                 CALL WRK-GRAVALOG USING WRK-DADOS.                     00048900
                 GOBACK.                                                00049000
      *---------------------------------------------------------------* 00049100
       9000-99-FIM.                                      EXIT.          00049200
      *---------------------------------------------------------------* 00049300
