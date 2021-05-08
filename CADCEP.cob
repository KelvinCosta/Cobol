       IDENTIFICATION DIVISION.
       PROGRAM-ID. CEPS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CEP-FILE ASSIGN TO DISK
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               FILE STATUS IS FS-STAT
               RECORD KEY IS FS-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD CEP-FILE VALUE OF FILE-ID IS "ceps.dat".
       01 CEP-FILE-REC.
           05 FS-KEY.
               10 FS-CEP PIC 9(08) BLANK WHEN ZEROS.
           05 FS-LOGRADOURO     PIC X(35).
           05 FS-BAIRRO PIC X(20).
           05 FS-CIDADE PIC X(20).
           05 FS-UF PIC X(02).
           05 FS-PONTO-REFERENCIA PIC X(35).
           05 FS-STATUS PIC X(01).
           05 FS-OBSERVACAO PIC X(40).
           05 FILLER      PIC X(20).
       WORKING-STORAGE SECTION.
       01 WS-MODULO.
           05 FILLER PIC X(11) VALUE "CEPS -".
           05 WS-OP PIC X(20) VALUE SPACES.

       77 WS-OPCAO PIC X.
           88 E-INCLUIR   VALUE IS "1".
           88 E-CONSULTAR VALUE IS "2".
           88 E-ALTERAR   VALUE IS "3".
           88 E-EXCLUIR   VALUE IS "4".
           88 E-ENCERRAR  VALUE IS "X" "x".
       77 FS-STAT PIC 9(02).
           88 FS-OK         VALUE ZEROS.
           88 FS-CANCELA    VALUE 99.
           88 FS-NAO-EXISTE VALUE 35.
       77 WS-ERRO PIC X.
           88 E-SIM VALUES ARE "S" "s".

       77 WS-NUML PIC 999.
       77 WS-NUMC PIC 999.
       77 COR-FUNDO PIC 9 VALUE 1.
       77 COR-FRENTE PIC 9 VALUE 6.

       77 WS-STATUS PIC X(30).
       77 WS-MSGERRO PIC X(80).

       COPY screenio.

       SCREEN SECTION.
       01 SS-CLS.
           05 SS-FILLER.
               10 BLANK SCREEN.
               10 LINE 01 COLUMN 01 ERASE EOL
                  BACKGROUND-COLOR COR-FUNDO.
               10 LINE WS-NUML COLUMN 01 ERASE EOL
                  BACKGROUND-COLOR COR-FUNDO.
           05 SS-CABECALHO.
               10 LINE 01 COLUMN 02 PIC X(31) FROM WS-MODULO
                  HIGHLIGHT FOREGROUND-COLOR COR-FRENTE
                  BACKGROUND-COLOR COR-FUNDO.
           05 SS-STATUS.
               10 LINE WS-NUML COLUMN 2 ERASE EOL PIC X(30)
                  FROM WS-STATUS HIGHLIGHT
                  FOREGROUND-COLOR COR-FRENTE
                  BACKGROUND-COLOR COR-FUNDO.
       01 SS-MENU FOREGROUND-COLOR 6.
           05 LINE 07 COLUMN 15 VALUE "1 - INCLUIR".
           05 LINE 08 COLUMN 15 VALUE "2 - CONSULTAR".
           05 LINE 09 COLUMN 15 VALUE "3 - ALTERAR".
           05 LINE 10 COLUMN 15 VALUE "4 - EXCLUIR".
           05 LINE 11 COLUMN 15 VALUE "X - ENCERRAR".
           05 LINE 13 COLUMN 15 VALUE "OPCAO: ".
           05 LINE 13 COL PLUS 1 USING WS-OPCAO AUTO.

       01 SS-TELA-REGISTRO.
           05 SS-CHAVE FOREGROUND-COLOR 2.
               10 LINE 10 COLUMN 10 VALUE "CEP:".
               10 COLUMN PLUS 2 PIC 9(09) USING FS-CEP
                  BLANK WHEN ZEROS.
           05 SS-DADOS.
               10 LINE 11 COLUMN 10 VALUE "LOGRADOURO:".
               10 COLUMN PLUS 2 PIC X(35) USING FS-LOGRADOURO.
               10 LINE 12 COLUMN 10 VALUE "BAIRRO:".
               10 COLUMN PLUS 2 PIC X(20) USING FS-BAIRRO.
               10 LINE 13 COLUMN 10 VALUE "CIDADE:".
               10 COLUMN PLUS 2 PIC X(20) USING FS-CIDADE.
               10 LINE 14 COLUMN 10 VALUE "UF:".
               10 COLUMN PLUS 2 PIC X(02) USING FS-UF.
               10 LINE 15 COLUMN 10 VALUE "PONTO REFERENCIA:".
               10 COLUMN PLUS 2 PIC X(35) USING FS-PONTO-REFERENCIA.
               10 LINE 16 COLUMN 10 VALUE "STATUS:".
               10 COLUMN PLUS 2 PIC X(02) USING FS-STATUS.
               10 LINE 17 COLUMN 10 VALUE "OBSERVACAO:".
               10 COLUMN PLUS 2 PIC X(40) USING FS-OBSERVACAO.

       01 SS-ERRO.
           05 FILLER FOREGROUND-COLOR 4 BACKGROUND-COLOR 1 HIGHLIGHT.
               10 LINE WS-NUML COLUMN 2 PIC X(80) FROM WS-MSGERRO BELL.
               10 COLUMN PLUS 2 TO WS-ERRO.
       PROCEDURE DIVISION.
       INICIO.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
           SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.
           SET ENVIRONMENT 'ESCDELAY' TO '25'.
           ACCEPT WS-NUML FROM LINES
           ACCEPT WS-NUMC FROM COLUMNS
           PERFORM ABRIR-ARQUIVOS
           PERFORM UNTIL E-ENCERRAR
               MOVE "MENU" TO WS-OP
               MOVE "ESCOLHA A OPCAO" TO WS-STATUS
               MOVE SPACES TO WS-OPCAO
               DISPLAY SS-CLS
               ACCEPT SS-MENU
               EVALUATE TRUE
                   WHEN E-INCLUIR
                       PERFORM INCLUI THRU INCLUI-FIM
                   WHEN E-CONSULTAR
                       PERFORM CONSULTA THRU CONSULTA-FIM
                   WHEN E-ALTERAR
                       PERFORM ALTERA THRU ALTERA-FIM
                   WHEN E-EXCLUIR
                       PERFORM EXCLUI THRU EXCLUI-FIM
               END-EVALUATE
           END-PERFORM.
       FINALIZA.
           CLOSE CEP-FILE.
           STOP RUN.

      * -----------------------------------
       INCLUI.
           MOVE "INCLUSAO" TO WS-OP.
           MOVE "ESC PARA ENCERRAR" TO WS-STATUS.
           DISPLAY SS-CLS.
           MOVE SPACES TO CEP-FILE-REC.
       INCLUI-LOOP.
           ACCEPT SS-TELA-REGISTRO.
           IF COB-CRT-STATUS = COB-SCR-ESC
               GO INCLUI-FIM
           END-IF
           IF FS-LOGRADOURO EQUAL SPACES
              MOVE "FAVOR INFORMAR LOGRADOURO" TO WS-MSGERRO
               PERFORM MOSTRA-ERRO
               GO INCLUI-LOOP
           END-IF
           IF FS-BAIRRO EQUAL SPACES
              MOVE "FAVOR INFORMAR BAIRRO" TO WS-MSGERRO
               PERFORM MOSTRA-ERRO
               GO INCLUI-LOOP
           END-IF
           IF FS-CIDADE EQUAL SPACES
              MOVE "FAVOR INFORMAR CIDADE" TO WS-MSGERRO
               PERFORM MOSTRA-ERRO
               GO INCLUI-LOOP
           END-IF
           IF FS-UF EQUAL SPACES
              MOVE "FAVOR INFORMAR UNIDADE FEDERATIVA" TO WS-MSGERRO
               PERFORM MOSTRA-ERRO
               GO INCLUI-LOOP
           END-IF
           IF FS-STATUS EQUAL SPACES
              MOVE "FAVOR INFORMAR STATUS" TO WS-MSGERRO
               PERFORM MOSTRA-ERRO
               GO INCLUI-LOOP
           END-IF
           WRITE CEP-FILE-REC
           INVALID KEY
               MOVE "CEP JÁ EXISTE" TO WS-MSGERRO
               PERFORM MOSTRA-ERRO
               MOVE ZEROS TO FS-KEY
           END-WRITE.
           GO INCLUI.
       INCLUI-FIM.

      * -----------------------------------
       CONSULTA.
           MOVE "CONSULTA" TO WS-OP.
           MOVE "ESC PARA ENCERRAR" TO WS-STATUS.
           DISPLAY SS-CLS.
       CONSULTA-LOOP.
           MOVE SPACES TO CEP-FILE-REC.
           DISPLAY SS-TELA-REGISTRO.
           PERFORM LE-CEP THRU LE-CEP-FIM.
           IF FS-CANCELA
               GO CONSULTA-FIM
           END-IF
           IF FS-OK
               DISPLAY SS-DADOS
               MOVE "PRESSIONE ENTER" TO WS-MSGERRO
               PERFORM MOSTRA-ERRO
           END-IF.
           GO CONSULTA-LOOP.
       CONSULTA-FIM.

      * -----------------------------------
       ALTERA.
           MOVE "ALTERACAO" TO WS-OP.
           MOVE "ESC PARA ENCERRAR" TO WS-STATUS.
           DISPLAY SS-CLS.
       ALTERA-LOOP.
           MOVE SPACES TO CEP-FILE-REC.
           DISPLAY SS-TELA-REGISTRO.
           PERFORM LE-CEP THRU LE-CEP-FIM.
           IF FS-CANCELA
               GO TO ALTERA-FIM
           END-IF
           IF FS-OK
               ACCEPT SS-DADOS
               IF COB-CRT-STATUS = COB-SCR-ESC
                   GO ALTERA-LOOP
               END-IF
           ELSE
               GO ALTERA-LOOP
            END-IF
            REWRITE CEP-FILE-REC
                INVALID KEY
                    MOVE "ERRO AO GRAVAR" TO WS-MSGERRO
                    PERFORM MOSTRA-ERRO
                NOT INVALID KEY
                    CONTINUE
            END-REWRITE.
            GO ALTERA-LOOP.
       ALTERA-FIM.

      * -----------------------------------
       EXCLUI.
           MOVE "EXCLUSAO" TO WS-OP.
           MOVE "ESC PARA ENCERRAR" TO WS-STATUS.
           DISPLAY SS-CLS.
           MOVE SPACES TO CEP-FILE-REC.
           DISPLAY SS-TELA-REGISTRO.
           PERFORM LE-CEP THRU LE-CEP-FIM.
           IF FS-CANCELA
               GO EXCLUI-FIM
           END-IF
           IF NOT FS-OK
               GO EXCLUI
           END-IF
           DISPLAY SS-DADOS.
           MOVE "N" TO WS-ERRO.
           MOVE "CONFIRMA A EXCLUSAO (S/N)?" TO WS-MSGERRO.
           ACCEPT SS-ERRO.
           IF NOT E-SIM
               GO EXCLUI-FIM
           END-IF
           DELETE CEP-FILE
               INVALID KEY
                   MOVE "ERRO AO EXCLUIR" TO WS-MSGERRO
                   PERFORM MOSTRA-ERRO
           END-DELETE.
       EXCLUI-FIM.

      * -----------------------------------
      * LE CLIENTE E MOSTRA MENSAGEM SE CHAVE NAO EXISTE
       LE-CEP.
           ACCEPT SS-CHAVE.
           IF NOT COB-CRT-STATUS = COB-SCR-ESC
               READ CEP-FILE
                   INVALID KEY
                       MOVE "CLIENTE NAO ENCONTRADO" TO WS-MSGERRO
                       PERFORM MOSTRA-ERRO
               END-READ
           ELSE
               MOVE 99 to FS-STAT
           END-IF.
       LE-CEP-FIM.

      * -----------------------------------
      * ABRE ARQUIVOS PARA ENTRADA E SAÍDA
       ABRIR-ARQUIVOS.
           OPEN I-O CEP-FILE
           IF FS-NAO-EXISTE THEN
               OPEN OUTPUT CEP-FILE
               CLOSE CEP-FILE
               OPEN I-O CEP-FILE
           END-IF.

      * -----------------------------------
      * MOSTRA MENSAGEM, ESPERA ENTER, ATUALIZA BARRA STATUS
       MOSTRA-ERRO.
           DISPLAY SS-ERRO
           ACCEPT SS-ERRO
           DISPLAY SS-STATUS.
