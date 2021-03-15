       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGR2IT.
      *AUTHOR. ANGELO LOTIERZO FILHO.
      *******************************************************
      * MANUTENCAO DO CADASTRO DE AMIGOS  *
      *******************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADAMIGO ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS APELIDO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADAMIGO
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADAMIGO.DAT". 
       01 REGAMIGO.
          03 APELIDO        PIC X(12).
          03 NOME             PIC X(30).
          03 EMAIL             PIC X(30).
          03 SEXO             PIC X(01).
          03 GENERO        PIC X(01).
          03 TPAMIGO       PIC 9(01).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-CONT          PIC 9(06) VALUE ZEROS.
       01 W-OPCAO       PIC X(01) VALUE SPACES.
       01 W-ACT              PIC 9(02) VALUE ZEROS.
       01 MENS               PIC X(50) VALUE SPACES.
       01 LIMPA               PIC X(50) VALUE SPACES. 
       01 ST-ERRO         PIC X(02) VALUE "00".
       01 W-SEL              PIC 9(01) VALUE ZEROS.
       01 TXTTPAMIGO    PIC X(10) VALUE SPACES.
       01 IND                   PIC 9(02) VALUE ZEROS.
       01 TEXSEXO       PIC X(12) VALUE SPACES.

       01 TABAMIGO.
          03 TBAMIGO    PIC X(10) OCCURS 9 TIMES.

       01 TABGENEROX.
          03 FILLER     PIC X(30) VALUE "THETEROSEXUAL  HHOMESEXUAL".
          03 FILLER     PIC X(45) VALUE 
                           "BBISSESUXUAL   PPANSEXUAL     AA-------------".
           03 FILLER     PIC X(15) VALUE "BB           ".
          03 FILLER     PIC X(15) VALUE "CC           ".
          03 FILLER     PIC X(15) VALUE "DD            ".
          03 FILLER     PIC X(15) VALUE "NNAO DECLARADO".
       01 TABGENERO REDEFINES TABGENEROX.
          03 TBGENERO   PIC X(15) OCCURS 9 TIMES.
       01 TXTGENERO.
          03 TXTGENERO1 PIC X(01) VALUE SPACES.
          03 TXTGENERO2 PIC X(14) VALUE SPACES. 

      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA2.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "                            ÚÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 01  COLUMN 41 
               VALUE  "ÄÄÄÄÄÄÄÄÄÄ¿".
           05  LINE 02  COLUMN 01 
               VALUE  "                            ³ CADASTRO D".
           05  LINE 02  COLUMN 41 
               VALUE  "E AMIGOS  ³".
           05  LINE 03  COLUMN 01 
               VALUE  "                            ÀÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 03  COLUMN 41 
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÙ".
           05  LINE 04  COLUMN 01 
               VALUE  "  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
           05  LINE 05  COLUMN 01 
               VALUE  "  ³ APELIDO :               ³      NOME".
           05  LINE 05  COLUMN 41 
               VALUE  ":".
           05  LINE 06  COLUMN 01 
               VALUE  "  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
           05  LINE 08  COLUMN 01 
               VALUE  "    EMAIL  :".
           05  LINE 11  COLUMN 01 
               VALUE  "    SEXO   :                       GENER".
           05  LINE 11  COLUMN 41 
               VALUE  "O :".
           05  LINE 14  COLUMN 01 
               VALUE  "    TIPO AMIGO :".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TAPELIDO
               LINE 05  COLUMN 15  PIC X(12)
               USING  APELIDO
               HIGHLIGHT.
           05  TNOME
               LINE 05  COLUMN 43  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TEMAIL
               LINE 08  COLUMN 14  PIC X(30)
               USING  EMAIL
               HIGHLIGHT.
           05  TSEXO
               LINE 11  COLUMN 14  PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  TTEXSEXO
               LINE 11  COLUMN 16  PIC X(15)
               USING  TEXSEXO
               HIGHLIGHT.
           05  TGENERO
               LINE 11  COLUMN 45  PIC X(01)
               USING  GENERO
               HIGHLIGHT.
           05  TTXTGENERO2
               LINE 11  COLUMN 47  PIC X(14)
               USING  TXTGENERO2
               HIGHLIGHT.
           05  TTPAMIGO
               LINE 14  COLUMN 18  PIC 9
               USING  TPAMIGO
               HIGHLIGHT.
           05  TTXTTPAMIGO
               LINE 14  COLUMN 20  PIC X(10)
               USING  TXTTPAMIGO
               HIGHLIGHT.
      *
       01  TELATA.
           05  LINE 14  COLUMN 41 VALUE  "1-FACULDADE".
           05  LINE 15  COLUMN 41 VALUE  "2-IGREJA".
           05  LINE 16  COLUMN 41 VALUE  "3-BAIRRO".
           05  LINE 17  COLUMN 41 VALUE  "4-COLEGIO".
           05  LINE 18  COLUMN 41 
               VALUE  "5-BALADA".
           05  LINE 19  COLUMN 41 
               VALUE  "6-FUTEBOL".
           05  LINE 20  COLUMN 41 
               VALUE  "7-VIAGEM".
           05  LINE 21  COLUMN 41 
               VALUE  "8-EX (A)".
           05  LINE 22  COLUMN 41 
               VALUE  "9-NAMORADA(0) EX(A)".
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           MOVE "FACULDADE" TO TBAMIGO(1)
           MOVE "IGREJA   " TO TBAMIGO(2)
           MOVE "BAIRRO"    TO TBAMIGO(3)
           MOVE "COLEGIO"   TO TBAMIGO(4)
           MOVE "BALADA"    TO TBAMIGO(5)
           MOVE "FUTEBOL"   TO TBAMIGO(6)
           MOVE "VIAGEM"    TO TBAMIGO(7)
           MOVE "EX"        TO TBAMIGO(8)
           MOVE "NAMOR.EX"  TO TBAMIGO(9).
       
       R0.
           OPEN I-O CADAMIGO 
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADAMIGO
                 MOVE "*** ARQUIVO CADAMIGO FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 CLOSE CADAMIGO
                 GO TO R0
              ELSE
                 IF ST-ERRO = "95"
                     MOVE "*** ISAM NAO CARREGADO **" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO ROT-FIM
                 ELSE
                     MOVE "ERRO NA ABERTURA DO CADAMIGO" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *                
       R1.
           MOVE SPACES TO APELIDO NOME EMAIL SEXO GENERO
           MOVE SPACES TO TXTGENERO TEXSEXO TXTTPAMIGO
           MOVE ZEROS TO TPAMIGO
           DISPLAY TELA2.
       R2.
           ACCEPT TAPELIDO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           IF W-ACT = 02
                     MOVE "*** TECLEI O F1 **" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R2.
           IF W-ACT = 10
                     MOVE "*** TECLEI O F9 **" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R2.
           IF APELIDO = SPACES
                     MOVE "*** DADO NAO PODE FICAR VAZIO **" TO MENS
                     PERFORM ROT-MENS THRU ROT-MENS-FIM
                     GO TO R2.   
       LER-CADMAMIGO.
           READ CADAMIGO
           IF ST-ERRO NOT = "23"
             IF ST-ERRO = "00"
                PERFORM R5A
                PERFORM R6A
                PERFORM R7A
                DISPLAY TELA2
                MOVE "*** AMIGO JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO R1
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADAMIGO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                NEXT SENTENCE.

       R3.       
           ACCEPT TNOME
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.
 
       R4.
           ACCEPT TEMAIL.  
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
       R5.
           ACCEPT TSEXO.
       R5A.
           IF SEXO = "M" OR SEXO = "m"
              MOVE "MASCULINO" TO TEXSEXO
           ELSE
              IF SEXO = "F"
                 MOVE "FEMENINO" TO TEXSEXO
              ELSE
                 MOVE "*DIGITE M=MASCULINO    F=FEMININO*" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R5.
           DISPLAY TTEXSEXO.
       R6.
           MOVE 1 TO IND
           ACCEPT TGENERO.
       R6A.
           MOVE TBGENERO(IND) TO TXTGENERO
           IF TXTGENERO1 NOT = GENERO
              ADD 1 TO IND
              IF IND < 10
                 GO TO R6A
              ELSE
                 MOVE "*** TIPO GENERO INCORRETO***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R6
           ELSE
               DISPLAY TELA2.

       R7.
           DISPLAY TELATA
           ACCEPT TTPAMIGO.
           IF TPAMIGO = 0 
                 MOVE "*** DIGITE APENAS DE 1 ATE 9 ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R7.
       R7A.
           MOVE TBAMIGO(TPAMIGO) TO TXTTPAMIGO
           DISPLAY TTXTTPAMIGO.
           DISPLAY TELA2.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R7.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO USUARIO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGAMIGO
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "* AMIGO JA EXISTE,DADOS NAO GRAVADOS *" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R1
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE AMIGOS"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

       ROT-FIM.
           CLOSE CADAMIGO.
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (23, 12) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.

      *    FILE STATUS
      *    00 = OPERAÇÃO REALIZADO COM SUCESSO
      *    22 = REGISTRO JÁ CADASTRADO
      *    23 = REGISTRO NÃO ENCONTRADO
      *    30 = ARQUIVO NÃO ENCONTRADO
      *    95 = ISAM NÃO CARREGADO
      

