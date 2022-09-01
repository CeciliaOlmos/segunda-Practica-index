      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLI ASSIGN TO "..\cli.txt"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT CLIENVIP ASSIGN TO "..\clienvip.dat"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS cli-llave
                   ALTERNATE RECORD KEY IS cli-vend WITH DUPLICATES.
       DATA DIVISION.
       FILE SECTION.
       FD  CLIENVIP.
       01  cli-reg.
           03 cli-llave.
               05 cli-codcli pic 9(6).
               05 cli-ganancia pic 9(10).
           03 cli-nombre pic x(30).
           03 cli-vend pic 9(3).
       FD  CLI.
       01  cli-tex-reg.
           03 cli-tex-codcli pic 9(6).
           03 cli-tex-ganancia pic 9(10).
           03 cli-tex-nombre pic x(30).
           03 cli-tex-vend pic 9(3).
       WORKING-STORAGE SECTION.
       77  w-flag-cli pic 9.
           88 fin-archivo value 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO.
           PERFORM 200-LEER-ARCH-FACT.
           PERFORM UNTIL fin-archivo
            PERFORM 300-PROCESO
            PERFORM 200-LEER-ARCH-FACT
           END-PERFORM.
           PERFORM 400-FIN.
            STOP RUN.
       100-INICIO.
           OPEN INPUT CLI.
           OPEN OUTPUT CLIENVIP.
       200-LEER-ARCH-FACT.
           READ CLI AT END MOVE 1 TO w-flag-cli.
       300-PROCESO.
           MOVE cli-tex-codcli TO cli-codcli.
           MOVE cli-tex-ganancia TO cli-ganancia.
           MOVE cli-tex-nombre TO cli-nombre.
           MOVE cli-tex-vend TO cli-vend.
           WRITE cli-reg.
           DISPLAY cli-reg.
       400-FIN.
           CLOSE CLI.
           CLOSE CLIENVIP.
       END PROGRAM YOUR-PROGRAM-NAME.
