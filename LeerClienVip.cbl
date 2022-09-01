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
       WORKING-STORAGE SECTION.
        77  sen pic 9 value 0.
       01  lin-cabecera.
           03 filler pic x(15) value "CODIGO CLIENTE:".
           03 filler pic x(2) value spaces.
           03 filler pic x(9) value "GANANCIA:".
           03 filler pic x(20) value spaces.
           03 filler pic x(10) value "NOMBRE:".
           03 filler pic x(1) value spaces.
           03 filler pic x(9) value "VENDEDOR:".
           03 filler pic x(4) value spaces.
       01  lin-guarda.
           03 filler pic x(80) value all "-".
       01  lin-detalle.
           03 l-cod pic zzzzzz value spaces.
           03 filler pic x(5) value spaces.
           03 l-gan pic z.zzz.zzz.zz9 value spaces.
           03 l-nom pic x(30) value spaces.
           03 filler pic x(5) value spaces.
           03 l-vend pic zzz.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO-LECTURA.
           PERFORM 200-LEE-ARCH-SOCIOS.
           PERFORM UNTIL sen is equal 1
               PERFORM 300-PROCESO-LECTURA
               PERFORM 200-LEE-ARCH-SOCIOS
           END-PERFORM.
           PERFORM 400-FIN-LECTURA.
            STOP RUN.
        100-INICIO-LECTURA.
           PERFORM 130-ABRIR-ARCHIVOS.
           PERFORM 150-LISTAR-ENCABEZADO.

       130-ABRIR-ARCHIVOS.
           OPEN INPUT CLIENVIP.

       150-LISTAR-ENCABEZADO.
           DISPLAY lin-guarda.
           DISPLAY lin-cabecera.
           DISPLAY lin-guarda.

       200-LEE-ARCH-SOCIOS.
           READ CLIENVIP at end move 1 to sen.

       300-PROCESO-LECTURA.
           MOVE cli-codcli TO l-cod.
           MOVE cli-ganancia TO l-gan
           MOVE cli-nombre TO l-nom
           MOVE cli-vend TO l-vend
           DISPLAY lin-detalle.

       400-FIN-LECTURA.
           CLOSE CLIENVIP.
       END PROGRAM YOUR-PROGRAM-NAME.
