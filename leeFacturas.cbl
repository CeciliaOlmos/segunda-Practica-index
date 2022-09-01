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
           SELECT FACTURAS ASSIGN TO "..\facturas.dat"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS SEQUENTIAL
                   RECORD KEY IS fac-llave.
       DATA DIVISION.
       FILE SECTION.
       FD  FACTURAS.
       01  fac-reg.
           03 fac-llave.
              05 fac-facturas pic 9(12).
              05 fac-art pic x(8).
           03 fac-precio pic 9(10).
           03 fac-cantid pic 9(4).
       WORKING-STORAGE SECTION.
       77  sen pic 9 value 0.
       01  lin-cabecera.
           03 filler pic x(12) value "NRO FACTURA:".
           03 filler pic x(2) value spaces.
           03 filler pic x(9) value "ARTICULO:".
           03 filler pic x(4) value spaces.
           03 filler pic x(7) value "PRECIO:".
           03 filler pic x(4) value spaces.
           03 filler pic x(9) value "CANTIDAD:".
           03 filler pic x(4) value spaces.
       01  lin-guarda.
           03 filler pic x(80) value all "-".
       01  lin-detalle.
           03 l-factura pic zzzzzzzzzzzz value spaces.
           03 filler pic x(5) value spaces.
           03 l-art pic x(8) value spaces.
           03 l-precio pic z.zzz.zzz.zz9 value spaces.
           03 l-cant pic zzz.

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
           OPEN INPUT FACTURAS.

       150-LISTAR-ENCABEZADO.
           DISPLAY lin-guarda.
           DISPLAY lin-cabecera.
           DISPLAY lin-guarda.

       200-LEE-ARCH-SOCIOS.
           READ FACTURAS at end move 1 to sen.

       300-PROCESO-LECTURA.
           MOVE fac-facturas to l-factura
           MOVE fac-art to l-art
           MOVE fac-precio to l-precio
           MOVE fac-cantid to l-cant
           DISPLAY lin-detalle.

       400-FIN-LECTURA.
           CLOSE FACTURAS.

       END PROGRAM YOUR-PROGRAM-NAME.
