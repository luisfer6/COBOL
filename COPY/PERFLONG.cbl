      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LONFLUJO.
      * Demuestra el uso de PERFORM..UNTIL.
      * El PERFORM..UNTIL se utiliza con mayor frecuencia para procesar
      * un flujo de datos donde la longitud del flujo no se puede
      * determinar de antemano.
      * Preste especial atención a la forma en que se
      * procesa el flujo de números * en este programa.
      * Observe cómo el ERROR DE TAMAÑO DE ENCENDIDO se puede usar para
      * detectar cuando el resultado de un cálculo es demasiado grande
      * para el elemento datos que se pretende que lo contenga.
      * El verbo INICIALIZAR establece un elemento de datos en su valor
      * inicial o inicial.
      * RECTO es recuento abreviado.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ITEM-RECUENTO             PIC 99  VALUE 0.
           88 MAX-RECTO-ALCANCE              VALUE 0.
       01  ENTRADA-USUARIO           PIC 99  VALUE 0.
           88 FINAL-USUARIO-ENTRADA          VALUE 0.
       01  ARRANQUE-TOTAL            PIC 999 VALUE 0.
       01  VALOR-PROMEDIO            PIC 99  VALUES 0.

       PROCEDURE DIVISION.
       BEGIN.
           PERFORM UNTIL ITEM-RECUENTO = 5
              DISPLAY 'ITEM RECUENTO = ' ITEM-RECUENTO
              ADD 1 TO ITEM-RECUENTO
           END-PERFORM
           DISPLAY 'FINALIZAR EN LA LINEA PERFORM.'

           INITIALIZE ITEM-RECUENTO

           DISPLAY 'Ingrese una secuencia de hasta 99 numeros'
           DISPLAY 'Cada numero debe estar en el rango 1 a 99.Ingrese 0'
                   'para detenerse'
           DISPLAY 'Ingresar numero: -' WITH NO ADVANCING
           ACCEPT ENTRADA-USUARIO
           PERFORM OBTENER-ENTRADA-USER  UNTIL FINAL-USUARIO-ENTRADA
                                         OR MAX-RECTO-ALCANCE

           DISPLAY 'El total final es -' ARRANQUE-TOTAL
           DISPLAY 'El recuento final es -' ITEM-RECUENTO
           COMPUTE VALOR-PROMEDIO = ARRANQUE-TOTAL / ITEM-RECUENTO
           DISPLAY 'El valor promedio introducido es -' VALOR-PROMEDIO
           STOP RUN.

           OBTERNER-ENTRADA-USER.
           ADD ENTRADA-USUARIO TO ARRANQUE-TOTAL
               ON SIZE ERROR DISPLAY 'ERROR: nuevo total demasiado '
               'grande para el elemento de datos'
               NOT ON SIZE ERROR ADD 1 TO ITEM-RECUENTO END-ADD
           END-ADD
           DISPLAY 'El total hasta ahora es -' ARRANQUE-TOTAL
           DISPLAY 'El recuento hasta ahora es -' ITEM-RECUENTO
           DISPLAY 'Ingrese el número: -'
           ACCEPT ENTRADA-USUARIO.

           END PROGRAM LONFLUJO.
