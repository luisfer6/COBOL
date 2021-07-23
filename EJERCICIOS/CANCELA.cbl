       PROGRAM-ID.   CANCELA.
      *AUTHOR. EDUARDO PALMEYRO
      *INSTALLATION. EDUSAM.
      *DATE-COMPILED.
      *DATE WRITTEN. XX/XX/XX.
      *-------------------------------------------------------------*
      * RUTINA DE CANCELACION  GENERA DIVISION POR CERO
      *-------------------------------------------------------------*

       ENVIRONMENT DIVISION.
      *-------------------------------------------------------------*
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       DATA DIVISION.
      *-------------------------------------------------------------*

       WORKING-STORAGE SECTION.
      *-------------------------------------------------------------*
       77  WS-CN-1                              PIC 9     VALUE 1.
       77  WS-CN-0                              PIC 9     VALUE 0.
       77  WS-CICLO                             PIC X     VALUE ' '.
           88 88-CICLO-INICIAL                            VALUE ' '.
           88 88-CICLO-CONTINUACION                       VALUE '1'.
       77  MSG                                  PIC X(50) VALUE ' '.
      *-------------------------------------------------------------*
       LINKAGE SECTION.
      *-------------------------------------------------------------*
      * COPY DE AREA DE COMUNICACION CON ESTA RUTINA

       COPY WCANCELA.

      *-------------------------------------------------------------*
       PROCEDURE DIVISION USING WCANCELA.
      *-------------------------------------------------------------*

       0000-CUERPO-PRINCIPAL SECTION.
      *-----------------------------

           EVALUATE WCANCELA-CODRET (1:2)
                WHEN '00' MOVE 'SUCCESS '            TO MSG
                WHEN '02' MOVE 'SUCCESS DUPLICATE '  TO MSG
                WHEN '04' MOVE 'SUCCESS INCOMPLETE ' TO MSG
                WHEN '05' MOVE 'SUCCESS OPTIONAL '   TO MSG
                WHEN '07' MOVE 'SUCCESS NO UNIT '    TO MSG
                WHEN '10' MOVE 'END OF FILE '        TO MSG
                WHEN '14' MOVE 'OUT OF KEY RANGE '   TO MSG
                WHEN '21' MOVE 'KEY INVALID '        TO MSG
                WHEN '22' MOVE 'KEY EXISTS '         TO MSG
                WHEN '23' MOVE 'KEY NOT EXISTS '     TO MSG
                WHEN '30' MOVE 'PERMANENT ERROR '    TO MSG
                WHEN '31' MOVE 'INCONSISTENT FILENAME ' TO MSG
                WHEN '34' MOVE 'BOUNDARY VIOLATION ' TO MSG
                WHEN '35' MOVE 'FILE NOT FOUND '     TO MSG
                WHEN '37' MOVE 'PERMISSION DENIED '  TO MSG
                WHEN '38' MOVE 'CLOSED WITH LOCK '  TO MSG
                WHEN '39' MOVE 'CONFLICT ATTRIBUTE ' TO MSG
                WHEN '41' MOVE 'ALREADY OPEN '      TO MSG
                WHEN '42' MOVE 'NOT OPEN '          TO MSG
                WHEN '43' MOVE 'READ NOT DONE '     TO MSG
                WHEN '44' MOVE 'RECORD OVERFLOW '   TO MSG
                WHEN '46' MOVE 'READ ERROR '        TO MSG
                WHEN '47' MOVE 'INPUT DENIED '      TO MSG
                WHEN '48' MOVE 'OUTPUT DENIED '     TO MSG
                WHEN '49' MOVE 'I/O DENIED '        TO MSG
                WHEN '51' MOVE 'RECORD LOCKED '     TO MSG
                WHEN '52' MOVE 'END-OF-PAGE '       TO MSG
                WHEN '57' MOVE 'I/O LINAGE '        TO MSG
                WHEN '61' MOVE 'FILE SHARING FAILURE ' TO MSG
                WHEN '91' MOVE 'FILE NOT AVAILABLE ' TO MSG
           END-EVALUATE.


           DISPLAY ' '.
           DISPLAY '************************************************'.
           DISPLAY '*****   RUTINA DE CANCELACION PROGRAMADA   *****'.
           DISPLAY '************************************************'.
           DISPLAY '*                                               '.
           DISPLAY '* PROGRAMA     : ' WCANCELA-PROGRAMA.
           DISPLAY '* PARRAFO      : ' WCANCELA-PARRAFO.
           DISPLAY '* RECURSO      : ' WCANCELA-RECURSO.
           DISPLAY '* OPERACION    : ' WCANCELA-OPERACION.
           DISPLAY '* COD RETORNO  : ' WCANCELA-CODRET.
           DISPLAY '* MENSAJE      : ' WCANCELA-MENSAJE.
           DISPLAY '* MENSAJE-2    : ' MSG.
           DISPLAY '*                                               '.
           DISPLAY '************************************************'.
           DISPLAY '*           SE CANCELA EL PROCESO              *'.
           DISPLAY '************************************************'.

           GOBACK.
