frant @ DESKTOP-LM8QUAK 2021-06-17 05:17:24

 ****** ***************************** Top of Data ******************************
 000001        CBL TEST
 000002       /------------------------------------------------------/
 000003       * PRE-REQUISITOS: HABER VALIDADO LOS ARCHIVOS DE INPUT *
 000004       *                 ORDENAR ARCHIVO NOVEDADES            *
 000005       /------------------------------------------------------/
 000006        IDENTIFICATION DIVISION.
 000007       /--------------------------------/
 000008         PROGRAM-ID PGMDBL01.
 000009       /--------------------------------/
 000010        ENVIRONMENT DIVISION.
 000011       /--------------------------------/
 000012        CONFIGURATION SECTION.
 000013       /--------------------------------/
 000014        SPECIAL-NAMES.
 000015            DECIMAL-POINT IS COMMA.
 000016       /--------------------------------/
 000017        INPUT-OUTPUT SECTION.
 000018       /--------------------------------/
 000019        FILE-CONTROL.
 000020       /--------------------------------/
 000021            SELECT SALIDA ASSIGN DDSALI
 000022               FILE STATUS IS FS-SALIDA.
 000023
 000024       /--------------------------------/
 000025        DATA DIVISION.
 000026       /--------------------------------/
 000027        FILE SECTION.
 000028       /--------------------------------/
 000029        FD SALIDA
 000030             BLOCK CONTAINS 0 RECORDS
 000031             RECORDING MODE IS F.
 000032        01 REG-SALIDA              PIC X(134).
 000033
 000034       /--------------------------------/
 000035        WORKING-STORAGE SECTION.
 000036       /--------------------------------/
 000037        77  FILLER                 PIC X(26)      VALUE
 000038                                    '* INICIO WORKING-STORAGE *'.
 000039       /-----CODIGOS-RETORNO-FILES------/
 000040        01 WS-CODE.
 000041          02  FS-SQLCODE           PIC -999       VALUE ZEROS.
 000042          02  FS-SALIDA            PIC XX         VALUE SPACES.
 000043
 000044       /--------FLAGS-CONTROL-----------/
 000045        01 WS-FLAG-FIN             PIC X.
 000046          88 WS-FIN                               VALUE '1'.
 000047          88 WS-NO-FIN                            VALUE '0'.
 000048
 000049        01 WS-FLAG-CUE             PIC X.
 000050          88 WS-FIN-CUE                           VALUE '1'.
 000051          88 WS-NO-FIN-CUE                        VALUE '0'.
 000052
 000053        01 WS-FLAG-CLI             PIC X.
 000054          88 WS-FIN-CLI                           VALUE '1'.
 000055          88 WS-NO-FIN-CLI                        VALUE '0'.
 000056
 000057       /--------FECHAS------------------/
 000058        01 WS-FECHA.
 000059          03 WS-FECHA-AAAA         PIC 9999       VALUE ZEROS.
 000060          03 WS-FECHA-MM           PIC 99         VALUE ZEROS.
 000061          03 WS-FECHA-DD           PIC 99         VALUE ZEROS.
 000062
 000063       /------LAYOUT-TITULO-------------/
 000064        01 CT-TITULO.
 000065           03  FILLER              PIC X(1)       VALUE '|'.
 000066           03 FILLER               PIC X(03)      VALUE SPACES.
 000067           03 FILLER               PIC X(39)      VALUE
 000068                           'LISTADO DE CLIENTES Y CUENTAS DEL ALU00'.
 000069           03 CT-ALU               PIC XX         VALUE SPACES.
 000070           03 FILLER               PIC X(10)      VALUE SPACES.
 000071           03 FILLER               PIC X(04)      VALUE 'AL: '.
 000072           03 CT-DD                PIC X(02)      VALUE SPACES.
 000073           03 FILLER               PIC X(01)      VALUE '-'.
 000074           03 CT-MM                PIC X(02)      VALUE SPACES.
 000075           03 FILLER               PIC X(01)      VALUE '-'.
 000076           03 CT-AAAA              PIC X(04)      VALUE SPACES.
 000077           03 FILLER               PIC X(10)      VALUE SPACES.
 000078           03 FILLER               PIC X(12)      VALUE 'PAGINA NRO: '.
 000079           03 CT-PAGINA            PIC 99         VALUE 01.
 000080
 000081       /----LAYOUT-SUB-TITULO-CLIENTE---/
 000082        01 CT-SUBTITULO-CLIE.
 000083           03  FILLER              PIC X(1)       VALUE '|'.
 000084           03  FILLER              PIC X(13)      VALUE 'CLIENTE NRO: '.
 000085           03  WS-CLI-NROCLI       PIC 9(5)       VALUE ZEROS.
 000086           03  FILLER              PIC X(1)       VALUE '|'.
 000087           03  FILLER              PIC X(10)      VALUE 'TIPO DOC: '.
 000088           03  WS-CLI-TIPDOC       PIC X(02)      VALUE SPACES.
 000089           03  FILLER              PIC X(01)      VALUE '|'.
 000090           03  FILLER              PIC X(9)       VALUE 'NRO DOC: '.
 000091           03  WS-CLI-NRODOC       PIC X(11)      VALUE ZEROS.
 000092           03  FILLER              PIC X(01)      VALUE '|'.
 000093           03  FILLER              PIC X(17)      VALUE
 000094                                                  'NOMBRE-APELLIDO: '.
 000095           03  WS-CLI-NOMBRE       PIC X(15)      VALUE SPACES.
 000096           03  FILLER              PIC X(01)      VALUE ' '.
 000097           03  WS-CLI-APELLIDO     PIC X(15)      VALUE SPACES.
 000098           03  FILLER              PIC X(01)      VALUE '|'.
 000099           03  FILLER              PIC X(12)      VALUE 'FECHA ALTA: '.
 000100           03  WS-CLI-F-ALTA       PIC X(10)      VALUE SPACES.
 000101       *   03  FILLER              PIC X(01)      VALUE '|'.
 000102       *   03  FILLER              PIC X(12)      VALUE 'FECHA BAJA: '.
 000103       *   03  WS-CLI-F-ALTA       PIC X(10)      VALUE SPACES.
 000104       *   03  FILLER              PIC X(01)      VALUE  '|'.
 000105
 000106       /---LAYOUT-SUB-TITULO-CUENTA-----/
 000107        01 CT-SUBTITULO-CUEN.
 000108           03  FILLER              PIC X(2)       VALUE '| '.
 000109           03  FILLER              PIC X(12)      VALUE 'TIPO CUENTA '.
 000110           03  FILLER              PIC X(2)       VALUE '| '.
 000111           03  FILLER              PIC X(12)      VALUE '  CUENTA NRO'.
 000112           03  FILLER              PIC X(7)       VALUE '   | '.
 000113           03  FILLER              PIC X(07)      VALUE 'MONEDA '.
 000114           03  FILLER              PIC X(2)       VALUE '| '.
 000115           03  FILLER              PIC X(12)      VALUE '    CBU     '.
 000116           03  FILLER              PIC X(2)       VALUE '| '.
 000117           03  FILLER              PIC X(12)      VALUE 'CLIENTE NRO '.
 000118           03  FILLER              PIC X(4)       VALUE '|   '.
 000119           03  FILLER              PIC X(12)      VALUE 'SALDO ACTUAL'.
 000120           03  FILLER              PIC X(6)       VALUE '    | '.
 000121           03  FILLER              PIC X(17)      VALUE
 000122                                                   'FECHA DE PROCESO '.
 000123           03  FILLER              PIC X(2)       VALUE '| '.
 000124           03  FILLER              PIC X(21)      VALUE
 000125                                               'FECHA ULTIMO CIERRE |'.
 000126
 000127       /------LAYOUT-CUENTA-------------/
 000128        01  WS-REG-CUENTA.
 000129            03  FILLER               PIC X(01)    VALUE '|'.
 000130            03  FILLER               PIC X(05)    VALUE SPACES.
 000131            03  WS-CUE-TIP-CUE       PIC X(02)    VALUE SPACES.
 000132            03  FILLER               PIC X(06)    VALUE SPACES.
 000133            03  FILLER               PIC X(01)    VALUE '|'.
 000134            03  WS-CUE-NRO-CTA       PIC X(15)    VALUE ZEROS.
 000135            03  FILLER               PIC X(1)     VALUE SPACES.
 000136            03  FILLER               PIC X(01)    VALUE '|'.
 000137            03  FILLER               PIC X(3)     VALUE SPACES.
 000138            03  WS-CUE-MONEDA        PIC X(02)    VALUE ZEROS.
 000139            03  FILLER               PIC X(5)     VALUE SPACES.
 000140            03  FILLER               PIC X(01)    VALUE '|'.
 000141            03  FILLER               PIC X(1)     VALUE SPACES.
 000142            03  WS-CUE-CBU           PIC 9(11)    VALUE ZEROS.
 000143            03  FILLER               PIC X(1)     VALUE SPACES.
 000144            03  FILLER               PIC X(01)    VALUE '|'.
 000145            03  FILLER               PIC X(4)     VALUE SPACES.
 000146            03  WS-CUE-NROCLI        PIC 9(05)    VALUE ZEROS.
 000147            03  FILLER               PIC X(4)     VALUE SPACES.
 000148            03  FILLER               PIC X(01)    VALUE '|'.
 000149            03  WS-CUE-SALDO  PIC -ZZZ.ZZZ.999,99 VALUE ZEROS.
 000150            03  FILLER               PIC X(4)     VALUE SPACES.
 000151            03  FILLER               PIC X(01)    VALUE '|'.
 000152            03  FILLER               PIC X(4)     VALUE SPACES.
 000153            03  WS-CUE-FECHA-PRO     PIC X(10)    VALUE SPACES.
 000154            03  FILLER               PIC X(4)     VALUE SPACES.
 000155            03  FILLER               PIC X(01)    VALUE '|'.
 000156            03  FILLER               PIC X(5)     VALUE SPACES.
 000157            03  WS-CUE-FECHA-CIE     PIC X(10)    VALUE SPACES.
 000158            03  FILLER               PIC X(6)     VALUE SPACES.
 000159            03  FILLER               PIC X(01)    VALUE '|'.
 000160
 000161       /-------PIE-DE-PAGINA------------/
 000162        01 CT-PIE-PAGINA.
 000163          03 FILLER                PIC X(2)       VALUE '| '.
 000164          03 FILLER                PIC X(18)      VALUE
 000165                                                  'REFERENCIA SALDO: '.
 000166          03 FILLER                PIC X(11)      VALUE 'PESOS= 80 ,'.
 000167          03 FILLER                PIC X(08)      VALUE 'USD= 02 '.
 000168          03 FILLER                PIC X(1)       VALUE '/'.
 000169          03 FILLER                PIC X(22)      VALUE
 000170                                              'SALDO TOTAL DE PESOS= '.
 000171          03 WS-SAL-PESOS        PIC -ZZZ.ZZZ.999,99 VALUE ZEROS.
 000172          03 FILLER                PIC X(3)       VALUE ' / '.
 000173          03 FILLER                PIC X(24)      VALUE
 000174                                            'SALDO TOTAL DE DOLARES= '.
 000175          03 WS-SAL-DOLAR        PIC -ZZZ.ZZZ.999,99 VALUE ZEROS.
 000176          03 FILLER                PIC X(1)       VALUE '/'.
 000177
 000178       /-----CUENTA-NO-FOUND------------/
 000179        01 CT-NO-CUEN-FOUND.
 000180          03 FILLER                PIC XX         VALUE '| '.
 000181          03 FILLER                PIC X(19)      VALUE
 000182                                                 'CLIENTE SIN CUENTAS'.
 000183
 000184       /----SEPARADORES-----------------/
 000185        01 CT-SEPARADOR-TITULO.
 000186          03 CT-SEPARADOR-TIT        PIC X(154).
 000187
 000188        01 CT-SEPARADOR-SUBTITULO.
 000189          03 CT-SEPARADOR-SUB        PIC X(154).
 000190
 000191       /------CONTROL-CONTADORES--------/
 000192        01 WS-CONTADOR.
 000193          04 WS-LEIDOS-CLI         PIC 9(03)      VALUE ZEROS.
 000194          04 WS-LEIDOS-CUE         PIC 9(03)      VALUE ZEROS.
 000195          04 WS-GRABADOS-CLI       PIC 9(03)      VALUE ZEROS.
 000196          04 WS-GRABADOS-CUE       PIC 9(03)      VALUE ZEROS.
 000197          04 WS-SIN-CUENTAS        PIC 9(03)      VALUE ZEROS.
 000198
 000199       /-----------SALDOS---------------/
 000200        01 WS-SALDOS-FIN.
 000201          02 WS-SALDO-PESOS          PIC S9(14)V99 COMP-3 VALUE ZEROS.
 000202          02 WS-SALDO-DOLAR          PIC S9(14)V99 COMP-3 VALUE ZEROS.
 000203
 000204       /------TEXTO-DISPLAY-------------/
 000205        01 CT-DISPLAY.
 000206          02 CT-ENCONTRADOS-CLI    PIC X(31)      VALUE
 000207                                     'CANTIDAD CLIENTES ENCONTRADOS: '.
 000208          02 CT-ENCONTRADOS-CUE    PIC X(30)      VALUE
 000209                                      'CANTIDAD CUENTAS ENCONTRADAS: '.
 000210          02 CT-GRABADOS-CLI       PIC X(28)      VALUE
 000211                                        'CANTIDAD CLIENTES GRABADOS: '.
 000212          02 CT-GRABADOS-CUE       PIC X(27)      VALUE
 000213                                         'CANTIDAD CUENTAS GRABADAS: '.
 000214          02 CT-NO-ENCONTRADO-CUE  PIC X(33)      VALUE
 000215                                     'CANTIDAD CLIENTES SIN CUENTAS: '.
 000216
 000217       /------VARIABLES-----------------/
 000218        01 KEY-CORTE.
 000219          02 WS-NROCLI-CLI-ANT     PIC 9(05)      VALUE ZEROS.
 000220          02 WS-NROCLI-CLI-ACT     PIC 9(05)      VALUE ZEROS.
 000221          02 WS-NROCLI-CUE-ANT     PIC 9(05)      VALUE ZEROS.
 000222          02 WS-NROCLI-CUE-ACT     PIC 9(05)      VALUE ZEROS.
 000223
 000224        01 WS-VARIABLES.
 000225          02 WS-NRO-MAX            PIC S9(5)V COMP-3 VALUE ZEROS.
 000226          02 WS-NRO-MIN            PIC S9(5)V COMP-3 VALUE ZEROS.
 000227
 000228        01 WS-ALUXX.
 000229          02 WS-ALU                PIC 99  VALUE ZEROS.
 000230
 000231       /------CONTADOR-LINEAS-----------/
 000232         77  WS-CUENTA-LINEA       PIC 9(02)    VALUE ZEROS.
 000233         77  WS-CUENTA-PAGINA      PIC 9(02)    VALUE 01.
 000234
 000235       /------SQLCA-COMMUNICATION-------/
 000236            EXEC SQL
 000237             INCLUDE SQLCA
 000238            END-EXEC.
 000239
 000240       /-INCLUDE-DCLGEN-TABLAS----------/
 000241            EXEC SQL
 000242             INCLUDE DB2CUEN
 000243            END-EXEC.
 000244            EXEC SQL
 000245             INCLUDE DB2CLIE
 000246            END-EXEC.
 000247
 000248       /--DECLARACION-CURSOR-CLIENTES---/
 000249            EXEC SQL
 000250            DECLARE ITEM_CURSOR1 CURSOR
 000251            FOR
 000252            SELECT TIPO_DOCUMENTO, NRO_DOCUMENTO, NRO_CLIENTE,
 000253                   NOMBRE_CLIENTE, APELLIDO_CLIENTE, FECHA_DE_ALTA,
 000254                   FECHA_DE_BAJA
 000255
 000256            FROM ITPFBIO.TBCLIENT
 000257
 000258            WHERE NRO_CLIENTE BETWEEN :WS-NRO-MIN AND :WS-NRO-MAX
 000259
 000260            ORDER BY NRO_CLIENTE ASC
 000261            END-EXEC.
 000262
 000263       /--DECLARACION-CURSOR-CUENTAS----/
 000264            EXEC SQL
 000265            DECLARE ITEM_CURSOR2 CURSOR
 000266            FOR
 000267            SELECT TIPO_CUENTA, NRO_CUENTA, MONEDA, CBU, NRO_CLIENTE,
 000268                   SALDO_ACTUAL, FECHA_ACTUAL, FECHA_ULTIMO_CIERRE
 000269
 000270            FROM ITPFBIO.TBCUENTAS
 000271
 000272            WHERE NRO_CLIENTE BETWEEN :WS-NRO-MIN AND :WS-NRO-MAX
 000273
 000274            ORDER BY NRO_CLIENTE ASC
 000275            END-EXEC.
 000276
 000277        77  FILLER                 PIC X(26)      VALUE
 000278                                   '* FINAL  WORKING-STORAGE *'.
 000279       /--------------------------------/
 000280        PROCEDURE DIVISION.
 000281       /--------------------------------/
 000282       /--CUERPO-PRINCIPAL-DEL-PROGRAMA-/
 000283        MAIN-PROGRAM.
 000284
 000285            PERFORM 1000-I-INICIO
 000286               THRU 1000-F-INICIO
 000287
 000288            PERFORM 2000-I-PROCESO
 000289               THRU 2000-F-PROCESO
 000290              UNTIL WS-FIN-CLI OR WS-FIN-CUE
 000291
 000292            PERFORM 9999-I-FINAL
 000293               THRU 9999-F-FINAL
 000294            .
 000295        F-MAIN-PROGRAM.
 000296            GOBACK.
 000297
 000298       /----INICIO-APERTURA-FILES-------/
 000299        1000-I-INICIO.
 000300            ACCEPT WS-ALU   FROM SYSIN
 000301            MOVE WS-ALU                           TO CT-ALU
 000302
 000303            ACCEPT WS-FECHA FROM DATE YYYYMMDD
 000304            MOVE WS-FECHA-AAAA                    TO CT-AAAA
 000305            MOVE WS-FECHA-MM                      TO CT-MM
 000306            MOVE WS-FECHA-DD                      TO CT-DD
 000307
 000308            PERFORM 2100-I-CALCULO-MAXIMOS
 000309               THRU 2100-I-CALCULO-MAXIMOS
 000310
 000311            SET WS-NO-FIN                         TO TRUE
 000312            SET WS-NO-FIN-CLI                     TO TRUE
 000313            SET WS-NO-FIN-CUE                     TO TRUE
 000314
 000315            EXEC SQL
 000316            OPEN ITEM_CURSOR1
 000317            END-EXEC
 000318
 000319            IF SQLCODE NOT EQUAL ZEROS
 000320             MOVE SQLCODE                         TO FS-SQLCODE
 000321             DISPLAY '* ERROR EN OPEN CURSOR1= '     FS-SQLCODE
 000322             MOVE 9999                            TO RETURN-CODE
 000323             SET  WS-FIN                          TO TRUE
 000324            END-IF
 000325
 000326            EXEC SQL
 000327            OPEN ITEM_CURSOR2
 000328            END-EXEC
 000329
 000330            IF SQLCODE NOT EQUAL ZEROS
 000331             MOVE SQLCODE                         TO FS-SQLCODE
 000332             DISPLAY '* ERROR EN OPEN CURSOR2= '     FS-SQLCODE
 000333             MOVE 9999                            TO RETURN-CODE
 000334             SET  WS-FIN                          TO TRUE
 000335            END-IF
 000336
 000337            OPEN OUTPUT SALIDA
 000338            IF FS-SALIDA IS NOT EQUAL '00'
 000339              DISPLAY '* ERROR EN OPEN PERSONA= ' FS-SALIDA
 000340              MOVE 9999                           TO RETURN-CODE
 000341              SET  WS-FIN                         TO TRUE
 000342            END-IF
 000343
 000344            PERFORM 3000-I-LEER-CLIENTE
 000345               THRU 3000-F-LEER-CLIENTE
 000346
 000347            MOVE WS-NRO-CLIENTE                   TO WS-NROCLI-CLI-ANT
 000348
 000349            PERFORM 3500-I-LEER-CUENTA
 000350               THRU 3500-F-LEER-CUENTA
 000351
 000352            MOVE CU-NRO-CLIENTE                   TO WS-NROCLI-CUE-ANT
 000353
 000354            PERFORM 5900-I-IMPRIMIR-INICIO
 000355               THRU 5900-F-IMPRIMIR-INICIO
 000356            .
 000357        1000-F-INICIO.
 000358            EXIT.
 000359
 000360       /----PROCESO-PRINCIPAL-----------/
 000361        2000-I-PROCESO.
 000362            IF WS-NROCLI-CLI-ACT = WS-NROCLI-CLI-ANT
 000363              IF WS-NROCLI-CUE-ACT = WS-NROCLI-CUE-ANT
 000364
 000365                IF WS-CUE-MONEDA = 80
 000366                  ADD CU-SALDO-ACTUAL               TO WS-SALDO-PESOS
 000367                ELSE
 000368                  ADD CU-SALDO-ACTUAL               TO WS-SALDO-DOLAR
 000369                END-IF
 000370
 000371                PERFORM 5300-I-IMPRIMIR-CUENTA
 000372                   THRU 5300-F-IMPRIMIR-CUENTA
 000373
 000374                PERFORM 3500-I-LEER-CUENTA
 000375                   THRU 3500-F-LEER-CUENTA
 000376
 000377              ELSE
 000378       * CAMBIO CLIE
 000379                MOVE CU-NRO-CLIENTE               TO WS-NROCLI-CUE-ANT
 000380                MOVE WS-SALDO-PESOS               TO WS-SAL-PESOS
 000381                MOVE WS-SALDO-DOLAR               TO WS-SAL-DOLAR
 000382                PERFORM 3000-I-LEER-CLIENTE
 000383                   THRU 3000-F-LEER-CLIENTE
 000384
 000385                PERFORM 5500-I-IMPRIMIR-SEPARADOR-B
 000386                   THRU 5500-F-IMPRIMIR-SEPARADOR-B
 000387
 000388                PERFORM 5600-I-IMPRIMIR-PIE
 000389                   THRU 5600-F-IMPRIMIR-PIE
 000390
 000391                PERFORM 5400-I-IMPRIMIR-SEPARADOR-A
 000392                   THRU 5400-F-IMPRIMIR-SEPARADOR-A
 000393
 000394                INITIALIZE WS-SALDO-PESOS
 000395                INITIALIZE WS-SALDO-DOLAR
 000396
 000397              END-IF
 000398
 000399            ELSE
 000400
 000401              MOVE WS-NRO-CLIENTE                 TO WS-NROCLI-CLI-ANT
 000402
 000403              PERFORM 5100-I-IMPRIMIR-CLIENTE
 000404                 THRU 5100-F-IMPRIMIR-CLIENTE
 000405
 000406              PERFORM 5500-I-IMPRIMIR-SEPARADOR-B
 000407                 THRU 5500-F-IMPRIMIR-SEPARADOR-B
 000408
 000409              PERFORM 5200-I-IMPRIMIR-SUB-CUEN
 000410                 THRU 5200-I-IMPRIMIR-SUB-CUEN
 000411
 000412              PERFORM 5500-I-IMPRIMIR-SEPARADOR-B
 000413                 THRU 5500-F-IMPRIMIR-SEPARADOR-B
 000414
 000415              IF WS-NROCLI-CLI-ACT < WS-NROCLI-CUE-ACT
 000416                ADD 1 TO WS-SIN-CUENTAS
 000417
 000418                PERFORM 5800-I-SIN-CUENTAS
 000419                   THRU 5800-F-SIN-CUENTAS
 000420
 000421                PERFORM 3000-I-LEER-CLIENTE
 000422                   THRU 3000-F-LEER-CLIENTE
 000423              END-IF
 000424
 000425            END-IF
 000426
 000427            IF WS-FIN-CLI OR WS-FIN-CUE
 000428                PERFORM 5500-I-IMPRIMIR-SEPARADOR-B
 000429                   THRU 5500-F-IMPRIMIR-SEPARADOR-B
 000430
 000431                PERFORM 5600-I-IMPRIMIR-PIE
 000432                   THRU 5600-F-IMPRIMIR-PIE
 000433
 000434                PERFORM 5400-I-IMPRIMIR-SEPARADOR-A
 000435                   THRU 5400-F-IMPRIMIR-SEPARADOR-A
 000436            END-IF
 000437            .
 000438        2000-F-PROCESO.
 000439            EXIT.
 000440
 000441       /-----CALCULO-MAXIMOS-DE-NRO-CLIENTE--------/
 000442        2100-I-CALCULO-MAXIMOS.
 000443       *    MULTIPLY WS-ALU BY 1000 GIVING WS-NRO-MIN
 000444       *    ADD 1 TO WS-ALU
 000445       *    MULTIPLY WS-ALU BY 1000 GIVING WS-NRO-MAX
 000446       *    SUBTRACT 1 FROM WS-ALU
 000447       *
 000448            MOVE 1000 TO WS-NRO-MIN
 000449            MOVE 2000 TO WS-NRO-MAX
 000450            .
 000451        2100-F-CALCULO-MAXIMOS.
 000452            EXIT.
 000453
 000454       /-----LEO-CLIENTE--------------/
 000455        3000-I-LEER-CLIENTE.
 000456            EXEC SQL
 000457                 FETCH ITEM_CURSOR1
 000458                 INTO  :DCLTBCLIENT.WS-TIPO-DOCUMENTO,
 000459                       :DCLTBCLIENT.WS-NRO-DOCUMENTO,
 000460                       :DCLTBCLIENT.WS-NRO-CLIENTE,
 000461                       :DCLTBCLIENT.WS-NOMBRE-CLIENTE,
 000462                       :DCLTBCLIENT.WS-APELLIDO-CLIENTE,
 000463                       :DCLTBCLIENT.WS-FECHA-DE-ALTA,
 000464                       :DCLTBCLIENT.WS-FECHA-DE-BAJA
 000465            END-EXEC
 000466
 000467            MOVE SQLCODE                          TO FS-SQLCODE
 000468
 000469            EVALUATE SQLCODE
 000470            WHEN ZEROS
 000471              ADD 1                               TO WS-LEIDOS-CLI
 000472              MOVE WS-NRO-CLIENTE                 TO WS-NROCLI-CLI-ACT
 000473
 000474              PERFORM 3100-I-MOVER-CAMPOS-CLIE
 000475                 THRU 3100-F-MOVER-CAMPOS-CLIE
 000476
 000477            WHEN 100
 000478              SET WS-FIN-CLI                      TO TRUE
 000479
 000480            WHEN OTHER
 000481              DISPLAY 'ERROR EN FETCH CURSOR CLIENTE= ' FS-SQLCODE
 000482              MOVE 9999                           TO RETURN-CODE
 000483              SET WS-FIN                          TO TRUE
 000484            END-EVALUATE
 000485            .
 000486        3000-F-LEER-CLIENTE.
 000487            EXIT.
 000488
 000489       /--MUEVO-CAMPOS-A-SUB-TITULO-CLIENTE--/
 000490        3100-I-MOVER-CAMPOS-CLIE.
 000491            MOVE WS-NRO-CLIENTE              TO WS-CLI-NROCLI
 000492            MOVE WS-TIPO-DOCUMENTO           TO WS-CLI-TIPDOC
 000493            MOVE WS-NRO-DOCUMENTO            TO WS-CLI-NRODOC
 000494            MOVE WS-NOMBRE-CLIENTE           TO WS-CLI-NOMBRE
 000495            MOVE WS-APELLIDO-CLIENTE         TO WS-CLI-APELLIDO
 000496            MOVE WS-FECHA-DE-ALTA            TO WS-CLI-F-ALTA
 000497            .
 000498        3100-F-MOVER-CAMPOS-CLIE.
 000499            EXIT.
 000500
 000501       /-----LEO-CUENTAS--------------/
 000502        3500-I-LEER-CUENTA.
 000503            EXEC SQL
 000504                 FETCH ITEM_CURSOR2
 000505                 INTO  :DCLTBCUENTAS.CU-TIPO-CUENTA,
 000506                       :DCLTBCUENTAS.CU-NRO-CUENTA,
 000507                       :DCLTBCUENTAS.CU-MONEDA,
 000508                       :DCLTBCUENTAS.CU-CBU,
 000509                       :DCLTBCUENTAS.CU-NRO-CLIENTE,
 000510                       :DCLTBCUENTAS.CU-SALDO-ACTUAL,
 000511                       :DCLTBCUENTAS.CU-FECHA-ACTUAL,
 000512                       :DCLTBCUENTAS.CU-FECHA-ULTIMO-CIERRE
 000513            END-EXEC
 000514
 000515            MOVE SQLCODE                          TO FS-SQLCODE
 000516
 000517            EVALUATE SQLCODE
 000518            WHEN ZEROS
 000519              ADD 1                               TO WS-LEIDOS-CUE
 000520              MOVE CU-NRO-CLIENTE                 TO WS-NROCLI-CUE-ACT
 000521
 000522              PERFORM 3600-I-MOVER-CAMPOS-CUEN
 000523                 THRU 3600-F-MOVER-CAMPOS-CUEN
 000524
 000525            WHEN 100
 000526              SET WS-FIN-CUE                      TO TRUE
 000527
 000528            WHEN OTHER
 000529              DISPLAY 'ERROR EN FETCH CURSOR CUENTA= ' FS-SQLCODE
 000530              MOVE 9999                           TO RETURN-CODE
 000531              SET WS-FIN                          TO TRUE
 000532            END-EVALUATE
 000533            .
 000534        3500-F-LEER-CUENTA.
 000535            EXIT.
 000536
 000537       /---MUEVO-CAMPOS-A-SUB-TITULO-CUEN--/
 000538        3600-I-MOVER-CAMPOS-CUEN.
 000539            MOVE CU-TIPO-CUENTA              TO WS-CUE-TIP-CUE
 000540            MOVE CU-NRO-CUENTA               TO WS-CUE-NRO-CTA
 000541            MOVE CU-MONEDA                   TO WS-CUE-MONEDA
 000542            MOVE CU-CBU                      TO WS-CUE-CBU
 000543            MOVE CU-NRO-CLIENTE              TO WS-CUE-NROCLI
 000544            MOVE CU-SALDO-ACTUAL             TO WS-CUE-SALDO
 000545            MOVE CU-FECHA-ACTUAL             TO WS-CUE-FECHA-PRO
 000546            MOVE CU-FECHA-ULTIMO-CIERRE      TO WS-CUE-FECHA-CIE
 000547            .
 000548        3600-F-MOVER-CAMPOS-CUEN.
 000549            EXIT.
 000550
 000551       /--IMPRIMIR-TITULO---------------/
 000552        5000-I-IMPRIMIR-TITULO.
 000553            PERFORM 5400-I-IMPRIMIR-SEPARADOR-A
 000554               THRU 5400-F-IMPRIMIR-SEPARADOR-A
 000555
 000556            WRITE REG-SALIDA FROM CT-TITULO AFTER PAGE
 000557
 000558            IF FS-SALIDA IS NOT EQUAL '00'
 000559               DISPLAY '* ERROR EN GRABAR TITULO: ' FS-SALIDA
 000560               MOVE 9999 TO RETURN-CODE
 000561               SET WS-FIN TO TRUE
 000562            END-IF
 000563
 000564            ADD 1 TO WS-CUENTA-LINEA
 000565            .
 000566        5000-F-IMPRIMIR-TITULO.
 000567            EXIT.
 000568
 000569       /--IMPRIMIR-CLIENTE--------------/
 000570        5100-I-IMPRIMIR-CLIENTE.
 000571            IF WS-CUENTA-LINEA GREATER 60
 000572              PERFORM 5700-I-CAMBIO-PAGINA
 000573                 THRU 5700-I-CAMBIO-PAGINA
 000574            END-IF
 000575
 000576            WRITE REG-SALIDA FROM CT-SUBTITULO-CLIE
 000577
 000578            IF FS-SALIDA IS NOT EQUAL '00'
 000579               DISPLAY '* ERROR EN GRABAR CLIENTE: ' FS-SALIDA
 000580               MOVE 9999 TO RETURN-CODE
 000581               SET WS-FIN TO TRUE
 000582            END-IF
 000583
 000584            ADD 1 TO WS-CUENTA-LINEA
 000585            ADD 1 TO WS-GRABADOS-CLI
 000586            .
 000587        5100-F-IMPRIMIR-CLIENTE.
 000588            EXIT.
 000589
 000590       /--IMPRIMIR-SUBTITULO-CUENTA-----/
 000591        5200-I-IMPRIMIR-SUB-CUEN.
 000592            IF WS-CUENTA-LINEA GREATER 60
 000593              PERFORM 5700-I-CAMBIO-PAGINA
 000594                 THRU 5700-I-CAMBIO-PAGINA
 000595            END-IF
 000596
 000597            WRITE REG-SALIDA FROM CT-SUBTITULO-CUEN
 000598
 000599            IF FS-SALIDA IS NOT EQUAL '00'
 000600               DISPLAY '* ERROR EN GRABAR SUBTITULO: ' FS-SALIDA
 000601               MOVE 9999 TO RETURN-CODE
 000602               SET WS-FIN TO TRUE
 000603            END-IF
 000604
 000605            ADD 1 TO WS-CUENTA-LINEA
 000606            .
 000607        5200-F-IMPRIMIR-SUB-CUEN.
 000608            EXIT.
 000609
 000610       /--IMPRIMIR-CUENTA---------------/
 000611        5300-I-IMPRIMIR-CUENTA.
 000612            IF WS-CUENTA-LINEA GREATER 60
 000613              PERFORM 5700-I-CAMBIO-PAGINA
 000614                 THRU 5700-I-CAMBIO-PAGINA
 000615            END-IF
 000616
 000617            WRITE REG-SALIDA FROM WS-REG-CUENTA
 000618
 000619            IF FS-SALIDA IS NOT EQUAL '00'
 000620               DISPLAY '* ERROR EN GRABAR CUENTA: ' FS-SALIDA
 000621               MOVE 9999 TO RETURN-CODE
 000622               SET WS-FIN TO TRUE
 000623            END-IF
 000624
 000625            ADD 1 TO WS-CUENTA-LINEA
 000626            ADD 1 TO WS-GRABADOS-CUE
 000627            .
 000628        5300-F-IMPRIMIR-CUENTA.
 000629            EXIT.
 000630
 000631       /--IMPRIMIR-SEPARADOR-(=)--------/
 000632        5400-I-IMPRIMIR-SEPARADOR-A.
 000633            MOVE ALL '='                          TO CT-SEPARADOR-TIT
 000634
 000635            WRITE REG-SALIDA FROM CT-SEPARADOR-TITULO
 000636
 000637            IF FS-SALIDA IS NOT EQUAL '00'
 000638               DISPLAY '* ERROR EN GRABAR SEPARADOR A: ' FS-SALIDA
 000639               MOVE 9999 TO RETURN-CODE
 000640               SET WS-FIN TO TRUE
 000641            END-IF
 000642
 000643            ADD 1 TO WS-CUENTA-LINEA
 000644            .
 000645        5400-F-IMPRIMIR-SEPARADOR-A.
 000646            EXIT.
 000647
 000648       /--IMPRIMIR-SEPARADOR-(-)--------/
 000649        5500-I-IMPRIMIR-SEPARADOR-B.
 000650            MOVE ALL '-'                          TO CT-SEPARADOR-SUB
 000651
 000652            WRITE REG-SALIDA FROM CT-SEPARADOR-SUBTITULO
 000653
 000654            IF FS-SALIDA IS NOT EQUAL '00'
 000655               DISPLAY '* ERROR EN GRABAR SEPARADOR B: ' FS-SALIDA
 000656               MOVE 9999 TO RETURN-CODE
 000657               SET WS-FIN TO TRUE
 000658            END-IF
 000659
 000660            ADD 1 TO WS-CUENTA-LINEA
 000661            .
 000662        5500-F-IMPRIMIR-SEPARADOR-B.
 000663            EXIT.
 000664
 000665       /--IMPRIMIR-FINAL-CLIENTE--------/
 000666        5600-I-IMPRIMIR-PIE.
 000667            WRITE REG-SALIDA FROM CT-PIE-PAGINA
 000668
 000669            IF FS-SALIDA IS NOT EQUAL '00'
 000670               DISPLAY '* ERROR EN GRABAR PIE DE PAGINA: ' FS-SALIDA
 000671               MOVE 9999 TO RETURN-CODE
 000672               SET WS-FIN TO TRUE
 000673            END-IF
 000674
 000675            ADD 1 TO WS-CUENTA-LINEA
 000676            .
 000677        5600-F-IMPRIMIR-PIE.
 000678            EXIT.
 000679
 000680       /--IMPRIMIR-CAMBIO-DE-PAGINA-----/
 000681        5700-I-CAMBIO-PAGINA.
 000682              INITIALIZE WS-CUENTA-LINEA
 000683              ADD 1                               TO CT-PAGINA
 000684
 000685              PERFORM 5000-I-IMPRIMIR-TITULO
 000686                 THRU 5000-F-IMPRIMIR-TITULO
 000687
 000688              PERFORM 5400-I-IMPRIMIR-SEPARADOR-A
 000689                 THRU 5400-F-IMPRIMIR-SEPARADOR-A
 000690            .
 000691        5700-F-CAMBIO-PAGINA.
 000692            EXIT.
 000693
 000694       /--IMPRIMIR-LEYENDA-NO-CUENTAS---/
 000695        5800-I-SIN-CUENTAS.
 000696            WRITE REG-SALIDA FROM CT-NO-CUEN-FOUND
 000697
 000698            IF FS-SALIDA IS NOT EQUAL '00'
 000699               DISPLAY '* ERROR EN GRABAR LEYENDA: ' FS-SALIDA
 000700               MOVE 9999 TO RETURN-CODE
 000701               SET WS-FIN TO TRUE
 000702            END-IF
 000703
 000704            ADD 1 TO WS-CUENTA-LINEA
 000705            ADD 1 TO WS-SIN-CUENTAS
 000706            .
 000707        5800-F-SIN-CUENTAS.
 000708            EXIT.
 000709
 000710        5900-I-IMPRIMIR-INICIO.
 000711            PERFORM 5000-I-IMPRIMIR-TITULO
 000712               THRU 5000-F-IMPRIMIR-TITULO
 000713
 000714            PERFORM 5400-I-IMPRIMIR-SEPARADOR-A
 000715               THRU 5400-F-IMPRIMIR-SEPARADOR-A
 000716
 000717            PERFORM 5100-I-IMPRIMIR-CLIENTE
 000718               THRU 5100-F-IMPRIMIR-CLIENTE
 000719
 000720            PERFORM 5500-I-IMPRIMIR-SEPARADOR-B
 000721               THRU 5500-F-IMPRIMIR-SEPARADOR-B
 000722
 000723            PERFORM 5200-I-IMPRIMIR-SUB-CUEN
 000724               THRU 5200-F-IMPRIMIR-SUB-CUEN
 000725
 000726            PERFORM 5500-I-IMPRIMIR-SEPARADOR-B
 000727               THRU 5500-F-IMPRIMIR-SEPARADOR-B
 000728            .
 000729        5900-F-IMPRIMIR-INICIO.
 000730            EXIT.
 000731
 000732       /------CIERRE-DE-ARCHIVOS--------/
 000733        9999-I-FINAL.
 000734            CLOSE SALIDA
 000735            IF FS-SALIDA  IS NOT EQUAL '00'
 000736              DISPLAY '* ERROR EN CLOSE SALIDA= ' FS-SALIDA
 000737              MOVE 9999                           TO RETURN-CODE
 000738              SET WS-FIN                          TO TRUE
 000739            END-IF
 000740
 000741            EXEC SQL
 000742                 CLOSE ITEM_CURSOR1
 000743            END-EXEC.
 000744
 000745            IF SQLCODE NOT EQUAL ZEROS
 000746               MOVE SQLCODE                    TO FS-SQLCODE
 000747               DISPLAY '* ERROR EN CLOSE CURSOR CLIENTES: ' FS-SQLCODE
 000748               MOVE 9999                       TO RETURN-CODE
 000749               SET  WS-FIN                     TO TRUE
 000750            END-IF
 000751
 000752            EXEC SQL
 000753                 CLOSE ITEM_CURSOR2
 000754            END-EXEC.
 000755
 000756            IF SQLCODE NOT EQUAL ZEROS
 000757               MOVE SQLCODE                    TO FS-SQLCODE
 000758               DISPLAY '* ERROR EN CLOSE CURSOR CUENTAS: ' FS-SQLCODE
 000759               MOVE 9999                       TO RETURN-CODE
 000760               SET  WS-FIN                     TO TRUE
 000761            END-IF
 000762
 000763       /-----MUESTRO-TOTALES-DE-CONTROL----/
 000764            DISPLAY '/----------TOTALES DE CONTROL------------/'
 000765            DISPLAY CT-ENCONTRADOS-CLI    WS-LEIDOS-CLI
 000766            DISPLAY CT-ENCONTRADOS-CUE    WS-LEIDOS-CUE
 000767            DISPLAY CT-GRABADOS-CLI       WS-GRABADOS-CLI
 000768            DISPLAY CT-GRABADOS-CUE       WS-GRABADOS-CUE
 000769            DISPLAY CT-NO-ENCONTRADO-CUE  WS-SIN-CUENTAS
 000770            .
 000771        9999-F-FINAL.
 000772            EXIT.
 ****** **************************** Bottom of Data ****************************

