frant @ DESKTOP-LM8QUAK 2021-06-17 06:14:11

 ****** ***************************** Top of Data ******************************
 000001       /----------------------ROBCO-INDUSTRIES-<TM>-TERMLINK PROTOCOL-/
 000002       /--------------------------------/
 000003        IDENTIFICATION DIVISION.
 000004       /--------------------------------/
 000005        PROGRAM-ID. PGMPRU01.
 000006       /--------------------------------/
 000007        DATA DIVISION.
 000008       /--------------------------------/
 000009        FILE SECTION.
 000010       /--------------------------------/
 000011        WORKING-STORAGE SECTION.
 000012       /--------------------------------/
 000013        01 CT-MENSAJES.
 000014           04 CT-DATASET            PIC  X(08)        VALUE 'PERSONA'.
 000015           04 CT-LEN-DAT            PIC  S9(04)       VALUE 160  COMP.
 000016           04 CT-CONFIRMA           PIC  X(42)        VALUE
 000017                         'DESEA ELIMINAR EL REGISTRO ? Y(PF6)/F(PF5)'.
 000018           04 CT-FIN                PIC  X(72)        VALUE
 000019                                             'FIN EJECUCION TRX T101'.
 000020           04 CT-IN                 PIC  X(54)        VALUE
 000021             'INGRESE DATOS Y PRESIONE ENTER PARA BUSCAR EL REGISTRO'.
 000022           04 CT-ERRDOC             PIC  X(26)        VALUE
 000023                                         'TIPO DE DOCUMENTO INVALIDO'.
 000024           04 CT-ERRNROD            PIC  X(28)        VALUE
 000025                                       'NUMERO DE DOCUMENTO INVALIDO'.
 000026           04 CT-CLIEN              PIC  X(45)        VALUE
 000027                      'CLIENTE ENCONTRADO, INGRESE ACCION A REALIZAR'.
 000028           04 CT-ERRARCH            PIC  X(13)        VALUE
 000029                                                      'ERROR ARCHIVO'.
 000030           04 CT-INVKEY             PIC  X(35)        VALUE
 000031                                'TECLA INVALIDA, INGRESE OTRA OPCION'.
 000032           04 CT-REGDEL             PIC  X(18)        VALUE
 000033                                                 'REGISTRO ELIMINADO'.
 000034           04 CT-INEXCLI            PIC  X(19)        VALUE
 000035                                                'CLIENTE INEXISTENTE'.
 000036           04 CT-EXISCLI            PIC  X(17)        VALUE
 000037                                                'CLIENTE EXISTENTE'.
 000038           04 CT-ARCHERR            PIC  X(24)        VALUE
 000039                                           'PROBLEMAS CON EL ARCHIVO'.
 000040           04 CT-CANCEL             PIC  X(16)        VALUE
 000041                                                   'ACCION CANCELADA'.
 000042           04 CT-CLIENF             PIC  X(26)        VALUE
 000043                                         'PRIMER REGISTRO ENCONTRADO'.
 000044           04 CT-COMPLETE           PIC  X(55)        VALUE
 000045            'INGRESE DATOS DEL REGISTRO Y PRESIONE PF7 O PF5 CANCELA'.
 000046           04 CT-ERRORCRE           PIC  X(29)        VALUE
 000047                                      'ERROR EN CREACION DE REGISTRO'.
 000048           04 CT-EXITO              PIC  X(28)        VALUE
 000049                                       'REGISTRO CREADO EXITOSAMENTE'.
 000050           04 CT-REGDUP             PIC  X(18)        VALUE
 000051                                                 'REGISTRO DUPLICADO'.
 000052
 000053        01 WS-ABSTIME               PIC  S9(16) COMP  VALUE +0.
 000054        01 WS-TIME                  PIC  X(08)        VALUE SPACES.
 000055        01 WS-TIPO-DOC              PIC  X(02)        VALUE '  '.
 000056           88 WS-TIP-DOC                   VALUE 'DU'
 000057                                                 'PE'
 000058                                                 'PA'
 000059                                                 'CI'.
 000060
 000061        01 WS-LENGTH-PERSO          PIC  S9(4)  COMP  VALUE 160.
 000062
 000063        01 WS-VAR-CICS.
 000064           02 WS-LONG               PIC  S9(04) COMP.
 000065           02 WS-SEP-D              PIC  X            VALUE '-'.
 000066           02 WS-SEP-T              PIC  X            VALUE ':'.
 000067           02 WS-MAP                PIC  X(08)        VALUE 'MAP0101'.
 000068           02 WS-MAPSET             PIC  X(08)        VALUE 'MAP0101'.
 000069           02 WS-FECHA              PIC  X(10)        VALUE SPACES.
 000070           02 WS-RESP               PIC  S9(04) COMP.
 000071           02 WS-RIDFLD.
 000072             08 WS-RID-TIPD         PIC X(02).
 000073             08 WS-RID-NROD         PIC 9(11).
 000074
 000075        01 WS-COMMAREA.
 000076           05 WS-FLAG-PF6           PIC 9             VALUE 0.
 000077           05 WS-FLAG-PF7           PIC 9             VALUE 0.
 000078           05 WS-COM-TIPD           PIC X(02).
 000079           05 WS-COM-NROD           PIC 9(11).
 000080
 000081       /--------COPY-SECTION------------/
 000082        COPY MAP0101.
 000083        COPY DFHBMSCA.
 000084        COPY DFHAID.
 000085        COPY CPPERSO.
 000086       /--------------------------------/
 000087        LINKAGE SECTION.
 000088       /--------------------------------/
 000089         01 DFHCOMMAREA PIC X(15).
 000090
 000091       /--------------------------------/
 000092        PROCEDURE DIVISION.
 000093       /--------------------------------/
 000094        0000-HOLACICS.
 000095
 000096            PERFORM 1000-I-INICIO
 000097               THRU 1000-F-INICIO
 000098
 000099            PERFORM 2000-I-PROCESO
 000100               THRU 2000-I-PROCESO
 000101
 000102            PERFORM 9999-I-FINAL
 000103               THRU 9999-F-FINAL
 000104            .
 000105        1000-I-INICIO.
 000106            PERFORM 2100-I-FECHA
 000107               THRU 2100-F-FECHA
 000108
 000109            MOVE DFHCOMMAREA                           TO WS-COMMAREA
 000110            MOVE LENGTH OF MAP0101O                    TO WS-LONG
 000111            .
 000112        1000-F-INICIO.
 000113            EXIT.
 000114
 000115        2000-I-PROCESO.
 000116
 000117            EXEC CICS
 000118                 RECEIVE MAP   (WS-MAP)
 000119                 MAPSET        (WS-MAPSET)
 000120                 INTO          (MAP0101I)
 000121                 RESP          (WS-RESP)
 000122            END-EXEC
 000123
 000124            EVALUATE WS-RESP
 000125            WHEN DFHRESP(NORMAL)
 000126
 000127                MOVE LOW-VALUES                        TO WS-RIDFLD
 000128
 000129                PERFORM 3000-I-PFKEY
 000130                   THRU 3000-F-PFKEY
 000131
 000132            WHEN DFHRESP(MAPFAIL)
 000133
 000134       *      PERFORM 2200-I-INITIALIZE
 000135       *         THRU 2200-F-INITIALIZE
 000136              INITIALIZE MAP0101O
 000137
 000138              MOVE WS-FECHA                            TO FECHAO
 000139              MOVE LENGTH OF MAP0101O                  TO WS-LONG
 000140              MOVE CT-IN                               TO MSGO
 000141
 000142              EXEC CICS
 000143                   SEND MAP (WS-MAP)
 000144                   MAPSET   (WS-MAPSET)
 000145                   FROM     (MAP0101O)
 000146                   LENGTH    (WS-LONG)
 000147                   ERASE
 000148              END-EXEC
 000149
 000150              IF EIBAID EQUAL DFHPF12
 000151                PERFORM 3500-I-PF12
 000152                   THRU 3500-F-PF12
 000153              END-IF
 000154
 000155            WHEN OTHER
 000156              MOVE 'ERROR MAPA'                        TO MSGO
 000157
 000158              EXEC CICS
 000159                   SEND MAP (WS-MAP)
 000160                   MAPSET   (WS-MAPSET)
 000161                   FROM     (MAP0101O)
 000162                   LENGTH   (WS-LONG)
 000163                   ERASE
 000164              END-EXEC
 000165
 000166            END-EVALUATE
 000167            .
 000168        2000-F-PROCESO.
 000169            EXIT.
 000170
 000171        2100-I-FECHA.
 000172            EXEC CICS
 000173                 ASKTIME
 000174                 ABSTIME (WS-ABSTIME)
 000175            END-EXEC
 000176            EXEC CICS FORMATTIME
 000177                 ABSTIME  (WS-ABSTIME)
 000178                 DDMMYYYY (WS-FECHA)    DATESEP (WS-SEP-D)
 000179                 TIME     (WS-TIME)     TIMESEP (WS-SEP-T)
 000180            END-EXEC
 000181            MOVE WS-FECHA                              TO FECHAO
 000182            .
 000183        2100-F-FECHA.
 000184            EXIT.
 000185
 000186        2200-I-INITIALIZE.
 000187            MOVE 0                                     TO NROCLIO
 000188            MOVE SPACES                                TO NOMAPEO
 000189            MOVE SPACES                                TO DIRECO
 000190            MOVE SPACES                                TO TELO
 000191            MOVE SPACES                                TO EMAILO
 000192            MOVE SPACES                                TO MSGO
 000193            .
 000194        2200-F-INITIALIZE.
 000195            EXIT.
 000196
 000197        3000-I-PFKEY.
 000198            EVALUATE TRUE ALSO TRUE
 000199            WHEN EIBAID = DFHENTER ALSO WS-FLAG-PF6 NOT = '1'
 000200
 000201              PERFORM 3100-I-ENTER
 000202                 THRU 3100-F-ENTER
 000203
 000204            WHEN EIBAID = DFHPF1   ALSO WS-FLAG-PF6 NOT = '1'
 000205
 000206              PERFORM 3400-I-PF1
 000207                 THRU 3400-F-PF1
 000208
 000209            WHEN EIBAID = DFHPF3   ALSO WS-FLAG-PF6 NOT = '1'
 000210
 000211              PERFORM 3200-I-PF3
 000212                 THRU 3200-F-PF3
 000213
 000214            WHEN EIBAID = DFHPF5   ALSO WS-FLAG-PF6 = '1'
 000215
 000216              MOVE 0                                   TO WS-FLAG-PF6
 000217              PERFORM 3700-I-CANCEL
 000218                 THRU 3700-F-CANCEL
 000219
 000220            WHEN EIBAID = DFHPF5   ALSO WS-FLAG-PF7 = '1'
 000221
 000222              MOVE 0                                   TO WS-FLAG-PF7
 000223              PERFORM 3700-I-CANCEL
 000224                 THRU 3700-F-CANCEL
 000225
 000226            WHEN EIBAID = DFHPF6   ALSO WS-FLAG-PF6 NOT = '1'
 000227
 000228              PERFORM 3300-I-PF6
 000229                 THRU 3300-F-PF6
 000230
 000231            WHEN EIBAID = DFHPF6   ALSO WS-FLAG-PF6 = '1'
 000232
 000233              PERFORM 3600-I-DELETE
 000234                 THRU 3600-F-DELETE
 000235
 000236            WHEN EIBAID = DFHPF7   ALSO WS-FLAG-PF7 NOT = '1'
 000237
 000238              PERFORM 3800-I-PF7
 000239                 THRU 3800-F-PF7
 000240
 000241            WHEN EIBAID = DFHPF7   ALSO WS-FLAG-PF7 = '1'
 000242
 000243              PERFORM 3850-I-CREATE
 000244                 THRU 3850-F-CREATE
 000245
 000246            WHEN EIBAID = DFHPF12  ALSO WS-FLAG-PF6 NOT = '1'
 000247
 000248              PERFORM 3500-I-PF12
 000249                 THRU 3500-F-PF12
 000250
 000251            WHEN OTHER
 000252
 000253                  MOVE WS-FECHA                        TO FECHAO
 000254                  MOVE CT-INVKEY                       TO MSGO
 000255
 000256                  EXEC CICS
 000257                       SEND MAP    (WS-MAP)
 000258                       MAPSET (WS-MAPSET)
 000259                       FROM   (MAP0101O)
 000260                       LENGTH (WS-LONG)
 000261                       ERASE
 000262                  END-EXEC
 000263
 000264            END-EVALUATE
 000265            .
 000266        3000-F-PFKEY.
 000267            EXIT.
 000268
 000269        3100-I-ENTER.
 000270
 000271            MOVE TIPDOCI                               TO WS-TIPO-DOC
 000272
 000273            IF NOT WS-TIP-DOC
 000274              INITIALIZE MAP0101O
 000275              MOVE WS-FECHA                            TO FECHAO
 000276              MOVE CT-ERRDOC                           TO MSGO
 000277            ELSE
 000278              IF NUMDOCI NOT NUMERIC
 000279                INITIALIZE MAP0101O
 000280                MOVE WS-FECHA                          TO FECHAO
 000281                MOVE CT-ERRNROD                        TO MSGO
 000282              ELSE
 000283                MOVE TIPDOCI                           TO WS-RID-TIPD
 000284                MOVE NUMDOCI                           TO WS-RID-NROD
 000285
 000286                EXEC CICS
 000287                     READ DATASET (CT-DATASET)
 000288                     RIDFLD       (WS-RIDFLD )
 000289                     INTO         (REG-PERSONA)
 000290                     LENGTH       (CT-LEN-DAT)
 000291                     EQUAL
 000292                     RESP         (WS-RESP)
 000293                END-EXEC
 000294
 000295                EVALUATE WS-RESP
 000296                WHEN DFHRESP(NORMAL)
 000297                  MOVE WS-FECHA                        TO FECHAO
 000298                  MOVE PER-CLI-NRO                     TO NROCLIO
 000299                  MOVE PER-NOMAPE                      TO NOMAPEO
 000300                  MOVE PER-DIRECCION                   TO DIRECO
 000301                  MOVE PER-TELEFONO                    TO TELO
 000302                  MOVE PER-EMAIL                       TO EMAILO
 000303                  MOVE CT-CLIEN                        TO MSGO
 000304                WHEN DFHRESP(NOTFND)
 000305                  INITIALIZE MAP0101O
 000306                  MOVE WS-FECHA                        TO FECHAO
 000307                  MOVE CT-INEXCLI                      TO MSGO
 000308                WHEN OTHER
 000309                  INITIALIZE MAP0101O
 000310                  MOVE WS-FECHA                        TO FECHAO
 000311                  MOVE CT-ERRARCH                      TO MSGO
 000312                END-EVALUATE
 000313              END-IF
 000314            END-IF
 000315
 000316            EXEC CICS
 000317                 SEND MAP  (WS-MAP)
 000318                 MAPSET    (WS-MAPSET)
 000319                 FROM      (MAP0101O)
 000320                 LENGTH    (WS-LONG)
 000321                 ERASE
 000322            END-EXEC
 000323            .
 000324        3100-F-ENTER.
 000325            EXIT.
 000326
 000327        3200-I-PF3.
 000328            INITIALIZE MAP0101O
 000329            MOVE WS-FECHA                              TO FECHAO
 000330            EXEC CICS
 000331                 SEND MAP  (WS-MAP)
 000332                 MAPSET    (WS-MAPSET)
 000333                 FROM      (MAP0101O)
 000334                 LENGTH    (WS-LONG)
 000335                 ERASE
 000336            END-EXEC
 000337            .
 000338        3200-F-PF3.
 000339            EXIT.
 000340
 000341        3300-I-PF6.
 000342            MOVE TIPDOCI                               TO WS-TIPO-DOC
 000343
 000344              IF NOT WS-TIP-DOC
 000345                MOVE WS-FECHA                          TO FECHAO
 000346                MOVE CT-ERRDOC                         TO MSGO
 000347                MOVE 0                                 TO WS-FLAG-PF6
 000348              ELSE
 000349                IF NUMDOCI NOT NUMERIC
 000350                  INITIALIZE MAP0101O
 000351                  MOVE WS-FECHA                        TO FECHAO
 000352                  MOVE CT-ERRNROD                      TO MSGO
 000353                  MOVE 0                               TO WS-FLAG-PF6
 000354                ELSE
 000355                  MOVE CT-CONFIRMA                     TO MSGO
 000356                  MOVE TIPDOCI                         TO WS-RID-TIPD
 000357                  MOVE NUMDOCI                         TO WS-RID-NROD
 000358                  MOVE 1                               TO WS-FLAG-PF6
 000359
 000360                  EXEC CICS
 000361                       READ DATASET (CT-DATASET)
 000362                       RIDFLD       (WS-RIDFLD )
 000363                       INTO         (REG-PERSONA)
 000364                       LENGTH       (CT-LEN-DAT)
 000365                       EQUAL
 000366                       RESP         (WS-RESP)
 000367                  END-EXEC
 000368
 000369                  EVALUATE WS-RESP
 000370                  WHEN DFHRESP(NORMAL)
 000371
 000372                    MOVE TIPDOCI                       TO WS-COM-TIPD
 000373                    MOVE NUMDOCI                       TO WS-COM-NROD
 000374                    MOVE WS-FECHA                      TO FECHAO
 000375                    MOVE PER-CLI-NRO                   TO NROCLIO
 000376                    MOVE PER-NOMAPE                    TO NOMAPEO
 000377                    MOVE PER-DIRECCION                 TO DIRECO
 000378                    MOVE PER-TELEFONO                  TO TELO
 000379                    MOVE PER-EMAIL                     TO EMAILO
 000380
 000381                  WHEN DFHRESP(NOTFND)
 000382
 000383                    INITIALIZE MAP0101O
 000384                    MOVE WS-FECHA                      TO FECHAO
 000385                    MOVE CT-INEXCLI                    TO MSGO
 000386                    MOVE 0                             TO WS-FLAG-PF6
 000387
 000388                  WHEN OTHER
 000389
 000390                    INITIALIZE MAP0101O
 000391                    MOVE WS-FECHA                      TO FECHAO
 000392                    MOVE CT-ERRARCH                    TO MSGO
 000393                    MOVE 0                             TO WS-FLAG-PF6
 000394
 000395                  END-EVALUATE
 000396                END-IF
 000397              END-IF
 000398              EXEC CICS
 000399                   SEND MAP  (WS-MAP)
 000400                   MAPSET    (WS-MAPSET)
 000401                   FROM      (MAP0101O)
 000402                   LENGTH    (WS-LONG)
 000403                   ERASE
 000404              END-EXEC
 000405            .
 000406        3300-F-PF6.
 000407            EXIT.
 000408
 000409        3400-I-PF1.
 000410            MOVE LOW-VALUES                            TO WS-RIDFLD
 000411
 000412            EXEC CICS
 000413                 STARTBR DATASET  (CT-DATASET)
 000414                 RIDFLD           (WS-RIDFLD)
 000415                 GTEQ
 000416                 RESP             (WS-RESP)
 000417            END-EXEC
 000418
 000419            EVALUATE WS-RESP
 000420            WHEN DFHRESP(NORMAL)
 000421
 000422              EXEC CICS
 000423                   READ DATASET  (CT-DATASET)
 000424                   RIDFLD        (WS-RIDFLD)
 000425                   INTO          (REG-PERSONA)
 000426                   RESP          (WS-RESP)
 000427              END-EXEC
 000428
 000429              MOVE PER-TIP-DOC                     TO TIPDOCO
 000430              MOVE PER-NRO-DOC                     TO NUMDOCO
 000431              MOVE WS-FECHA                        TO FECHAO
 000432              MOVE PER-CLI-NRO                     TO NROCLIO
 000433              MOVE PER-NOMAPE                      TO NOMAPEO
 000434              MOVE PER-DIRECCION                   TO DIRECO
 000435              MOVE PER-TELEFONO                    TO TELO
 000436              MOVE PER-EMAIL                       TO EMAILO
 000437              MOVE CT-CLIENF                       TO MSGO
 000438
 000439            WHEN OTHER
 000440
 000441              MOVE WS-FECHA                        TO FECHAO
 000442              MOVE CT-ERRARCH                      TO MSGO
 000443
 000444            END-EVALUATE
 000445
 000446            EXEC CICS
 000447                 SEND MAP  (WS-MAP)
 000448                 MAPSET    (WS-MAPSET)
 000449                 FROM      (MAP0101O)
 000450                 LENGTH    (WS-LONG)
 000451                 ERASE
 000452            END-EXEC
 000453            .
 000454        3400-F-PF1.
 000455            EXIT.
 000456
 000457        3500-I-PF12.
 000458            EXEC CICS
 000459                 SEND CONTROL ERASE
 000460            END-EXEC
 000461
 000462            EXEC CICS
 000463                 SEND TEXT
 000464                 FROM (CT-FIN)
 000465            END-EXEC
 000466
 000467            EXEC CICS
 000468                 RETURN
 000469            END-EXEC
 000470            .
 000471        3500-F-PF12.
 000472            EXIT.
 000473
 000474        3600-I-DELETE.
 000475
 000476            MOVE WS-COM-TIPD                           TO WS-RID-TIPD
 000477            MOVE WS-COM-NROD                           TO WS-RID-NROD
 000478
 000479            EXEC CICS
 000480                 DELETE DATASET (CT-DATASET)
 000481                 RIDFLD         (WS-RIDFLD)
 000482                 RESP           (WS-RESP)
 000483            END-EXEC
 000484
 000485            EVALUATE WS-RESP
 000486            WHEN DFHRESP(NORMAL)
 000487
 000488              MOVE CT-REGDEL                           TO MSGO
 000489              PERFORM 2200-I-INITIALIZE
 000490                 THRU 2200-F-INITIALIZE
 000491
 000492            WHEN DFHRESP(NOTFND)
 000493
 000494              INITIALIZE MAP0101O
 000495              MOVE WS-FECHA                            TO FECHAO
 000496              MOVE CT-INEXCLI                          TO MSGO
 000497
 000498            WHEN OTHER
 000499
 000500              MOVE CT-ARCHERR                          TO MSGO
 000501
 000502            END-EVALUATE
 000503
 000504            MOVE 0                                     TO WS-FLAG-PF6
 000505            MOVE SPACES                                TO WS-COM-TIPD
 000506            MOVE ZEROS                                 TO WS-COM-NROD
 000507
 000508            EXEC CICS
 000509                 SEND MAP  (WS-MAP)
 000510                 MAPSET    (WS-MAPSET)
 000511                 FROM      (MAP0101O)
 000512                 LENGTH    (WS-LONG)
 000513                 ERASE
 000514            END-EXEC
 000515            .
 000516        3600-F-DELETE.
 000517            EXIT.
 000518
 000519        3700-I-CANCEL.
 000520
 000521            INITIALIZE MAP0101O
 000522            MOVE WS-FECHA                            TO FECHAO
 000523            MOVE CT-CANCEL                           TO MSGO
 000524
 000525            EXEC CICS
 000526                 SEND MAP  (WS-MAP)
 000527                 MAPSET    (WS-MAPSET)
 000528                 FROM      (MAP0101O)
 000529                 LENGTH    (WS-LONG)
 000530                 ERASE
 000531            END-EXEC
 000532            .
 000533        3700-F-CANCEL.
 000534            EXIT.
 000535
 000536        3800-I-PF7.
 000537              MOVE TIPDOCI                             TO WS-COM-TIPD
 000538              MOVE NUMDOCI                             TO WS-COM-NROD
 000539              MOVE CT-COMPLETE                         TO MSGO
 000540              MOVE DFHBMUNP                            TO NROCLIA
 000541              MOVE DFHBMUNP                            TO NOMAPEA
 000542              MOVE DFHBMUNP                            TO DIRECA
 000543              MOVE DFHBMUNP                            TO TELA
 000544              MOVE DFHBMUNP                            TO EMAILA
 000545              MOVE 1                                   TO WS-FLAG-PF7
 000546
 000547            EXEC CICS
 000548                 SEND MAP  (WS-MAP)
 000549                 MAPSET    (WS-MAPSET)
 000550                 FROM      (MAP0101O)
 000551                 LENGTH    (WS-LONG)
 000552                 ERASE
 000553                 CURSOR    (+00670)
 000554            END-EXEC
 000555            .
 000556        3800-F-PF7.
 000557            EXIT.
 000558
 000559        3850-I-CREATE.
 000560            MOVE WS-COM-TIPD                           TO WS-TIPO-DOC
 000561
 000562              IF NOT WS-TIP-DOC
 000563                INITIALIZE MAP0101O
 000564                MOVE WS-FECHA                          TO FECHAO
 000565                MOVE CT-ERRDOC                         TO MSGO
 000566                MOVE 0                                 TO WS-FLAG-PF7
 000567              ELSE
 000568                IF WS-COM-NROD IS NOT NUMERIC
 000569                  INITIALIZE MAP0101O
 000570                  MOVE WS-FECHA                        TO FECHAO
 000571                  MOVE CT-ERRNROD                      TO MSGO
 000572                  MOVE 0                               TO WS-FLAG-PF7
 000573                ELSE
 000574
 000575                  INITIALIZE REG-PERSONA
 000576
 000577                  MOVE WS-COM-TIPD                     TO PER-TIP-DOC
 000578                  MOVE WS-COM-NROD                     TO PER-NRO-DOC
 000579                  MOVE NROCLII                         TO PER-CLI-NRO
 000580                  MOVE NOMAPEI                         TO PER-NOMAPE
 000581                  MOVE DIRECI                          TO PER-DIRECCION
 000582                  MOVE TELI                            TO PER-TELEFONO
 000583                  MOVE EMAILI                          TO PER-EMAIL
 000584
 000585                  MOVE WS-COM-TIPD                     TO WS-RID-TIPD
 000586                  MOVE WS-COM-NROD                     TO WS-RID-NROD
 000587
 000588                  EXEC CICS
 000589                       WRITE DATASET (CT-DATASET)
 000590                       RIDFLD        (WS-RIDFLD)
 000591                       FROM          (REG-PERSONA)
 000592                       LENGTH        (WS-LENGTH-PERSO)
 000593                       RESP          (WS-RESP)
 000594                  END-EXEC
 000595
 000596                  EVALUATE WS-RESP
 000597                  WHEN DFHRESP(DUPREC)
 000598
 000599                    INITIALIZE MAP0101O
 000600                    MOVE WS-FECHA                      TO FECHAO
 000601                    MOVE CT-REGDUP                     TO MSGO
 000602
 000603                  WHEN OTHER
 000604
 000605                    INITIALIZE MAP0101O
 000606                    MOVE WS-FECHA                      TO FECHAO
 000607                    MOVE CT-EXITO                      TO MSGO
 000608
 000609                  END-EVALUATE
 000610
 000611                END-IF
 000612              END-IF
 000613
 000614            MOVE 0                                     TO WS-FLAG-PF7
 000615            MOVE WS-FECHA                              TO FECHAO
 000616            MOVE SPACES                                TO WS-COM-TIPD
 000617            MOVE ZEROS                                 TO WS-COM-NROD
 000618
 000619            EXEC CICS
 000620                 SEND MAP  (WS-MAP)
 000621                 MAPSET    (WS-MAPSET)
 000622                 FROM      (MAP0101O)
 000623                 LENGTH    (WS-LONG)
 000624                 ERASE
 000625            END-EXEC
 000626            .
 000627        3850-F-CREATE.
 000628            EXIT.
 000629
 000630        9999-I-FINAL.
 000631            EXEC CICS
 000632                 RETURN
 000633                 TRANSID  ('T101')
 000634                 COMMAREA (WS-COMMAREA)
 000635            END-EXEC
 000636            .
 000637        9999-F-FINAL.
 000638            EXIT.
 000639
 ****** **************************** Bottom of Data ****************************

