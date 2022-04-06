FUNCTION ztablomailgonder.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_TABLE) TYPE  CHAR30 OPTIONAL
*"     REFERENCE(IV_HEADER) TYPE  CHAR100 OPTIONAL
*"     REFERENCE(IV_MAIL) TYPE  SOMLREC90 OPTIONAL
*"     REFERENCE(IV_MESSAGE) TYPE  CHAR200 OPTIONAL
*"     REFERENCE(IV_WHERE) TYPE  STRING OPTIONAL
*"     REFERENCE(IV_COLOUM) TYPE  STRING OPTIONAL
*"     REFERENCE(IT_TABLE) TYPE REF TO  DATA OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_RESULT) TYPE  CHAR50
*"----------------------------------------------------------------------
  TYPE-POOLS : abap.
  DATA:gv_tabname TYPE char30,
       gv_column  TYPE char200,
       gv_header  TYPE char100,
       gv_mail    TYPE somlrec90,
       gv_result  TYPE char50,
       gv_message TYPE char200,
       gv_where   TYPE string,
       gv_dynamic TYPE string,
       value TYPE string,
       p_tab_ref  TYPE REF TO data,
       wa_tab     TYPE REF TO data.

  DATA: BEGIN OF email_str,
      email TYPE somlrec90,
      END OF email_str.
  DATA: BEGIN OF col_str,
      colname TYPE char30,
      END OF col_str.
  DATA: BEGIN OF where_str,
      wherename TYPE char30,
      END OF where_str.
  DATA: BEGIN OF like_str,
      likename TYPE char30,
      END OF like_str.

  DATA: gt_email LIKE email_str OCCURS 0 WITH HEADER LINE,
        gs_email LIKE email_str.

  DATA: gt_col LIKE col_str OCCURS 0 WITH HEADER LINE,
        gs_col LIKE col_str.

  DATA: gt_like LIKE like_str OCCURS 0 WITH HEADER LINE,
        gs_like LIKE like_str.

  DATA: email_tab TYPE str OCCURS 0 WITH HEADER LINE,
        email_tab_s TYPE str.

  DATA: gt_where LIKE where_str OCCURS 0 WITH HEADER LINE,
        gs_where LIKE LINE OF gt_where.

*** --- Field Symbols
  FIELD-SYMBOLS : <fs_table> TYPE ANY.
  FIELD-SYMBOLS : <fs_value> TYPE ANY.
  FIELD-SYMBOLS : <ft_table> TYPE STANDARD TABLE.

  gv_header = iv_header.
  gv_column = iv_coloum.
  gv_where = iv_where.
  gv_tabname = iv_table.
  gv_mail = iv_mail.
  gv_message = iv_message.

  CREATE DATA p_tab_ref TYPE TABLE OF (gv_tabname).
  ASSIGN p_tab_ref->* TO <ft_table>.
  ASSIGN p_tab_ref->* TO <fs_table>.

  SELECT (gv_column) FROM (gv_tabname) INTO CORRESPONDING FIELDS OF TABLE <ft_table> WHERE (gv_where).

  DATA: maildata  TYPE sodocchgi1,
        mailtxt   TYPE TABLE OF solisti1 WITH HEADER LINE,
        mailrec   TYPE TABLE OF somlrec90 WITH HEADER LINE,
        it_pack   TYPE STANDARD TABLE OF sopcklsti1 WITH HEADER LINE,
        tab_lines TYPE i.
  SPLIT gv_column AT ' ' INTO TABLE gt_col.
  maildata-obj_name = gv_header.
  maildata-obj_langu = sy-langu.
  maildata-no_change = 'X'.

  mailtxt-line = '<html><head><style>table,td,th {margin: 5px; padding: 5px;border: 1px solid; border-color: black; text-align:center}table {  width: auto;  border-collapse: collapse;}</style> </head>'.
  APPEND mailtxt.
  mailtxt-line = '<body style="background-color:#FFFFFF;"><basefont face="arial, verdana, courier" size="2">'.
  APPEND mailtxt.
  mailtxt-line = '<table><tr>'.
  APPEND mailtxt.
  LOOP AT gt_col INTO gs_col.
    CONCATENATE '<th width="auto">' gs_col-colname '</th>' INTO mailtxt.
    APPEND mailtxt.
  ENDLOOP.

  mailtxt = '</tr><p><tr>'.
  APPEND mailtxt.
  LOOP AT <ft_table> ASSIGNING <fs_table>.
    LOOP AT gt_col INTO gs_col.
      ASSIGN COMPONENT gs_col-colname OF STRUCTURE <fs_table> TO <fs_value>.
      value = <fs_value>.
      CONCATENATE '<td width="auto">' value '</td>' INTO mailtxt.
      CLEAR value.
      APPEND mailtxt.
    ENDLOOP.
    mailtxt-line = '</tr>'.
    APPEND mailtxt.
  ENDLOOP.
  mailtxt-line = '</table></body></html>'.
  APPEND mailtxt.
  mailrec-receiver = gv_mail.
  mailrec-rec_type = 'U'.
  APPEND mailrec.

  DESCRIBE TABLE mailtxt LINES tab_lines.

  it_pack-head_start = 1.
  it_pack-head_num = 0.
  it_pack-body_start = 1.
  it_pack-body_num = tab_lines.
  it_pack-doc_type = 'HTML'.
  APPEND it_pack.

  IF NOT mailtxt IS INITIAL .
    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
        document_data = maildata
*        document_type = 'RAW'
        put_in_outbox = 'X'
        commit_work   = 'X'
      TABLES
        packing_list  = it_pack
        contents_txt  = mailtxt
        receivers     = mailrec.
    SUBMIT rsconn01 WITH mode = 'INT' AND RETURN.

    MESSAGE 'Mail gönderimi başarılı.' TYPE 'I'.
  ENDIF.
ENDFUNCTION.
