TIUFLLM2 ; SLC/MAM - Library; LM Related: BUFENTRY(INFO,NODE0,CONTENT,PFILEDA), CONTENT params: 80,N,H,B,I,W ;4/28/97  21:21
 ;;1.0;TEXT INTEGRATION UTILITIES;**6,43**;Jun 20, 1997
 ;
BUFENTRY(INFO,NODE0,CONTENT,PFILEDA) ;Get data for FILEDA; set Docmt
 ;Def entry INFO("FILEDA") into buffer array, inserting it at lineno
 ;+INFO with Lineno +INFO.
 ; This module adds ONE line to buffer array and is called multiple
 ;times to complete buffer array.  When finished, buffer array is
 ;inserted AS IS into LM array ^TMP("TIUF1/2/or3" for LM Sub/Template
 ;TIUFTMPL/TIUFSTMP (unless CONTENT contains W, in which case Buffer
 ;Array is written to screen.)
 ; Buffer array does NOT necessarily start with line 1, but with line
 ;0 if CONTENT contains W, or with number appropriate for where it will
 ;be inserted into LM array.
 ;
 ; Requires INFO array, where INFO is either as set in NINFO^TIUFLLM if
 ;entry is not yet in LM array, or is = ^TMP("TIUF*IDX,$J,LINENO) if
 ;entry is already in LM array, and where INFO array is as set in
 ;PARSE^TIUFLLM(INFO). FILEDA = INFO("FILEDA"), is entry in 8925.1.
 ; Requires Array NODE0, set as TIUFNOD0 in NODE0ARR^TIUFLF.  NODE0 is
 ;enhanced 0 node of FILEDA.
 ; Requires CONTENT = String of flags defining what information is set
 ;into buffer array for FILEDA: parameters allowed: 80, H, C, A, J, D, O,
 ;T, W.  For more info on CONTENT see rtn TIUFLLM3.
 ; Requires PFILEDA if CONTENT contains  D, or T, where
 ;PFILEDA=FILEDA of (anticipated) parent of entry.
 ; Requires TIUFWHO, set in Options TIUF/A/C/H EDIT/SORT/CREATE DDEFS CLIN/MGR/NATL.
 ; Can't use VALMDDEF since called by nontarget Template.
 ;
 N DIC,DIQ,DA,DR,MNEM,MENUTEXT,SEQUENCE,NUMBER,NAME,TIUREC,IFN
 N TYPE,LP,LC,OWNER,TIUREC,STATUS,DDEFUSED,INUSE
 N MNWIDTH,RESTCOL,NMCOLUMN,TIUFI,FILEDA,BOILPT,SHARE,POWNER,COWNER,ITEMS
 N CONT80,CONTH,CONTC,CONTA,CONTJ,CONTD,CONTT,CONTW,CONTO
 N PRINTNM,NATIONAL,FRACTION,BLANKS
 ; If already had LM array node for entry and saved it (nothing changed except lineno) then put IT in buffer instead of getting all data again:
 I $D(TIUFSAVE(INFO("FILEDA"))) D  Q
 . S TIUREC=TIUFSAVE(INFO("FILEDA")) S BLANKS=$L(+INFO)-$L(+TIUFSAVE(INFO("FILEDA")))
 . S TIUREC=+INFO_$S(BLANKS=2:"",BLANKS:" ",1:"  ")_$E(TIUREC,$L(+TIUFSAVE(INFO("FILEDA")))+3,500)
 . S ^TMP("TIUFB",$J,+INFO,0)=TIUREC
 . S ^TMP("TIUFBIDX",$J,+INFO)=INFO
 S CONTH=$S(CONTENT["H":1,1:0),CONTC=$S(CONTENT["C":1,1:0),CONTA=$S(CONTENT["A":1,1:0)
 S CONTJ=$S(CONTENT["J":1,1:0),CONTD=$S(CONTENT["D":1,1:0),CONTT=$S(CONTENT["T":1,1:0),CONTW=$S(CONTENT["W":1,1:0),CONT80=$S(+CONTENT:1,1:0),CONTO=$S(CONTENT["O":1,1:0)
NUM ; Set Entry number
 I CONTH!CONTA!CONTC!CONTJ!CONTT S NUMBER=$S(+INFO:+INFO,1:""),TIUREC=$$SETSTR^VALM1(NUMBER,"",1,5),NMCOLUMN=8
 E  S TIUREC="",NMCOLUMN=2
NAME ; Set DDEF Name, Type into TIUREC:
 S NAME=$P(NODE0,U),TYPE=$P(NODE0,U,4) S:TYPE="DOC" TYPE="TL"
 I CONTH!CONTC S NAME=$J(NAME,$L(NAME)+(2*(INFO("LEVEL"))))
 S TIUREC=$$SETSTR^VALM1(NAME,TIUREC,NMCOLUMN,$S(CONTD:37,CONTO:55,1:67))
 I CONTH!CONTA!CONTC D
 . I TYPE'="",NODE0("SHARE")="Yes" S TYPE=$E(TYPE,1,2)_" S"
 . S TIUREC=$$SETSTR^VALM1(TYPE,TIUREC,77,4) Q
 I CONTJ S STATUS=$E(NODE0("STATUS")),STATUS=$J(STATUS,6) S TIUREC=$$SETSTR^VALM1(STATUS,TIUREC,75,6)
 ;
 I $D(PFILEDA),CONTD!CONTT D
 . K TIUFQ S DIC=8925.1,DA=PFILEDA,DIQ="TIUFQ",DR="10",DA(8925.14)=INFO("TENDA"),DR(8925.14)=".01:4" D EN^DIQ1
 . S MNEM=TIUFQ(8925.14,INFO("TENDA"),2),MNEM=$J(MNEM,4)
 . I TYPE'="CL",(TYPE'="DC") S MNEM="    "
 . S MENUTEXT=TIUFQ(8925.14,INFO("TENDA"),4),SEQUENCE=TIUFQ(8925.14,INFO("TENDA"),3),FRACTION=$P(SEQUENCE,".",2),SEQUENCE=$S(FRACTION="":SEQUENCE_"   ",$L(FRACTION)=1:SEQUENCE_" ",1:SEQUENCE_""),SEQUENCE=$J(SEQUENCE,6)
 . K TIUFQ
 I CONTT,SEQUENCE'="" S TIUREC=$$SETSTR^VALM1(SEQUENCE,TIUREC,75,6)
 ;
 S FILEDA=INFO("FILEDA")
GET ; Get/Set the rest of the line:
 S IFN=FILEDA,IFN=$J(IFN,7)
 I CONT80!CONTH!CONTA!CONTC!CONTJ!CONTW D
 . S NATIONAL=NODE0("NATL"),NATIONAL=$J(NATIONAL,4)
 . S:TIUFTMPL'["J" INUSE=NODE0("INUSE"),INUSE=$J(INUSE,6)
 . I TIUFTMPL'["J" S STATUS=$E(NODE0("STATUS")),STATUS=$J(STATUS,6) I $P(NODE0,U,10) S STATUS="      "
 . S:TIUFTMPL'["J" BOILPT=NODE0("BOILPT"),BOILPT=$J(BOILPT,4)
 . S:TIUFTMPL'["J" ITEMS=NODE0("ITEMS"),ITEMS=$J(ITEMS,5)
 . S PRINTNM=$P(NODE0,U,3)
 I CONT80!CONTH!CONTA!CONTC!CONTJ!CONTW!CONTO D
 . S POWNER=$P(NODE0,U,5),LP=$L(POWNER)
 . S COWNER=$P(NODE0,U,6),LC=$L(COWNER)
 . I LP D
 . . S POWNER=$G(^VA(200,POWNER,0))
 . . I POWNER="" S POWNER="ERROR" Q
 . . S POWNER=$S($L($P(POWNER,U,2)):$P(POWNER,U,2),1:$E($P(POWNER,U),1,7))
 . I LC D
 . . S COWNER=$$USRCLASS^USRLFF(COWNER) ; **43**
 . . I COWNER="" S COWNER="ERROR" Q
 . . S COWNER=$S($L($P(COWNER,U,2)):$P(COWNER,U,2),1:$E($P(COWNER,U),1,7))
 . S OWNER=$S(LP&'LC:POWNER,LC&'LP:COWNER,LP&LC:$E(POWNER,1,3)_","_$E(COWNER,1,4),1:"")
SET S RESTCOL=$S(CONT80:45,"NM"[TIUFWHO&CONTD:30,CONTD:41,CONTO:57,"C"[TIUFWHO&CONTT:91,1:82)
 I CONT80 D  G PLUS
 . S TIUREC=$$SETSTR^VALM1(" "_TYPE,TIUREC,RESTCOL-1,5),RESTCOL=RESTCOL+6 ;RESTCOL=RESTCOL+LASTWIDTH +2
 . S TIUREC=$$SETSTR^VALM1("  "_IFN,TIUREC,RESTCOL-2,9),RESTCOL=RESTCOL+9
 . S TIUREC=$$SETSTR^VALM1("  "_NATIONAL,TIUREC,RESTCOL-2,6),RESTCOL=RESTCOL+6
 . S TIUREC=$$SETSTR^VALM1("  "_STATUS,TIUREC,RESTCOL-2,8),RESTCOL=RESTCOL+8
 . S TIUREC=$$SETSTR^VALM1("  "_OWNER,TIUREC,RESTCOL-2,9),RESTCOL=RESTCOL+9
 I CONTO D
 . S STATUS=$E(NODE0("STATUS")),STATUS=$J(STATUS,6) S TIUREC=$$SETSTR^VALM1(STATUS,TIUREC,RESTCOL,6),RESTCOL=RESTCOL+8
 . S TIUREC=$$SETSTR^VALM1(OWNER,TIUREC,RESTCOL,7),RESTCOL=RESTCOL+9
 I "NM"[TIUFWHO D
 . I CONTD S TIUREC=$$SETSTR^VALM1("  ",TIUREC,RESTCOL,2),RESTCOL=RESTCOL+2
 . S TIUREC=$$SETSTR^VALM1(IFN,TIUREC,RESTCOL,7),RESTCOL=RESTCOL+9 ;RESTCOL=RESTCOL+LASTWIDTH +2
 I CONTH!CONTA!CONTC!CONTJ!CONTW D  G PLUS
 . S TIUREC=$$SETSTR^VALM1(NATIONAL,TIUREC,RESTCOL,4),RESTCOL=RESTCOL+6
 . S:TIUFTMPL'["J" TIUREC=$$SETSTR^VALM1(STATUS,TIUREC,RESTCOL,6),RESTCOL=RESTCOL+8
 . S TIUREC=$$SETSTR^VALM1(OWNER,TIUREC,RESTCOL,7),RESTCOL=RESTCOL+9
 . S:TIUFTMPL'["J" TIUREC=$$SETSTR^VALM1($G(INUSE),TIUREC,RESTCOL,6),RESTCOL=RESTCOL+8
 . S:TIUFTMPL'["J" TIUREC=$$SETSTR^VALM1(BOILPT,TIUREC,RESTCOL,4),RESTCOL=RESTCOL+6
 . I CONTA S TIUREC=$$SETSTR^VALM1(ITEMS,TIUREC,RESTCOL,5),RESTCOL=RESTCOL+7
 . S TIUREC=$$SETSTR^VALM1(PRINTNM,TIUREC,RESTCOL,60)
 I CONTD!CONTT D
 . S:CONTD TIUREC=$$SETSTR^VALM1(SEQUENCE,TIUREC,RESTCOL,6),RESTCOL=RESTCOL+8
 . S TIUREC=$$SETSTR^VALM1(MNEM,TIUREC,RESTCOL,4),RESTCOL=RESTCOL+6
 . S TIUREC=$$SETSTR^VALM1(MENUTEXT,TIUREC,RESTCOL,26)
PLUS I CONTH S TIUREC=$$PLUSUP^TIUFLLM(.INFO,TIUREC)
 I 'CONT80,'CONTO,'CONTD,'CONTT D RTSCROLL^TIUFLLM(.TIUREC,TYPE)
 S ^TMP("TIUFB",$J,+INFO,0)=TIUREC
 I 'CONTW D
 . S ^TMP("TIUFBIDX",$J,+INFO)=INFO
BUFEX Q
 ;
