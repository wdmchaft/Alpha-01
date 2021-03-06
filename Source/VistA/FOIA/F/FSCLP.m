FSCLP ;SLC/STAFF-NOIS List Process ;1/13/98  13:18
 ;;1.1;NOIS;;Sep 06, 1998
 ;
UPDATE(CALL,LIST) ; from FSCLMPD, FSCTASKA, FSCTASKU
 I $G(LIST),$P($G(^FSC("LIST",LIST,0)),U,3)'="A" Q
 I $G(LIST),$G(CALL) D PROCESS(LIST,CALL) Q
 I '$G(LIST),$G(CALL) D CALL(CALL) Q
 I $G(LIST),'$G(CALL) D LIST(LIST) Q
 I '$G(LIST),'$G(CALL) D  Q
 .N DAYDATE,DOW,REBUILD
 .S DOW=$$DOW^XLFDT(DT)
 .S DAYDATE=+$E(DT,6,7)
 .S LIST=0 F  S LIST=$O(^FSC("LIST","AU","A",LIST)) Q:LIST<1  D
 ..S REBUILD=$P($G(^FSC("LIST",LIST,0)),U,10)
 ..I REBUILD="NEVER" Q
 ..I REBUILD="DAILY" D LIST(LIST) Q
 ..I REBUILD="WEEKLY",DOW="Saturday" D LIST(LIST) Q
 ..I REBUILD="",DOW="Saturday" D LIST(LIST) Q
 ..I REBUILD="MONTHLY",DOW="Saturday",DAYDATE<8 D LIST(LIST) Q
 Q
 ;
CALL(CALLNUM) ;
 N ADD,EVAL,EXP,LEVEL,LIST,LNUM,NUM,Q,VALUE,X K Q,VALUE,X
 D GET^FSCGET("ALL",CALLNUM,.VALUE)
 S LIST=0 F  S LIST=$O(^FSC("LIST","AU","A",LIST)) Q:LIST<1  D
 .L +^XTMP("FSC LIST DEF",LIST):20 I '$T Q
 .I '$D(^XTMP("FSC LIST DEF",LIST,"XOP")) K ^XTMP("FSC LIST DEF",LIST)
 .I '$D(^XTMP("FSC LIST DEF",LIST)) D BUILD^FSCLDU(LIST)
 .S NUM=0 F  S NUM=$O(^XTMP("FSC LIST DEF",LIST,"Q",NUM)) Q:NUM<1  S EXP=^(NUM) S Q(NUM)=0 I @EXP S Q(NUM)=1
 .S LEVEL=0 F  S LEVEL=$O(^XTMP("FSC LIST DEF",LIST,"X",LEVEL)) Q:LEVEL<1  S EXP=$P(^(LEVEL),U,2) S X(LEVEL)=0 I @EXP S X(LEVEL)=1
 .S EVAL=^XTMP("FSC LIST DEF",LIST,"XOP")
 .L -^XTMP("FSC LIST DEF",LIST)
 .I @EVAL D  Q
 ..S ADD=0 D ADD^FSCLMPS(CALLNUM,LIST,.OK) I OK S ADD=1
 ..D NOTIFY(CALLNUM,LIST,ADD)
 .S LNUM=+$O(^FSCD("LISTS","ALC",LIST,CALLNUM,0)) I LNUM D DELETE^FSCLMPS(LNUM)
 Q
 ;
LIST(LIST) ;
 N ADD,CALL,CNT,CRITERIA,LNUM,LSTART,LSTOP,OPNUM K CRITERIA,^TMP("FSC LIST",$J)
 S LSTART=$$NOW^XLFDT
 D TMP(LIST)
 S (CNT,OPNUM)=0 F  S OPNUM=$O(^TMP("FSC LIST DEF",$J,LIST,"CRITERIA",OPNUM)) Q:OPNUM<1  D
 .K CRITERIA M CRITERIA=^TMP("FSC LIST DEF",$J,LIST,"CRITERIA",OPNUM)
 .D QUERY^FSCQR("",.CNT,.CRITERIA)
 K ^TMP("FSC LIST DEF",$J)
 S CALL=0 F  S CALL=$O(^FSCD("LISTS","ALC",LIST,CALL)) Q:CALL<1  D
 .I '$D(^TMP("FSC LIST",$J,CALL)) S LNUM=+$O(^FSCD("LISTS","ALC",LIST,CALL,0)) D:LNUM DELETE^FSCLMPS(LNUM) Q
 .K ^TMP("FSC LIST",$J,CALL)
 S CALL=0 F  S CALL=$O(^TMP("FSC LIST",$J,CALL)) Q:CALL<1  D
 .S ADD=0 D ADD^FSCLMPS(CALL,LIST,.OK) I OK S ADD=1
 .D NOTIFY(CALL,LIST,ADD)
 S LSTOP=$$NOW^XLFDT
 S $P(^FSC("LIST",LIST,0),U,11)=$$FMDIFF^XLFDT(LSTOP,LSTART,2)
 Q
 ;
PROCESS(LISTNUM,CALLNUM) ;
 I '$D(^FSC("LIST",LISTNUM)) Q
 D TMP(LISTNUM)
 N ADD,EVAL,EXP,FIELD,LEVEL,LNUM,NUM,Q,VALUE,X K Q,VALUE,X
 S FIELD=0 F  S FIELD=$O(^TMP("FSC LIST DEF",$J,LISTNUM,"VAR",FIELD)) Q:FIELD<1  S VALUE(^(FIELD))=""
 D GET^FSCGET("CUSTOM",CALLNUM,.VALUE)
 S NUM=0 F  S NUM=$O(^TMP("FSC LIST DEF",$J,LISTNUM,"Q",NUM)) Q:NUM<1  S EXP=^(NUM) S Q(NUM)=0 I @EXP S Q(NUM)=1
 S LEVEL=0 F  S LEVEL=$O(^TMP("FSC LIST DEF",$J,LISTNUM,"X",LEVEL)) Q:LEVEL<1  S EXP=$P(^(LEVEL),U,2) S X(LEVEL)=0 I @EXP S X(LEVEL)=1
 S EVAL=^TMP("FSC LIST DEF",$J,LISTNUM,"XOP")
 K ^TMP("FSC LIST DEF",$J)
 I @EVAL D  Q
 .S ADD=0 D ADD^FSCLMPS(CALLNUM,LISTNUM,.OK) I OK S ADD=1
 .D NOTIFY(CALLNUM,LISTNUM,ADD)
 S LNUM=+$O(^FSCD("LISTS","ALC",LISTNUM,CALLNUM,0)) I LNUM D DELETE^FSCLMPS(LNUM)
 Q
 ;
NOTIFY(CALL,LIST,ADD) ;
 I '$L($P(^FSC("LIST",LIST,0),U,6)) Q
 I $D(^FSCD("NOTIFY","ACLIST",CALL,LIST)) Q
 I 'ADD,$P(^FSC("LIST",LIST,0),U,7)="ADDED" Q
 D SETUP^FSCNOT(CALL,LIST)
 Q
 ;
MANUAL(LIST) ; from FSCLML, FSCLMPQU, FSCRPCA, FSCRPCL
 N CNT,OPNUM,CRITERIA K CRITERIA,^TMP("FSC LIST",$J)
 D TMP(LIST)
 S (CNT,OPNUM)=0 F  S OPNUM=$O(^TMP("FSC LIST DEF",$J,LIST,"CRITERIA",OPNUM)) Q:OPNUM<1  D
 .K CRITERIA M CRITERIA=^TMP("FSC LIST DEF",$J,LIST,"CRITERIA",OPNUM)
 .D QUERY^FSCQR("",.CNT,.CRITERIA)
 K ^TMP("FSC LIST DEF",$J)
 Q
 ;
TMP(LIST) ; builds ^TMP("FSC LIST DEF",$J,LIST) from ^XTMP
 K ^TMP("FSC LIST DEF",$J,LIST)
 L +^XTMP("FSC LIST DEF",LIST):20 I '$T Q
 I '$D(^XTMP("FSC LIST DEF",LIST,"XOP")) K ^XTMP("FSC LIST DEF",LIST)
 I '$D(^XTMP("FSC LIST DEF",LIST)) D BUILD^FSCLDU(LIST)
 M ^TMP("FSC LIST DEF",$J,LIST)=^XTMP("FSC LIST DEF",LIST)
 L -^XTMP("FSC LIST DEF",LIST)
 Q
