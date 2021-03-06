FSCRPCGT ;SLC/STAFF-NOIS RPC Test Package Installs ;1/11/98  15:25
 ;;1.1;NOIS;;Sep 06, 1998
 ;
SITE(SITE) ; from FSCRPCG
 N CNT,DATE,LINE,LINE1,NUM,OK,PACK,PACKNM,SITENUM,VER K ^TMP("FSC TEST",$J)
 S SITENUM=+$$STSITE^FSCRPCOC(+$G(SITE)) I 'SITENUM Q
 S CNT=0
 S NUM=0 F  S NUM=$O(^NTS(2050,SITENUM,8,NUM)) Q:NUM<1  S LINE=$G(^(NUM,0)) D
 .S PACK=$$STPACK(NUM) I 'PACK Q
 .S VER=$P(LINE,U,7)
 .S OK=0
 .I VER["T" S OK=1
 .I VER["V" S OK=1
 .I 'OK Q
 .S DATE=$P(LINE,U,8),DATE=$$FMTE^XLFDT(DATE)
 .S PACKNM=$P(^FSC("PACK",PACK,0),U)
 .S LINE1=$$SETSTR^VALM1(VER,PACKNM,30,$L(VER))
 .S LINE1=$$SETSTR^VALM1(DATE,LINE1,37,$L(DATE))
 .S CNT=CNT+1,^TMP("FSC TEST",$J,PACKNM,CNT)=LINE1
 I 'CNT S ^TMP("FSCRPC",$J,"OUTPUT",1)="   No test versions of packages installed." Q
 S CNT=0,PACKNM="" F  S PACKNM=$O(^TMP("FSC TEST",$J,PACKNM)) Q:PACKNM=""  D
 .S NUM=0 F  S NUM=$O(^TMP("FSC TEST",$J,PACKNM,NUM)) Q:NUM<1  S LINE=^(NUM) D
 ..S CNT=CNT+1,^TMP("FSCRPC",$J,"OUTPUT",CNT)=LINE
 K ^TMP("FSC TEST",$J)
 Q
 ;
MOD(MODULE) ; from  FSCRPCG
 N CNT,DATE,LINE,NUM,OK,PACKAGE,SITENM,SITENUM,VER K ^TMP("FSC TEST",$J)
 S PACKAGE=+$P($G(^FSC("MOD",+$G(MODULE),0)),U,8) I 'PACKAGE Q
 S PACKAGE=$$STPACK^FSCRPCGN(PACKAGE) I 'PACKAGE Q
 S CNT=0
 S SITENUM=0 F  S SITENUM=$O(^NTS(2050,SITENUM)) Q:SITENUM<1  D
 .S LINE=$G(^NTS(2050,SITENUM,8,PACKAGE,0)) I '$L(LINE) Q
 .S VER=$P(LINE,U,7)
 .S OK=0
 .I VER["T" S OK=1
 .I VER["V" S OK=1
 .I 'OK Q
 .S DATE=$P(LINE,U,8),DATE=$$FMTE^XLFDT(DATE)
 .S SITENM=$$STSITE(SITENUM) I 'SITENM Q
 .S SITENM=$P($G(^FSC("SITE",SITENM,0)),U) I '$L(SITENM) Q
 .S LINE=$$SETSTR^VALM1(VER,SITENM,30,$L(VER))
 .S LINE=$$SETSTR^VALM1(DATE,LINE,37,$L(DATE))
 .S CNT=CNT+1,^TMP("FSC TEST",$J,SITENM,CNT)=LINE
 I 'CNT S ^TMP("FSCRPC",$J,"OUTPUT",1)="   No sites with test versions installed." Q
 S CNT=0,SITENM="" F  S SITENM=$O(^TMP("FSC TEST",$J,SITENM)) Q:SITENM=""  D
 .S NUM=0 F  S NUM=$O(^TMP("FSC TEST",$J,SITENM,NUM)) Q:NUM<1  S LINE=^(NUM) D
 ..S CNT=CNT+1,^TMP("FSCRPC",$J,"OUTPUT",CNT)=LINE
 K ^TMP("FSC TEST",$J)
 Q
 ;
STPACK(STPACK) ; $$(site tracking package) -> site
 N OPACK,PACK
 S OPACK=+$P($G(^DIC(120102,+$G(STPACK),0)),U,8) I 'STPACK Q ""
 S PACK=+$O(^FSC("PACK","AC",OPACK,0)) I 'PACK Q ""
 Q PACK
 ;
STSITE(STSITE) ; $$(site tracking site) -> site
 N DOMAIN,SITE
 S DOMAIN=$P($G(^NTS(2050,+$G(STSITE),22)),U) I '$L(DOMAIN) Q ""
 S DOMAIN=+$O(^DIC(4.2,"B",DOMAIN,0)) I 'DOMAIN Q ""
 S SITE=+$O(^FSC("SITE","AE",DOMAIN,0)) I 'SITE Q ""
 Q SITE
