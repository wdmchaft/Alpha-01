DDBRZIS ;SFISC/DCL-BROWSER DEVICE UTILITIES ;4:50 AM  9 Apr 2006
 ;;22.0;VA FileMan;**145,148**;Mar 30, 1999
 ;Per VHA Directive 10-93-142, this routine should not be modified.
OPEN ;
 ;DDBRZIS AND DDBDMSG ARE KILLED IN POST
 S DDBRZIS=1,DDBDMSG=$G(DDBDMSG)
 U IO(0)
 I $G(DDBDMSG)="" D  Q:DDBDMSG="$$DTOUT$$"
 .N DIR,X,Y
 .S DIR(0)="FUO^0:78",DIR("A")="BROWSER TITLE (optional)"
 .S DIR("B")="VA FileMan Browser"
 .S DIR("?")="Enter any free text, which will appear in the Title Bar"
 .D ^DIR
 .I $G(DTOUT) S DDBDMSG="$$DTOUT$$" K DTOUT,DUOUT,DIRUT,DIROUT Q
 .S DDBDMSG=$S(Y="":DDBDMSG,1:Y)
 .Q
 W !,"...one moment..."
 U IO
 Q:DDBDMSG]""
 I $G(DHD)="W """" D ^DIDH" S DDBDMSG="DATA DICTIONARY" Q
 S DDBDMSG="VA FileMan Browser"
 Q
 ;
CLOSE ;
 Q:$G(DDBDMSG)="$$DTOUT$$"
 S DDBRZIS=$G(DDBRZIS,1)
 N C,CHAR,DDBROS,EOF,X
 K ^TMP("DDB",$J)
 S DDBROS=^%ZOSF("OS"),EOF="EOF-End Of File"
 S CHAR="" F I=1:1:31 S CHAR=CHAR_$C(I)
 U IO W !,EOF,!
 S DDBRZIS("REWIND")=$$REWIND^%ZIS(IO,IOT,IOPAR)
 I 'DDBRZIS("REWIND") S DDBRZIS=0 U IO(0) W $C(7),!!?5,"<< UNABLE TO REWIND FILE>>",! H 3 Q
 U IO
 S C=0
 F  R X:2 Q:X="EOF-End Of File"  D
 .S X=$TR(X,CHAR)
 .S:X']"" X=" "
 .S C=C+1,^TMP("DDB",$J,C)=$E(X,1,255) Q
 .Q
 Q
 ;
POST ;
 I $G(DDBDMSG)="$$DTOUT$$" K DDBDMSG,DDBRZIS W $C(7) Q
 I $G(DDBRZIS) D BROWSE^DDBR("^TMP(""DDB"",$J)","NR",$G(DDBDMSG))
 ; *148* Moved to POST-CLOSE EXECUTE of BROWSER device
 ;I ^%ZOSF("OS")["OpenM",$G(IO("CLOSE"))]"" S DDBRZIS=$ZF(-1,"del "_ IO("CLOSE")_";*")
 K DDBRZIS,DDBDMSG
 Q
 ;
DEVICE(MSG) ;TEST IF BROWSER IS BEING INVOKED VIA DEVICE HANDLER
 ;EXTRINSIC FUNCTION
 I $D(DDBRZIS)#2,$G(MSG)]"" S DDBDMSG=MSG Q 1
 Q 0
 ;
MSG(TXT) ;PASS TEXT FOR BROWSER TITLE WHEN BROWSER INVOKED VIA DEVICE HANDLER
 ;PROCEDURE CALL
 S DDBDMSG=$G(TXT)
 Q
STR(X) ;  Remove windows
 N I,Y
 I $L(X,"|")'>2 Q X
 I X["|WRAP|"!(X["| NO WRAP|")!(X["|NOWRAP|") S Y="" F I=1:1:$L(X,"|") S:(I#2) Y=Y_$P(X,"|",I)
 Q $S(X'["|":X,1:$G(Y))
