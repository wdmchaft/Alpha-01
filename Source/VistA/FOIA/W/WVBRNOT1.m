WVBRNOT1 ;HCIOFO/FT,JR IHS/ANMC/MWR - BROWSE NOTIFICATIONS; ;7/30/98  11:05
 ;;1.0;WOMEN'S HEALTH;;Sep 30, 1998
 ;;* MICHAEL REMILLARD, DDS * ALASKA NATIVE MEDICAL CENTER *
 ;;  DISPLAY CODE FOR BROWSING NOTIFICATIONS.  CALLED BY WVBRNOT.
 ;
DISPLAY ;EP
 ;---> WVCONF=DISPLAY "CONFIDENTIAL PT INFO" BANNER.
 ;---> WVTITLE=TITLE AT TOP OF DISPLAY HEADER.
 ;---> WVSUBH=CODE TO EXECUTE FOR SUBHEADER (COLUMN TITLES).
 ;---> WVCODE=CODE TO EXECUTE AS 3RD PIECE OF DIR(0) (AFTER DIR READ).
 ;---> WVCRT=1 IF OUTPUT IS TO SCREEN (ALLOWS SELECTIONS TO EDIT).
 ;---> WVTAB=6 IF OUTPUT IS TO SCREEN, =3 IF OUTPUT IS TO PRINTER.
 ;---> WVPRMT(1,Q)=PROMPTS FOR DIR.
 ;---> WVD=0, TELLS DIRPRMT^WVUTL3 TO DISPLAY SELECTION NUMBERS.
 ;
 U IO
 S WVCONF=1
 S WVTITLE1=$S(WVC=1:"DATE",WVC=2:"PATIENT",WVC=3:"PRIORITY",1:"?")
 S WVTITLE="* * *  NOTIFICATIONS LISTED BY "_WVTITLE1_"  * * *"
 D CENTERT^WVUTL5(.WVTITLE)
 S WVSUBH="SUBHEAD^WVBRNOT1"
 S WVCODE="D EDIT^WVBRNOT1,SORT^WVBRNOT,COPYGBL^WVBRNOT"
 S (WVD,WVPOP,N,Z)=0
 D TOPHEAD^WVUTL7
 S WVTAB=$S(WVCRT:6,1:3)
 ;
NOMATCH ;EP
 ;---> QUIT IF NO RECORDS MATCH.
 I '$D(^TMP("WV",$J,3)) D  Q
 .D HEADER1^WVUTL7
 .K WVPRMT,WVPRMT1,WVPRMTQ,DIR
 .W !!?5,"No records match the selected criteria.",!
 .I WVCRT&('$D(IO("S"))) D DIRZ^WVUTL3 W @IOF
 .D ^%ZISC S WVPOP=1
 ;
DISPLAY1 ;EP
 ;---> IF A NOTIFICATION IS EDITED ON THE LAST PAGE, GOTO HERE
 ;---> FROM LINELABEL "END" BELOW.
 D HEADER1^WVUTL7
 F  S N=$O(^TMP("WV",$J,4,N)) Q:'N!(WVPOP)  D
 .I $Y+6>IOSL D:WVCRT DIRZ^WVUTL3 Q:WVPOP  D
 ..S WVPAGE=WVPAGE+1
 ..D HEADER1^WVUTL7 S Z=0
 .S Y=^TMP("WV",$J,4,N),M=N
 .W !
 .;---> DON'T WRITE SSN# AND NAME IF IT MATCHES THE PREVIOUS RECORD.
 .;---> DON'T WRITE BROWSE SELECTION#'S IF IO IS NOT A CRT (BRCRT).
 .I $P(Y,U)'=Z D
 ..W ! W:WVCRT $J(N,3),")"                      ;BROWSE SELECTION#
 ..W ?WVTAB,$P(Y,U)                             ;SSN#
 ..W ?WVTAB+10,$E($P(Y,U,2),1,22)," "           ;NAME
 ..W $$REPEAT^XLFSTR(".",22-$L($P(Y,U,2)))               ;CONNECTING DOTS
 ..W:'WVCRT "..."                               ;ADD DOTS IF NOT A CRT
 .I $P(Y,U)=Z D                                 ;IF NEW SSN#...
 ..W:WVCRT $J(N,3),")"                          ;BROWSE SELECTION#
 ..W ?WVTAB,". . . .   . . . . . . . . . . . . ." ;CONNECTING DOTS
 .S Z=$P(Y,U)                                   ;STORE AS PREVIOUS CHRT#
 .;
 .W ?40,$$SLDT2^WVUTL5($P(Y,U,3))               ;DATE OF NOTIFICATION
 .W ?51,$P(Y,U,4)                               ;ACCESSION#
 .;W ?64,$E($P(Y,U,5),1,6)
 .W ?65,$E($P(Y,U,5))                           ;STATUS
 .S X=$P(Y,U,6)                                 ;PRIORITY
 .W ?70,$S(X=1:"*",1:" ")
 .W $E($P($P(^DD(790.404,.02,0),X_":",2),";"),1,8) K X
 I $D(^TMP("WV",$J,3)) I WVCRT&('$D(IO("S"))) D:'WVPOP DIRZ^WVUTL3 W @IOF
END ;EP
 D ^%ZISC
 Q
 ;
SUBHEAD ;EP
 ;---> SUB HEADER FOR NOTIFICATION BROWSE OUTPUT.
 W !?WVTAB,$$PNLB^WVUTL5()
 W ?WVTAB+12,"PATIENT",?41,"DATE",?51,"ACC#"
 W ?63,"STATUS",?71,"PRIORITY",!
 W $$REPEAT^XLFSTR("-",80)
 Q
 ;
EDIT ;EP
 ;---> FROM BROWSE, POP IN TO EDIT A SINGLE NOTIFICATION.
 D SETVARS^WVUTL5
 S X=+X,DA=$P(^TMP("WV",$J,4,X),U,7)
 S WVNN=X N X
 D EDIT2^WVNOTIF(DA)
 ;---> BACK UP 5 RECORDS AFTER EDIT.
 S N=$S(WVNN<6:1,1:WVNN-5),Z=0 K WVNN
 Q