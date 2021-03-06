YSKFASI3 ;16IT/PTC - SUBSTANCE ABUSE ;6/22/01  14:21
 ;;5.01;MENTAL HEALTH;**73**;Dec 30, 1994
 ;
 ;Reference to ^DIC(40.7 supported by DBIA #1024
 ;Reference to ^DIC(42.4 supported by DBIA #557
 ;Reference to ^DPT( supported by DBIA #10035
 ;
 ;TOTALS
 S (NEW,ASI,NOASI,MAYBE,G12)=0 ;ASF 6/20/01
 S (CKCTT,YSKFDFN)=0 F  S YSKFDFN=$O(^TMP("XNNEW",$J,YSKFDFN)) Q:YSKFDFN'>0  I ($G(^UTILITY($J,"OPT",YSKFDFN))>2)!($D(^TMP("24STAY",$J,YSKFDFN))) S CKCTT=CKCTT+1 D
 .I '$D(^TMP("X90",$J,YSKFDFN)) S NEW=NEW+1 S YSKFDT=$O(^TMP("XNNEW",$J,YSKFDFN,4001231),-1) D
 ..I $D(^TMP("SHORT",$J,YSKFDFN))&('$D(^TMP("XN",$J,"ASI",YSKFDFN))) S MAYBE=MAYBE+1 D ALPHA1
 ..I $D(^TMP("XN",$J,"ASI",YSKFDFN))&($P($G(^TMP("XN",$J,"ASI",YSKFDFN)),U,4)="N") S ASI=ASI+1,FLGASI=1,TYPE=+^TMP("XN",$J,"ASI",YSKFDFN) D ALPHA ;G12 CHECK ASF 6/20/01
 ..I $D(^TMP("XN",$J,"ASI",YSKFDFN))&($P($G(^TMP("XN",$J,"ASI",YSKFDFN)),U,4)'="N") S G12=G12+1,FLGASI=1,TYPE=+^TMP("XN",$J,"ASI",YSKFDFN) D ALPHA ;G12 CHECK ASF 6/20/01
 ..I '$D(^TMP("XN",$J,"ASI",YSKFDFN))&('$D(^TMP("SHORT",$J,YSKFDFN))) S NOASI=NOASI+1,FLGASI=0 S YSKFDT1=$O(^TMP("XNNEW",$J,YSKFDFN,"A"),-1) S TYPE=+^TMP("XNNEW",$J,YSKFDFN,YSKFDT) S SAVEDT=YSKFDT,YSKFDT=YSKFDT1 D ALPHA S YSKFDT=SAVEDT K SAVEDT
 ;
LIST ; patient w/o ASI
 S YSKFJCNT=0
 S YSKFJCNT=YSKFJCNT+1,^TMP("XM",$J,YSKFJCNT)=" NEW PATIENTS WITHOUT ASI  (date is stay or 3rd visit during date range)"
 S YSKFJCNT=YSKFJCNT+1,^TMP("XM",$J,YSKFJCNT)="==============================================================================="
 S NAME="" F  S NAME=$O(^TMP("RPT",$J,NAME)) Q:NAME=""  S YSKFDFN=0 F  S YSKFDFN=$O(^TMP("RPT",$J,NAME,YSKFDFN)) Q:YSKFDFN'>0  D
 .S NODE=^TMP("RPT",$J,NAME,YSKFDFN) F I=1:1:6 S P(I)=$P(NODE,U,I)
 .S YSKFJCNT=YSKFJCNT+1,^TMP("XM",$J,YSKFJCNT)="   "_P(1)_P(2)_"  "_P(4)_" "_P(6)
 .S YSKFJCNT=YSKFJCNT+1,^TMP("XM",$J,YSKFJCNT)="       "_P(3)_"   "_P(5)
 S YSKFJCNT=YSKFJCNT+1,^TMP("XM",$J,YSKFJCNT)=""
 S YSKFJCNT=YSKFJCNT+1,^TMP("XM",$J,YSKFJCNT)=" NEW PATIENTS WITHOUT ASI BUT 14TH DAY NOT REACHED"
 S YSKFJCNT=YSKFJCNT+1,^TMP("XM",$J,YSKFJCNT)="==============================================================================="
 I $D(^TMP("MAYBE",$J)) S NAME="" F  S NAME=$O(^TMP("MAYBE",$J,NAME)) Q:NAME=""  S YSKFDFN=0 F  S YSKFDFN=$O(^TMP("MAYBE",$J,NAME,YSKFDFN)) Q:YSKFDFN=""  D
 .S NODE=^TMP("MAYBE",$J,NAME,YSKFDFN) F I=1:1:5 S P(I)=$P(NODE,U,I)
 .S YSKFJCNT=YSKFJCNT+1,^TMP("XM",$J,YSKFJCNT)="   "_P(1)_P(2)_"   "_P(4)
 .S YSKFJCNT=YSKFJCNT+1,^TMP("XM",$J,YSKFJCNT)="      "_P(3)_"    "_P(5)
 S YSKFJCNT=YSKFJCNT+1,^TMP("XM",$J,YSKFJCNT)=""
 S YSKFJCNT=YSKFJCNT+1,^TMP("XM",$J,YSKFJCNT)=" NEW PATIENTS WITH ASI (date is ASI interview date)"
 S YSKFJCNT=YSKFJCNT+1,^TMP("XM",$J,YSKFJCNT)="Sorted by Followup Months"
 S YSKFJCNT=YSKFJCNT+1,^TMP("XM",$J,YSKFJCNT)="==============================================================================="
 S NXT=0 F  S NXT=$O(^TMP("RPT1",$J,NXT)) Q:NXT'>0  S NAME="" F  S NAME=$O(^TMP("RPT1",$J,NXT,NAME)) Q:NAME=""  S YSKFDFN=0 F  S YSKFDFN=$O(^TMP("RPT1",$J,NXT,NAME,YSKFDFN)) Q:YSKFDFN'>0  D
 .S NODE=^TMP("RPT1",$J,NXT,NAME,YSKFDFN) F I=1:1:7 S P(I)=$P(NODE,U,I)
 .S XNXT=$$DATE(NXT)
 .S P(7)=$$DATE(P(7))
 .S YSKFJCNT=YSKFJCNT+1,^TMP("XM",$J,YSKFJCNT)=XNXT_"-"_P(7)_"  "_P(1)_"   "_P(2)_"  "_P(4)_"  "_P(6)
 .S YSKFJCNT=YSKFJCNT+1,^TMP("XM",$J,YSKFJCNT)="          "_P(3)_"   "_P(5)
 .S ^TMP("FOLLUP",$J,YSKFDFN)=^TMP("FOLLUP",$J,YSKFDFN)_U_XNXT_U_P(7)
 S YSKFJCNT=YSKFJCNT+1,^TMP("XM",$J,YSKFJCNT)=""
 D ^YSKFASIF
 D ^YSKFASIP
 Q
ALPHA ;
 S NAME=$P(^DPT(YSKFDFN,0),U,1),SSN=$P(^(0),U,9)
 S PRTNAME=NAME S YSKFL=$L(PRTNAME),YSKFLM=25-YSKFL F YSKFLCNT=1:1:YSKFLM S PRTNAME=PRTNAME_" "
 ;I +^TMP("XNNEW",$J,YSKFDFN,YSKFDT)=1 D
 I TYPE=1 D
 . I FLGASI=0 D
 ..S WARD=$P(^TMP("XNNEW",$J,YSKFDFN,YSKFDT),U,2) I WARD]"" S WARD=$P(^DIC(42.4,WARD,0),U)
 ..S ADMIT=$E(YSKFDT,4,5)_"/"_$E(YSKFDT,6,7)_"/"_$E(YSKFDT,2,3)
 ..S SPECIAL=$P($G(^TMP("XN",$J,"ASI",YSKFDFN)),"^",4)
 ..I SPECIAL="N" S SPECIAL=""
 ..S ^TMP("RPT",$J,NAME,YSKFDFN)=PRTNAME_U_SSN_U_WARD_U_ADMIT_U_U_SPECIAL
 .I FLGASI=1 S MON=$P(^TMP("XN",$J,"ASI",YSKFDFN),"^",5) D
 ..I MON<6 S NXT=MON+5,NXT2=MON+7
 ..I MON=6 S NXT=MON+5,NXT2=1
 ..I MON=7 S NXT=MON+5,NXT2=2
 ..I MON=8 S NXT=1,NXT2=3
 ..I MON=9 S NXT=2,NXT2=4
 ..I MON=10 S NXT=3,NXT2=5
 ..I MON=11 S NXT=4,NXT2=6
 ..I MON=12 S NXT=5,NXT2=7
 ..S WARD=$P(^TMP("XN",$J,"ASI",YSKFDFN),U,8) I WARD]"" S WARD=$P(^DIC(42.4,WARD,0),U)
 ..S ADMIT=$E(YSKFDT,4,5)_"/"_$E(YSKFDT,6,7)_"/"_$E(YSKFDT,2,3)
 ..S SPECIAL=$P($G(^TMP("XN",$J,"ASI",YSKFDFN)),"^",4)
 ..I SPECIAL="N" S SPECIAL=""
 ..S ASIDATE=$P(^TMP("XN",$J,"ASI",YSKFDFN),"^",6)
 ..S ASIDATE1=$P(^TMP("XN",$J,"ASI",YSKFDFN),"^",7)
 ..S ^TMP("RPT1",$J,NXT,NAME,YSKFDFN)=PRTNAME_U_SSN_U_WARD_U_ASIDATE_U_U_SPECIAL_U_NXT2
 ..S ^TMP("FOLLUP",$J,YSKFDFN)=NXT_U_NXT2_U_ASIDATE1
 ;I +^TMP("XNNEW",$J,YSKFDFN,YSKFDT)=2 D
 I TYPE=2 D
 .I FLGASI=0 D
 ..S CLIN=$P(^TMP("XNNEW",$J,YSKFDFN,YSKFDT),U,2) I CLIN]"" S CLIN=$P(^DIC(40.7,CLIN,0),U)
 ..S PRVDER=$P(^TMP("XNNEW",$J,YSKFDFN,YSKFDT),U,3)
 ..S LSTVST=$S($D(^UTILITY($J,"TRDOP",YSKFDFN)):+^UTILITY($J,"TRDOP",YSKFDFN),1:YSKFDT)
 ..S LSTVSTPR=$E(LSTVST,4,5)_"/"_$E(LSTVST,6,7)_"/"_$E(LSTVST,2,3)
 .I FLGASI=1 D
 ..S CLIN=$P(^TMP("XN",$J,"ASI",YSKFDFN),U,8) I CLIN]"" S CLIN=$P(^DIC(40.7,CLIN,0),U)
 ..S PRVDER=$P(^TMP("XNNEW",$J,YSKFDFN,YSKFDT),U,3)
 ..S ENCDT=$E(YSKFDT,4,5)_"/"_$E(YSKFDT,6,7)_"/"_$E(YSKFDT,2,3)
 ..S ASIDATE=$P(^TMP("XN",$J,"ASI",YSKFDFN),"^",6)
 ..S ASIDATE1=$P(^TMP("XN",$J,"ASI",YSKFDFN),"^",7)
 .S SPECIAL=$P($G(^TMP("XN",$J,"ASI",YSKFDFN)),"^",4)
 .I SPECIAL="N" S SPECIAL=""
 .I FLGASI=0 S ^TMP("RPT",$J,NAME,YSKFDFN)=PRTNAME_U_SSN_U_CLIN_U_LSTVSTPR_U_PRVDER_U_SPECIAL
 .I FLGASI=1 S MON=$P(^TMP("XN",$J,"ASI",YSKFDFN),"^",5) D
 ..I MON<6 S NXT=MON+5,NXT2=MON+7
 ..I MON=6 S NXT=MON+5,NXT2=1
 ..I MON=7 S NXT=MON+5,NXT2=2
 ..I MON=8 S NXT=1,NXT2=3
 ..I MON=9 S NXT=2,NXT2=4
 ..I MON=10 S NXT=3,NXT2=5
 ..I MON=11 S NXT=4,NXT2=6
 ..I MON=12 S NXT=5,NXT2=7
 ..S ASIDATE=$P(^TMP("XN",$J,"ASI",YSKFDFN),"^",6)
 ..S ASIDATE1=$P(^TMP("XN",$J,"ASI",YSKFDFN),"^",7)
 ..S ^TMP("RPT1",$J,NXT,NAME,YSKFDFN)=PRTNAME_U_SSN_U_CLIN_U_ASIDATE_U_PRVDER_U_SPECIAL_U_NXT2
 ..S ^TMP("FOLLUP",$J,YSKFDFN)=NXT_U_NXT2_U_ASIDATE1
 Q
DATE(X) ;
 S X=$P("JAN^FEB^MAR^APR^MAY^JUN^JUL^AUG^SEP^OCT^NOV^DEC","^",+X)
 Q X
ALPHA1 ;
 S NAME=$P(^DPT(YSKFDFN,0),U,1),SSN=$P(^(0),U,9)
 S PRTNAME=NAME S YSKFL=$L(PRTNAME),YSKFLM=25-YSKFL F YSKFLCNT=1:1:YSKFLM S PRTNAME=PRTNAME_" "
 I +^TMP("XNNEW",$J,YSKFDFN,YSKFDT)=1 D
 .S WARD=$P(^TMP("XNNEW",$J,YSKFDFN,YSKFDT),U,2) I WARD]"" S WARD=$P(^DIC(42.4,WARD,0),U)
 .S ADMIT=$E(YSKFDT,4,5)_"/"_$E(YSKFDT,6,7)_"/"_$E(YSKFDT,2,3)
 .S ^TMP("MAYBE",$J,NAME,YSKFDFN)=PRTNAME_U_SSN_U_WARD_U_ADMIT
 I +^TMP("XNNEW",$J,YSKFDFN,YSKFDT)=2 D
 .S CLIN=$P(^TMP("XNNEW",$J,YSKFDFN,YSKFDT),U,2) I CLIN]"" S CLIN=$P(^DIC(40.7,CLIN,0),U)
 .S PRVDER=$P(^TMP("XNNEW",$J,YSKFDFN,YSKFDT),U,3)
 .S ENCDT=$E(YSKFDT,4,5)_"/"_$E(YSKFDT,6,7)_"/"_$E(YSKFDT,2,3)
 .S TRDVST=$P(^TMP("SHORT",$J,YSKFDFN),U,5)
 .S TRDVSTD=$E(TRDVST,4,5)_"/"_$E(TRDVST,6,7)_"/"_$E(TRDVST,2,3)
 .S ^TMP("MAYBE",$J,NAME,YSKFDFN)=PRTNAME_U_SSN_U_CLIN_U_TRDVSTD_U_PRVDER
 Q
