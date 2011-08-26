PSIVPRO ;BIR/PR,MLM-PROFILE AN ORDER ;01 OCT 96 / 9:48 AM
 ;;5.0;INPATIENT MEDICATIONS ;**38,58,85,110,181,263**;16 DEC 97;Build 51
 ;
 ; Reference to ^PS(55 is supported by DBIA 2191
 ;        
 ;Needs DFN and ^TMP("PSIV",$J) array
 S PSJLN=1,PSIVX2=0
 S PSIVST=$O(^TMP("PSIV",$J,0)),X="",(PSIVON,PS)=0 D REACT I PSIVST]"" F PSIVX1=1:1 D PSIVST Q:'PSIVON  D PR
 S ^TMP("PSJPRO",$J,0)=PSIVX2,VALMCNT=PSJLN-1
 I $G(PSIVBR)="D ^PSIVOPT" S VALM("TITLE")="IV Order Entry"
 E  S VALM("TITLE")="IV Profile"
QUIT ; Kill and exit.
 S ON=X K ADM,AL,DRG,GMRA,GMRAL,PSIVST,PSIVX1,PSIVX2,Y,NAD,N0,X3,X4,X5
 Q
 ;
PSIVST ;
 S PSIVON=$O(^TMP("PSIV",$J,PSIVST,PSIVON)) I 'PSIVON S PSIVST=$O(^TMP("PSIV",$J,PSIVST_"Z")) I PSIVST]"" S PSIVON=$O(^TMP("PSIV",$J,PSIVST,0)) D HDL
 Q
REACT ;
 S PSJL="#   Additive",PSJL=$$SETSTR^VALM1("Last fill",PSJL,32,9)
 S PSJL=$$SETSTR^VALM1("Type",PSJL,49,4),PSJL=$$SETSTR^VALM1(" Start   Stop  Stat",PSJL,54,19)
 S PSJL=$$SETSTR^VALM1("Renew",PSJL,74,5)
 D SETTMP^PSJLMPRI
HDL ; Display type heading.
 S PSJL=""
 D ACL:PSIVST="A",DPL:PSIVST="RD",POL:PSIVST="P",POCL:PSIVST="PD",NVL:PSIVST="N",NVCL:PSIVST="ND",NOL:PSIVST="X",NOC:PSIVST="" S X=""
 S PSJL=$E(PSJL,1,79) D SETTMP^PSJLMPRI
 Q
 ;
PR ; Get & display order.
 S (ON,ON55)=9999999999-($S(PSIVST["P":$E(PSIVON,2,11),PSIVST["N":$E(PSIVON,2,11),1:PSIVON))_$S(PSIVST["P":"P",PSIVST["N":"P",1:"V") D @$S(PSIVST["P":"GT531^PSIVORFA(DFN,ON)",PSIVST["N":"GT531^PSIVORFA(DFN,ON)",1:"GT55^PSIVORFB")
 S X="",PS=PSIVX1 K ^TMP("PSIV",$J,PSIVST,PSIVON) S ^TMP("PSIV",$J,PSIVST_"B",PSIVX1)=$S(PSIVST["P":$E(PSIVON,2,11),PSIVST["N":$E(PSIVON,2,11),1:PSIVON)_$S(PSIVST["P":"P",PSIVST["N":"P",1:"V")
 I PSIVST="RD" D ENPL Q
 I PSIVST["D" N PSJO,PSIVX3 S PSIVX3=PSIVX1,PSJO=0 F  S PSJO=$O(^PS(53.1,"ACX",PSJCOM,PSJO)) Q:'PSJO  S ON=PSJO_"P" D GT531^PSIVORFA(DFN,ON),ENPL S PSIVX1=""
 I PSIVST["D" S PSIVX1=PSIVX3 Q
 ;
ENPL ;
 NEW MARX,DRUGNAME,X,XX
 S PSJL=$J(PSIVX1,4) I ON["P",(P("OT")'="F"),P(4)'="H" D  Q
 . I $D(VALMEVL) D
 .. N PSJFLAG
 .. S PSJFLAG=$P($S(ON["V":$G(^PS(55,DFN,"IV",+ON,.2)),1:$G(^PS(53.1,+ON,.2))),U,7)
 .. I PSJFLAG D CNTRL^VALM10(PSJLN,1,4,IORVON,IORVOFF,0)
 . D DRGDISP^PSJLMUT1(DFN,ON,34,59,.DRUGNAME,0)
 . NEW X F X=0:0 S X=$O(DRUGNAME(X)) Q:'X  S:X>1 PSJL="" S PSJL=$$SETSTR^VALM1(DRUGNAME(X),PSJL,$S(X=1:6,1:7),$S(X=1:34,1:65)) D:X=1 V D SETTMP^PSJLMPRI
 S X=$J(PSIVX1,4)_$S(P("PRY")="D":" d",1:"  ")
 I ON["V" S XX=$G(^PS(55,DFN,"IV",+ON,4)) D
 . I +PSJSYSU=1,'+XX S X=X_"->"
 . I +PSJSYSU=3,'+$P(XX,U,4) S X=X_"->"
 S PSJL=X
 I $D(VALMEVL) D
 . N PSJFLAG
 . S PSJFLAG=$P($S(ON["V":$G(^PS(55,DFN,"IV",+ON,.2)),1:$G(^PS(53.1,+ON,.2))),U,7)
 . I PSJFLAG D CNTRL^VALM10(PSJLN,1,4,IORVON,IORVOFF,0)
 D AD,SOL
 Q
SOL ;
 NEW NAME,PSJNOAD,L ;S PSJNOAD=0,L=34
 S NAD=0 F  S NAD=$O(DRG("SOL",NAD)) Q:'NAD  D
 . K NAME S L=34,PSJNOAD=0
 . I '$D(DRG("AD",1)),NAD=1 S PSJNOAD=1,L=27
 . S:NAD=1 PSJL=$$SETSTR^VALM1("in",PSJL,6,11)
 . D NAME^PSIVUTL(DRG("SOL",NAD),L,.NAME,0)
 . F X=0:0 S X=$O(NAME(X)) Q:'X  S:(NAD>1!(X>1)) PSJL="" S PSJL=$$SETSTR^VALM1(NAME(X),PSJL,9,34) D:X=1&PSJNOAD V D SETTMP^PSJLMPRI
 . S PSJL=""
 Q
AD ;
 NEW NAME
 S NAD=0 F  S NAD=$O(DRG("AD",NAD)) Q:'NAD  D
 . K NAME
 . ;D NAME^PSIVUTL(DRG("AD",NAD),30,.NAME,1)
 . D NAME^PSIVUTL(DRG("AD",NAD),27,.NAME,1)
 . F X=0:0 S X=$O(NAME(X)) Q:'X  S:(NAD>1!(X>1)) PSJL="" S PSJL=$$SETSTR^VALM1(NAME(X),PSJL,6,34) D:(NAD=1&(X=1)) V D SETTMP^PSJLMPRI
 . S PSJL=""
 Q
 ;
V S Y=$S(ON["V":$P($G(^PS(55,DFN,"IV",+ON,9)),U),1:"")
 I +Y>0 X ^DD("DD") S Y=$P(Y,",")_" "_$P($P(Y,"@",2),":",1,2)
 E  S Y="**   N/P  **"
 S PSJL=$$SETSTR^VALM1(Y,PSJL,33,12)
 S PSJL=$$SETSTR^VALM1(" #"_$S(ON["V":+$P($G(^PS(55,DFN,"IV",+ON,9)),U,2),1:0),PSJL,46,3)
 S:PSIVX1]"" PSIVX2=PSIVX2+1
 D REST
 Q
ACL ;
 F X3=1:1:71 S PSJL=PSJL_"-" S:X3=34 PSJL=PSJL_" A c t i v e "
 Q
NVL ;
 F X3=1:1:71 S PSJL=PSJL_"-" S:X3=34 PSJL=PSJL_" N o n - V e r i f i e d "
 Q
NVCL ;
 F X3=1:1:71 S PSJL=PSJL_"-" S:X3=34 PSJL=PSJL_" N o n - V e r i f i e d  C o m p l e x "
 Q
POL ;
 F X3=1:1:71 S PSJL=PSJL_"-" S:X3=34 PSJL=PSJL_" P e n d i n g "
 Q
POCL ;
 F X3=1:1:66 S PSJL=PSJL_"-" S:X3=34 PSJL=PSJL_" P e n d i n g  C o m p l e x "
 Q
NOL ;
 F X3=1:1:66 S PSJL=PSJL_"-" S:X3=34 PSJL=PSJL_" N o t   A c t i v e "
 Q
DPL ;Recently dc/expired header
 S PSJDCEXP=$$RECDCEXP^PSJP()
 F X3=1:1:71 S PSJL=PSJL_"-" S:X3=15 PSJL=PSJL_"Recently Discontinued/Expired (Last "_+$G(PSJDCEXP)_" hours)"
 Q
NOC ;
 F X3=1:1:66 S PSJL=PSJL_"-" S:X3=34 PSJL=PSJL_" No current IV information "
 ;
 S PSJL=""
 Q
REST ;
 S PSJL=$$SETSTR^VALM1(P(4),PSJL,52,1)
 S PSJL=$$SETSTR^VALM1($E($$ENDTC^PSGMI(P(2)),1,5),PSJL,55,5)
 S PSJL=$$SETSTR^VALM1($E($$ENDTC^PSGMI(P(3)),1,5),PSJL,62,5)
 S PSJL=$$SETSTR^VALM1($S(P(17)="R"&(ON'["V"):"R/I",$G(P(25))]"":P(25),1:P(17)),PSJL,69,2)
 S PSJL=$$SETSTR^VALM1($S(ON["P":P("PRY"),1:""),PSJL,71,1)
 N PSJLRN S PSJLRN=$$LASTREN^PSJLMPRI(DFN,ON55) I PSJLRN S PSJLRN=$E($$ENDTC^PSGMI(PSJLRN),1,5) S PSJL=$$SETSTR^VALM1(PSJLRN,PSJL,74,5)
 Q
XCHK ;
 I $E(X)="?" W !!?2,"Select order",$E("s",PS'=1)," (1" W:PS>1 "-",PS W ")."
 I $E(X)="?" W:$S($O(^TMP("PSIV",$J,PSIVST,ON)):1,1:$O(^TMP("PSIV",$J,PSIVST))]"") "  Press RETURN to view more orders, or enter '^' to abort",!,"the profile, or 'A' to view Allergies." D:$E(X,1,2)="??" H2^PSGON K X Q
 S PSGLMT=PS D ^PSGON Q
 ;
PSPD S Y=$S(PSIVST'="P":$P($G(^PS(55,DFN,"IV",+ON,9)),U),1:"")
 X ^DD("DD") S:Y="" PSJL=$$SETSTR^VALM1("**   N/P  **",PSJL,36,12)
 S:Y'="" PSJL=$$SETSTR^VALM1($P(Y,","),PSJL,36,7),PSJL=$$SETSTR^VALM1($P($P(Y,"@",2),":",1,2),PSJL,43,45)
 S PSJL=PSJL_" #"_$S(Y="":0,1:$P(^PS(55,DFN,"IV",+ON,9),U,2))
 D REST
 Q
